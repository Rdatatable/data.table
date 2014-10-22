desc <- function(x, ...) {
  UseMethod ("desc")
}

desc.default <- function(x, ...) {
  stop ("Currently desc is only implemented for data.table / data.frame.\nPlease feel free to submit a feature request on https://github.com/Rdatatable/data.table/issues i")
}

desc.data.frame <- function(x, ...) {
  x.DT <- copy(x)
  setDT(x.DT)
  desc.data.table(x.DT)
}

desc.data.table <- function(x, tight.output=FALSE, sort.column=TRUE, quietly=FALSE) { 
## NOTE: depends on  as.data.table.matrix which returns strings instead of factors for character matrices
##
## TODO:  Add 'details=TRUE' argument, such as min, max, etc, depending on class

  if (!length(x))
    return(null.data.table())

  cl.list <- lapply(DT, class)
  #find max length of cl.l and pad the rest of the values with ""
  l <- sapply(cl.list, length)
  if (mean(l) != min(l))
    ans <- rbindlist(mapply(function(cl, p) as.data.table(t(c(cl, rep("", p)))), cl.list, max(l) - l, SIMPLIFY=FALSE))
  else
    ans <- as.data.table(do.call(rbind, cl.list))

  ## TODO:  Once characters are returned unofromly as characters, we can remove this check
  if (is.factor(ans[[1]]))
    ans <- ans[, lapply(.SD, as.character)]

  ## First column is called "class", subsequent are called "class2", "class3" etc
  setnames(ans, 1, "class")
  setnames(ans, gsub("V", "class", names(ans)))

  ## add in column names
  ans[, column := names(DT)]

  setclass(ans, "desc", append=TRUE)

  ## Avoid printing the output twice, if user executes print(desc(DT))
  if (missing(quietly) && grepl("^print(\\(|\\.)", tail(sys.calls(), 3)[1:2]))
    quietly <- TRUE

  ## This last portion, as well as the template definition of quietly is to allow 
  if (!missing(quietly) && isTRUE(quietly))
    return(invisible(ans))

  print.desc(ans, tight.output=tight.output, sort.column=sort.column)
  return(invisible(ans))
}

print.desc <- function(x, tight.output=FALSE, sort.column=TRUE, ...) {
## Some columns have more than one class. However, 
##  for the  purposes of this clean output, we show only the primary class.

  ## error check. The dots are included to avoid crashes when print(x, some.arg) is called
  ##  However, notify user, as it might be sign of unexpected method deployment 
  if (length(dots <- list(...)))
    warning("print.desc(x) does not use argument(s) ", paste("'", names(dots), "'", collapse=", "))

  ## Output will be sorted according to this manual ordering
  classOrder <- c("idcol", "character", "factor", "logical", "integer", "numeric"
                  , "Date", "POSIXct", "POSIXlt"
                  , "list", "data.table", "data.frame")

  if (!inherits(x, "desc"))
      x <- desc(x)
  else 
      x <- copy(x)

  ## Add in any missing classes
  classOrder <- unique(c(classOrder, x$class, use.names=FALSE) )
  x[, class.factor := factor(class, levels=classOrder)]

  if (sort.column)
    setkeyv(x, c("class", "column"))

  ## fmt for sprintf, check largest char size of class.factor
  space.class <- sprintf("%%s %%-%is %%1s  %%s", max(nchar(x$class)))  ## eg:  "%s %-12s %1s %s"  for:  "\n   class  :  columns..."
  ## combine the columns into a comma separated string, by class
  ans <- x[, list(columns.out = paste(column, collapse=", ")), keyby=class.factor]

  ## chop each line between 30~60 characters
  repl <- sprintf(space.class, "", "", "", "") ## thhe spae at the left of a new line for the same class
  repl <- paste0("\\1,\n", repl)
  ## TODO:  Take into account max(nchar(x$class) for the regex pattern
  ans[, columns.out := gsub("(.{35}.{0,30}?), ", repl, columns.out)]


  out <- ans[, sprintf(fmt=space.class, ifelse(tight.output, "", "\n"), class.factor, ":", columns.out)]
  cat(out, if (!tight.output) "", sep="\n")

  return(invisible(NULL))
}


if (FALSE)
{
  L <- 1:5
  DT <- data.table(ID = LETTERS[1L]
               ,   date = seq(Sys.Date(), length=max(L), by="day")
               ,   last.occurance = seq(Sys.Date(), length=max(L), by="-2 month")
               ,   value = rnorm(L)
               ,   distance = runif(L, 100, 1e5)
               ,   group = factor(letters[L])
               ,   days = as.integer(sample(30, L))
               ,   user = c("tammy", "tommmy", "billie", "zoe", "chloe")
               ,   storeid = paste0("store", L)
               ,   mileage = {set.seed(1); rnorm(L, 100, 1e5)}
               ,   mileage.scaled = scale({set.seed(1); rnorm(L, 100, 1e5)})
  )

  desc(DT)
  desc.data.table(DT, tight.output=TRUE)


  desc(DT, sort=FALSE)
  desc(DT, tight=TRUE)
  desc(DT, tight=TRUE, sort=FALSE)
  desc(DT, tight=TRUE, sort=FALSE, quietly=FALSE)
  desc(DT, tight=TRUE, sort=FALSE, quietly=TRUE)
  desc(DT, quietly=FALSE)
  desc(DT, quietly=TRUE)
  desc(DT, tight=TRUE, sort=FALSE, quietly=TRUE)


  ## Confirming that we do not output the ans twice. 
  ## (the calls to identity are simply to test sys.calls() is correctly used. 
  print(desc(DT))
  identity(print(desc(DT)))
  identity(print(desc(identity(DT))))

}

