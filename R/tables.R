# globals to pass NOTE from R CMD check, see http://stackoverflow.com/questions/9439256
MB = NCOL = NROW = NULL

tables <- function(mb=TRUE, order.col="NAME", width=80,
                   env=parent.frame(), silent=FALSE, index=FALSE)
{
  # Prints name, size and colnames of all data.tables in the calling environment by default
  all_obj = objects(envir=env, all.names=TRUE)
  is_DT = which(vapply_1b(all_obj, function(x) is.data.table(get(x, envir=env))))
  if (!length(is_DT)) {
    if (!silent) cat("No objects of class data.table exist in", if (identical(env,.GlobalEnv)) ".GlobalEnv" else format(env), "\n")
    return(invisible(data.table(NULL)))
  }
  DT_names = all_obj[is_DT]
  info = rbindlist(lapply(DT_names, function(dt_n){
    DT = get(dt_n, envir=env)   # doesn't copy
    info_i =
      data.table(NAME = dt_n,
                 NROW = nrow(DT),
                 NCOL = ncol(DT))
    if (mb) set(info_i, , "MB", round(as.numeric(object.size(DT))/1024^2))
    # mb is an option because object.size() appears to be slow. TO DO: revisit
    set(info_i, , "COLS", list(list(names(DT))))
    set(info_i, , "KEY", list(list(key(DT))))
    if (index) set(info_i, , "INDICES", list(list(indices(DT))))
    info_i
  }))
  if (!order.col %chin% names(info)) stop("order.col='",order.col,"' not a column name of info")
  info = info[base::order(info[[order.col]])]  # base::order to maintain locale ordering of table names
  if (!silent) {
    # prettier printing on console
    pretty_format = function(x, width) {
      format(prettyNum(x, big.mark=","),
             width=width, justify="right")
    }
    tt = copy(info)
    tt[ , NROW := pretty_format(NROW, width=4L)]
    tt[ , NCOL := pretty_format(NCOL, width=4L)]
    if (mb) tt[ , MB := pretty_format(MB, width=2L)]
    print(tt, class=FALSE, nrow=Inf)
    if (mb) cat("Total: ", prettyNum(sum(info$MB), big.mark=","), "MB\n", sep="")
  }
  invisible(info)
}

