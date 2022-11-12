# globals to pass NOTE from R CMD check, see http://stackoverflow.com/questions/9439256
MB = NCOL = NROW = INDICES = NULL

type_size = function(DT) {
  # for speed and ram efficiency, a lower bound by not descending into character string lengths or list items
  # if a more accurate and higher estimate is needed then user can pass object.size or alternative to mb=
  # in case number of columns is very large (e.g. 1e6 columns) then we use a for() to avoid allocation of sapply()
  ans = 0L
  lookup = c("raw"=1L, "integer"=4L, "double"=8L, "complex"=16L)
  for (i in seq_along(DT)) {
    col = DT[[i]]
    tt = lookup[storage.mode(col)]
    if (is.na(tt)) tt = .Machine$sizeof.pointer
    tt = tt*nrow(DT)
    if (is.factor(col)) tt = tt + length(levels(col))*.Machine$sizeof.pointer
    ans = ans + tt
  }
  ans + ncol(DT)*.Machine$sizeof.pointer  # column name pointers
}

tables = function(mb=type_size, order.col="NAME", width=80,
                  env=parent.frame(), silent=FALSE, index=FALSE)
{
  # Prints name, size and colnames of all data.tables in the calling environment by default
  # include "hidden" objects (starting with .) via all.names=TRUE, but exclude ... specifically, #5197
  all_obj = grep("...", ls(envir=env, all.names=TRUE), invert=TRUE, fixed=TRUE, value=TRUE)
  if (order.col=="NAME") all_obj=sort(all_obj)  # neither ls() nor objects() had sorted arg in R 3.1.0
  is_DT = vapply_1b(mget(all_obj, envir=env), is.data.table)
  if (!any(is_DT)) {
    if (!silent) catf("No objects of class data.table exist in %s\n", if (identical(env, .GlobalEnv)) ".GlobalEnv" else format(env))
    return(invisible(data.table(NULL)))
  }
  if (isTRUE(mb)) mb=type_size  # can still use TRUE, although TRUE will now be the lower faster type_size method
  DT_names = all_obj[is_DT]
  info = rbindlist(lapply(DT_names, function(dt_n){
    DT = get(dt_n, envir=env)   # doesn't copy
    list(  # list() here was 9MB better than data.table() for tests.Rraw 1538, #5517
      dt_n,
      nrow(DT),
      ncol(DT),
      if (is.function(mb)) round(as.numeric(mb(DT))/1024^2) else NA,
      list(names(DT)),
      list(key(DT)),
      if (index) list(indices(DT)) else NA)
  }))
  setnames(info, c("NAME","NROW","NCOL","MB","COLS","KEY","INDICES"))
  if (!is.function(mb)) info[,MB:=NULL]
  if (!index)           info[,INDICES:=NULL]
  if (order.col != "NAME") {
    if (!order.col %chin% names(info)) stopf("order.col='%s' not a column name of info", order.col)
    info = info[base::order(info[[order.col]])]  # base::order to maintain locale ordering of table names
  }
  if (!silent) {
    # prettier printing on console
    pretty_format = function(x, width) {
      format(prettyNum(x, big.mark=","),
             width=width, justify="right")
    }
    tt = copy(info)
    tt[ , NROW := pretty_format(NROW, width=4L)]
    tt[ , NCOL := pretty_format(NCOL, width=4L)]
    if (is.function(mb)) tt[ , MB := pretty_format(MB, width=2L)]
    print(tt, class=FALSE, nrows=Inf)
    if (is.function(mb)) catf("Total: %sMB\n", prettyNum(sum(info$MB), big.mark=","))
  }
  invisible(info)
}

