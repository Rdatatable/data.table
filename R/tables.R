# globals to pass NOTE from R CMD check, see http://stackoverflow.com/questions/9439256
MB = NCOL = NROW = NULL

tables = function(mb=TRUE, order.col="NAME", width=80,
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
  DT_names = all_obj[is_DT]
  info = rbindlist(lapply(DT_names, function(dt_n){
    DT = get(dt_n, envir=env)   # doesn't copy
    data.table(  # data.table excludes any NULL items (MB and INDICES optional) unlike list()
      NAME = dt_n,
      NROW = nrow(DT),
      NCOL = ncol(DT),
      MB = if (mb) round(as.numeric(object.size(DT))/1024^2), # object.size() is slow hence optional; TODO revisit
      COLS = list(names(DT)),
      KEY = list(key(DT)),
      INDICES = if (index) list(indices(DT)))
  }))
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
    if (mb) tt[ , MB := pretty_format(MB, width=2L)]
    print(tt, class=FALSE, nrows=Inf)
    if (mb) catf("Total: %sMB\n", prettyNum(sum(info$MB), big.mark=","))
  }
  invisible(info)
}

