# data.table defined last(x) with no arguments, just for last. If you need the last 10 then use tail(x,10).
# for xts class objects it will dispatch to xts::last
# reworked to avoid loading xts namespace (#3857) then again to fix dispatching of xts class (#4053)
last = function(x, n=1L, ...) {
  verbose = isTRUE(getOption("datatable.verbose", FALSE))
  if (!inherits(x, "xts")) {
    if (nargs()>1L) {
      if ("package:xts" %chin% search()) {
        if (verbose)
          catf("%s: using %s: %s\n", "last", "xts::last", "!is.xts(x) & nargs>1 & 'package:xts'%in%search()")
        xts::last(x, n=n, ...)
      } else {
        # nocov start
        if (verbose)
          catf("%s: using %s: %s\n", "last", "utils::tail", "!is.xts(x) & nargs>1 & !'package:xts'%in%search()")
        utils::tail(x, n=n, ...)
        # nocov end
      }
    } else {
      dx = dim(x)
      if (is.null(dx)) {
        if (verbose)
          catf("%s: using %s: %s\n", "last", "'x[[length(x)]]'", "!is.xts(x) & !nargs>1 & is.null(dim(x))")
        lx = length(x)
        if (!lx) x else x[[lx]]
      } else if (is.data.frame(x)) {
        if (verbose)
          catf("%s: using %s: %s\n", "last", "'x[nrow(x),]'", "!is.xts(x) & !nargs>1 & is.data.frame(x)")
        x[dx[1L], , drop=FALSE]
      } else {
        if (verbose)
          catf("%s: using %s: %s\n", "last", "utils::tail", "!is.xts(x) & !nargs>1 & !is.null(dim(x)) & !is.data.frame(x)")
        utils::tail(x, n=n, ...)
      }
    }
  } else {
    if (!requireNamespace("xts", quietly=TRUE))
      stopf("'xts' class passed to %s function but 'xts' is not available, you should have 'xts' installed already", "data.table::last") # nocov
    if (verbose)
      catf("%s: using %s: %s\n", "last", "xts::last", "is.xts(x)")
    xts::last(x, n=n, ...)
  }
}

first = function(x, n=1L, ...) {
  verbose = isTRUE(getOption("datatable.verbose", FALSE))
  if (!inherits(x, "xts")) {
    if (nargs()>1L) {
      if ("package:xts" %chin% search()) {
        if (verbose)
          catf("%s: using %s: %s\n", "first", "xts::first", "!is.xts(x) & nargs>1 & 'package:xts'%in%search()")
        xts::first(x, n=n, ...)
      } else {
        # nocov start
        if (verbose)
          catf("%s: using %s: %s\n", "first", "utils::head", "!is.xts(x) & nargs>1 & !'package:xts'%in%search()")
        utils::head(x, n=n, ...)
        # nocov end
      }
    } else {
      dx = dim(x)
      if (is.null(dx)) {
        if (verbose)
          catf("%s: using %s: %s\n", "first", "'x[[1L]]'", "!is.xts(x) & !nargs>1 & is.null(dim(x))")
        lx = length(x)
        if (!lx) x else x[[1L]]
      } else if (is.data.frame(x)) {
        if (verbose)
          catf("%s: using %s: %s\n", "first", "'x[1L,]'", "!is.xts(x) & !nargs>1 & is.data.frame(x)")
        if (!dx[1L]) x else x[1L, , drop=FALSE]
      } else {
        if (verbose)
          catf("%s: using %s: %s\n", "first", "utils::head", "!is.xts(x) & !nargs>1 & !is.null(dim(x)) & !is.data.frame(x)")
        utils::head(x, n=n, ...)
      }
    }
  } else {
    if (!requireNamespace("xts", quietly=TRUE))
      stopf("'xts' class passed to %s function but 'xts' is not available, you should have 'xts' installed already", "data.table::first") # nocov
    if (verbose)
      catf("%s: using %s: %s\n", "first", "xts::first", "is.xts(x)")
    xts::first(x, n=n, ...)
  }
}
