# data.table defined last(x) with no arguments, just for last. If you need the last 10 then use tail(x,10).
# for xts class objects it will dispatch to xts::last
# reworked to avoid loading xts namespace (#3857) then again to fix dispatching of xts class (#4053)
last = function(x, n=1L, ...) {
  verbose = isTRUE(getOption("datatable.verbose", FALSE))
  if (!inherits(x, "xts")) {
    if (nargs()>1L) {
      if ("package:xts" %chin% search()) {
        if (verbose)
          cat("last: using xts::last: !is.xts(x) & nargs>1 & 'package:xts'%in%search()\n")
        xts::last(x, n=n, ...)
      } else {
        # nocov start
        if (verbose)
          cat("last: using utils::tail: !is.xts(x) & nargs>1 & !'package:xts'%in%search()\n")
        utils::tail(x, n=n, ...)
        # nocov end
      }
    } else {
      dx = dim(x)
      if (is.null(dx)) {
        if (verbose)
          cat("last: using 'x[[length(x)]]': !is.xts(x) & !nargs>1 & is.null(dim(x))\n")
        lx = length(x)
        if (!lx) x else x[[lx]]
      } else if (is.data.frame(x)) {
        if (verbose)
          cat("last: using 'x[nrow(x),]': !is.xts(x) & !nargs>1 & is.data.frame(x)\n")
        x[dx[1L], , drop=FALSE]
      } else {
        if (verbose)
          cat("last: using utils::tail: !is.xts(x) & !nargs>1 & !is.null(dim(x)) & !is.data.frame(x)\n")
        utils::tail(x, n=n, ...)
      }
    }
  } else {
    if (!requireNamespace("xts", quietly=TRUE))
      stop(gettextf("'xts' class passed to %s function but 'xts' is not available, you should have 'xts' installed already", "data.table::last", domain="R-data.table")) # nocov
    if (verbose)
      cat("last: using xts::last: is.xts(x)\n")
    xts::last(x, n=n, ...)
  }
}

first = function(x, n=1L, ...) {
  verbose = isTRUE(getOption("datatable.verbose", FALSE))
  if (!inherits(x, "xts")) {
    if (nargs()>1L) {
      if ("package:xts" %chin% search()) {
        if (verbose)
          cat("first: using xts::first: !is.xts(x) & nargs>1 & 'package:xts'%in%search()\n")
        xts::first(x, n=n, ...)
      } else {
        # nocov start
        if (verbose)
          cat("first: using utils::head: !is.xts(x) & nargs>1 & !'package:xts'%in%search()\n")
        utils::head(x, n=n, ...)
        # nocov end
      }
    } else {
      dx = dim(x)
      if (is.null(dx)) {
        if (verbose)
          cat("first: using 'x[[1L]]': !is.xts(x) & !nargs>1 & is.null(dim(x))\n")
        lx = length(x)
        if (!lx) x else x[[1L]]
      } else if (is.data.frame(x)) {
        if (verbose)
          cat("first: using 'x[1L,]': !is.xts(x) & !nargs>1 & is.data.frame(x)\n")
        if (!dx[1L]) x else x[1L, , drop=FALSE]
      } else {
        if (verbose)
          cat("first: using utils::head: !is.xts(x) & !nargs>1 & !is.null(dim(x)) & !is.data.frame(x)\n")
        utils::head(x, n=n, ...)
      }
    }
  } else {
    if (!requireNamespace("xts", quietly=TRUE))
      stop(gettextf("'xts' class passed to %s function but 'xts' is not available, you should have 'xts' installed already", "data.table::first", domain="R-data.table")) # nocov
    if (verbose)
      cat("first: using xts::first: is.xts(x)\n")
    xts::first(x, n=n, ...)
  }
}
