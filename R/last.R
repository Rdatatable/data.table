
# data.table defined last(x) with no arguments, just for last. If you need the last 10 then use tail(x,10).
# This last is implemented this way for compatibility with xts::last which is S3 generic taking 'n' and 'keep' arguments
# We'd like last() on vectors to be fast, so that's a direct x[NROW(x)] as it was in data.table, otherwise use xts's.
# If xts is loaded higher than data.table, xts::last will work but slower.
last = function(x, n=1L, ...) {
  if (nargs()==1L) {
    if (is.vector(x) || is.atomic(x)) {
      if (!length(x)) x else x[[length(x)]]
    } else if (is.data.frame(x)) {
      x[NROW(x),]
    }
  } else if ("package:xts" %chin% search()) {
    if (!requireNamespace("xts", quietly=TRUE))
      stop("internal error, package:xts is on search path but could not be loaded via requireNamespace") # nocov
    if (isTRUE(getOption("datatable.verbose", FALSE)))
      cat("last: using xts::last\n")
    xts::last(x, n=n, ...) # UseMethod("last") doesn't find xts's methods, not sure what I did wrong.
  } else {
    tail(x, n=n, ...) # nocov
  }
}

# first(), similar to last(), not sure why this wasn't exported in the first place...
first = function(x, n=1L, ...) {
  if (nargs()==1L) {
    if (is.vector(x) || is.atomic(x)) {
      if (!length(x)) x else x[[1L]]
    } else if (is.data.frame(x)) {
      if (!NROW(x)) x else x[1L,]
    }
  } else if ("package:xts" %chin% search()) {
    if (!requireNamespace("xts", quietly=TRUE))
      stop("internal error, package:xts is on search path but could not be loaded via requireNamespace") # nocov
    if (isTRUE(getOption("datatable.verbose", FALSE)))
      cat("first: using xts::first\n")
    xts::first(x, n=n, ...)
  } else {
    head(x, n=n, ...) # nocov
  }
}
