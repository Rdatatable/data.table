coalesce = function(x, ...) {
  if (missing(..1)) {
    if (is.list(x)) {
      if (length(x)<=1L) return(x[[1L]])
      values = if (is.data.table(x)) x[,-1L] else x[-1L]
      x = x[[1L]]
    } else return(x)
  } else {
    if (!missing(..2)) values = list(...)
    else values = if (is.list(..1)) ..1 else list(..1)
  }
  .Call(Ccoalesce, x, values, FALSE)
}

setcoalesce = function(x, ...) {
  if (missing(..1)) {
    if (is.list(x)) {
      if (length(x)<=1L) return(x[[1L]])
      values = if (is.data.table(x)) x[,-1L] else x[-1L]
      x = x[[1L]]
    } else return(x)
  } else {
    if (!missing(..2)) values = list(...)
    else values = if (is.list(..1)) ..1 else list(..1)
  }
  invisible(.Call(Ccoalesce, x, values, TRUE))
}
