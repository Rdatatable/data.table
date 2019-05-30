coalesce = function(x, ..., .dots=NULL, .inplace=FALSE) {
  if (is.null(.dots) && missing(..1)) return(x)

  values = if (!is.null(.dots)) {
    if (!missing(..1)) stop("provide either '...' or '.dots' argument, not both")
    if (!is.list(.dots)) stop("argument '.dots' requires a list input")
    .dots
  } else list(...)

  ans = .Call(Ccoalesce, x, values, .inplace)
  if (.inplace) invisible(ans) else ans
}
setcoalesce = function(x, ..., .dots=NULL) {
  invisible(coalesce(x, ..., .dots=.dots, .inplace=TRUE))
}
which_eq.double = function(x, value, negate=FALSE) {
  stopifnot(is.double(x), is.double(value))
  .Call(Cwhich_eq_doubleR, x, value, negate)
}
which_eq.character = function(x, value, negate=FALSE) {
  stopifnot(is.character(x), is.character(value))
  .Call(Cwhich_eq_charR, x, value, negate)
}
