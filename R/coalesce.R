coalesce = function(x, ..., .dots=NULL) {
  if (is.null(.dots) && missing(..1)) return(x)

  values = if (!is.null(.dots)) {
    if (!missing(..1)) stop("provide either '...' or '.dots' argument, not both")
    if (!is.list(.dots)) stop("argument '.dots' requires a list input")
    .dots
  } else list(...)

  .Call(Ccoalesce, x, values, FALSE)
}
setcoalesce = function(x, ..., .dots=NULL) {
  if (is.null(.dots) && missing(..1)) return(x)

  values = if (!is.null(.dots)) {
    if (!missing(..1)) stop("provide either '...' or '.dots' argument, not both")
    if (!is.list(.dots)) stop("argument '.dots' requires a list input")
    .dots
  } else list(...)

  invisible(.Call(Ccoalesce, x, values, TRUE))
}
which_eq = function(x, value, negate=FALSE) {
  .Call(Cwhich_eqR, x, value, negate)
}
