coalesce = function(x, ..., .dots=NULL) {
  if (is.null(.dots) && missing(..1)) return(copy(x))

  values = if (!is.null(.dots)) {
    if (!missing(..1)) stop("provide either '...' or '.dots' argument, not both")
    if (!is.list(.dots)) stop("argument '.dots' requires a list input")
    .dots
  } else list(...)

  if (!length(values)) return(copy(x))

  .Call(Ccoalesce, x, values, FALSE)
}

setcoalesce = function(x, ..., .dots=NULL) {
  if (is.null(.dots) && missing(..1)) return(x)

  values = if (!is.null(.dots)) {
    if (!missing(..1)) stop("provide either '...' or '.dots' argument, not both")
    if (!is.list(.dots)) stop("argument '.dots' requires a list input")
    .dots
  } else list(...)

  if (!length(values)) return(x)

  invisible(.Call(Ccoalesce, x, values, TRUE))
}

