froll <- function(fun, x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), partial=FALSE, na.rm=FALSE, hasNA=NA, adaptive=is.list(n), verbose=FALSE) {
  stopifnot(is.character(fun), length(fun)==1L, !is.na(fun))
  align = match.arg(align)
  # n type handling to be moved to C
  if (is.double(n)) n = as.integer(n)
  else if (is.list(n)) stopifnot(sapply(n, inherits, "integer"))
  # verbose int only during dev, later on logical
  verbose = as.integer(verbose)
  # this will probably need to stay here, unless there `missing` function in C
  if (partial && !missing(fill)) stop("Using 'partial' TRUE and passing 'fill' does not make sense. Argument 'fill' is meant to be used when rolling window is not yet in expected width. Argument 'partial' TRUE asks to write results even when rolling window is not yet in fully expected width.")
  ans = .Call(Crollfun, fun, x, n, fill, exact, align, partial, na.rm, hasNA, adaptive, verbose)
  ans
}

frollmean <- function(x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), partial=FALSE, na.rm=FALSE, hasNA=NA, adaptive=is.list(n), verbose=FALSE) {
  froll(fun="mean", x=x, n=n, fill=fill, exact=exact, align=align, partial=partial, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive, verbose=verbose)
}

frollsum <- function(x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), partial=FALSE, na.rm=FALSE, hasNA=NA, adaptive=is.list(n), verbose=FALSE) {
  froll(fun="sum", x=x, n=n, fill=fill, exact=exact, align=align, partial=partial, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive, verbose=verbose)
}
