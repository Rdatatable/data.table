froll <- function(fun, x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=is.list(n)) {
  stopifnot(is.character(fun), length(fun)==1L, !is.na(fun))
  align = match.arg(align)
  # n type handling to be moved to C
  if (is.double(n)) n = as.integer(n)
  else if (is.list(n)) stopifnot(sapply(n, inherits, "integer"))
  ans = .Call(Crollfun, fun, x, n, fill, exact, align, na.rm, hasNA, adaptive)
  ans
}

frollmean <- function(x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=is.list(n)) {
  froll(fun="mean", x=x, n=n, fill=fill, exact=exact, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive)
}

frollsum <- function(x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=is.list(n)) {
  froll(fun="sum", x=x, n=n, fill=fill, exact=exact, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive)
}
