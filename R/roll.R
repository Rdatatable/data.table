frollmean <- function(x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=is.list(n)) {
  align = match.arg(align)
  # n type handling to be moved to C
  if (is.double(n)) n = as.integer(n)
  else if (is.list(n)) stopifnot(sapply(n, inherits, "integer"))
  ans = .Call(Crollmean, x, n, fill, exact, align, na.rm, hasNA, adaptive)
  ans
}
