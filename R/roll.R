rollmean <- function(x, n, fill=NA, align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=is.list(n)) {
  align = match.arg(align)
  ans = .Call(Crollmean, x, as.integer(n), fill, align, na.rm, hasNA, adaptive)
  ans
}
