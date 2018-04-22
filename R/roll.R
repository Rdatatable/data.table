rollmean <- function(x, n, fill=NA, align=c("right", "center", "left"), adaptive=FALSE) {
  align =  match.arg(align)
  ans = .Call(Crollmean, x, as.integer(n), fill, align, adaptive)
  ans
}
