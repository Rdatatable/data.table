rollmean <- function(x, n, fill=NA, align=c("right", "left", "center"), adaptive=is.list(n)) {
  align =  match.arg(align)
  ans = .Call(Crollmean, x, as.integer(n), fill, align, adaptive)
  ans
}
