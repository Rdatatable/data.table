rollmean <- function(x, n, fill=NA) {
  ans = .Call(Crollmean, x, as.integer(n), fill, PACKAGE="data.table")
  ans
}
