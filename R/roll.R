rollmean <- function(x, n, fill=NA) {
  ans = .Call(Crollmean, x, as.integer(n), as.double(fill), PACKAGE="data.table")
  ans
}

