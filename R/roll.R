rollmean <- function(x, n, fill=NA) {
  ans = .Call(Crollmean, x, as.integer(n), as.double(fill), PACKAGE="data.table")
  ans
}

srollmean <- function(x, n, fill=NA, na.rm=TRUE) {
  stopifnot(is.numeric(x))
  if (!(l<-length(x))) return(x)
  ans = rep(as.double(fill), l)
  for (i in seq.int(length(x))) {
    if (i < n) next
    ans[i] = mean(x[(i-n+1L):i], na.rm=na.rm)
  }
  ans
}

