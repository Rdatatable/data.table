rollmean <- function(x, k, fill=NA_real_) {
    ans = .Call(Crollmean, x, as.integer(k), fill, PACKAGE="data.table")
    ans
}

