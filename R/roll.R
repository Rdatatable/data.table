rollmean <- function(x, k) {
    ans = .Call(Crollmean, x, as.integer(k))
    ans
}

