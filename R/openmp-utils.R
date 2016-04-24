setthreads <- function(threads) {
    .Call(CsetThreads, as.integer(threads[1L]))
}

getthreads <- function() {
    .Call(CgetThreads)
}
