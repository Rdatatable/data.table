setDTthreads <- function(threads) {
    invisible(.Call(CsetDTthreads, as.integer(threads)))
}

getDTthreads <- function() {
    .Call(CgetDTthreads)
}

