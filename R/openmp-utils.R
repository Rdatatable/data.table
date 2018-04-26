setDTthreads <- function(threads) {
  invisible(.Call(CsetDTthreads, as.integer(threads)))
}

getDTthreads <- function(verbose=getOption("datatable.verbose", FALSE)) {
  .Call(CgetDTthreads, verbose)
}

