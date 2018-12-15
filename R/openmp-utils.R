setDTthreads <- function(threads=0, restore_after_fork=NULL) {
  invisible(.Call(CsetDTthreads, as.integer(threads), restore_after_fork))
}

getDTthreads <- function(verbose=getOption("datatable.verbose", FALSE)) {
  .Call(CgetDTthreads, verbose)
}

