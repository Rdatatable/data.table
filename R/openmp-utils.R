setDTthreads = function(threads=NULL, restore_after_fork=NULL, percent=NULL, throttle=NULL) {
  if (!missing(percent)) {
    if (!missing(threads)) stopf("Provide either threads= or percent= but not both")
    if (length(percent) != 1L) stopf("percent= is provided but is length %d", length(percent))
    percent=as.integer(percent)
    if (is.na(percent) || percent<2L || percent>100L) stopf("percent==%d but should be a number between 2 and 100", percent)
    invisible(.Call(CsetDTthreads, percent, restore_after_fork, TRUE, as.integer(throttle)))
  } else {
    invisible(.Call(CsetDTthreads, as.integer(threads), restore_after_fork, FALSE, as.integer(throttle)))
  }
}

getDTthreads = function(verbose=getOption("datatable.verbose")) {
  .Call(CgetDTthreads, verbose)
}
