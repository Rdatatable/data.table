rollmean <- function(x, n, fill=NA, verbose=getOption("datatable.verbose",FALSE)) {
  isTrueFalse = function(x) isTRUE(x) || identical(FALSE, x)
  stopifnot(isTrueFalse(verbose))
  ans = .Call(Crollmean, x, as.integer(n), fill, verbose, PACKAGE="data.table")
  ans
}
