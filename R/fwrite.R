fwrite <- function(x, file.path, append = FALSE, quote = TRUE,
                   sep = ",", eol = if (.Platform$OS.type=="windows") "\r\n" else "\n",
                   na = "", col.names = TRUE, qmethod = "double", verbose=FALSE) {
  
  # validate arguments
  stopifnot(is.data.frame(x))
  stopifnot(ncol(x) > 0)
  isFALSE = function(x)identical(FALSE,x)  # it seems there is no isFALSE in R?
  stopifnot(isTRUE(quote) || isFALSE(quote))
  stopifnot(length(sep) == 1 && class(sep) == "character" && nchar(sep) == 1)
  stopifnot(length(eol) == 1 && class(eol) == "character")
  stopifnot(length(qmethod) == 1 && qmethod %in% c("double", "escape"))
  stopifnot(isTRUE(col.names) || isFALSE(col.names))
  stopifnot(isTRUE(append) || isFALSE(append))
  stopifnot(isTRUE(verbose) || isFALSE(verbose))
  if (append && missing(col.names)) col.names = FALSE  # Otto's test 1658.16 checks this
  
  # handle paths like "~/foo/bar"
  file.path <- path.expand(file.path)
  
  .Call(Cwritefile, x, file.path, sep, eol, na, quote, qmethod == "escape", append, col.names, verbose)
  invisible()
}

