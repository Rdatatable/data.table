fwrite <- function(x, file.path, append = FALSE, quote = TRUE,
                   sep = ",", eol = "\n", na = "", col.names = TRUE, qmethod = "double") {
  
  # validate arguments
  stopifnot(is.data.frame(x))
  stopifnot(ncol(x) > 0)
  
  stopifnot(length(quote) == 1 && class(quote) == "logical")
  stopifnot(length(sep) == 1 && class(sep) == "character" && nchar(sep) == 1)
  stopifnot(length(eol) == 1 && class(eol) == "character")
  stopifnot(length(qmethod) == 1 && qmethod %in% c("double", "escape"))
  stopifnot(length(col.names) == 1 && class(col.names) == "logical")
  stopifnot(length(append) == 1 && class(append) == "logical")
  
  # handle paths like "~/foo/bar"
  file.path <- path.expand(file.path)
  
  quoted_cols <- rep(quote, ncol(x))
  
  # write header row separately for correct quoting of row names
  if (col.names && !append) {
    .Call(Cwritefile, as.list(names(x)), file.path, sep, eol, na, quoted_cols, qmethod == "escape", append)
    append <- TRUE
  }
  
  # handle empty x
  if (nrow(x) == 0) return()
  
  # determine from column types, which ones should be quoted
  if (quote) {
    column_types <- sapply(x, class)
    quoted_cols <- column_types %chin% c('character', 'factor')
  }
  
  .Call(Cwritefile, x, file.path, sep, eol, na, quoted_cols, qmethod == "escape", append)
}

