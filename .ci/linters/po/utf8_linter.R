utf8_linter <- function(po_file) {
  if (!any(grepl("charset=UTF-8", readLines(po_file), fixed=TRUE)))
    stop("In ", po_file, ", please use charset=UTF-8.")
}
