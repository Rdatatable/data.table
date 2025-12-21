tools_check_linter = function(po_file) {
  res = tools::checkPoFile(po_file, strictPlural=TRUE)
  if (NROW(res)) {
    print(res)
    stop("Fix the above .po file issues.")
  }
}
