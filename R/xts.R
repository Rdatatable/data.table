# nocov start. See tests 18.* in other.Rraw, not in the main suite.
as.data.table.xts = function(x, keep.rownames = TRUE, key=NULL, ...) {
  stopifnot(requireNamespace("xts"), !missing(x), xts::is.xts(x))
  if (length(keep.rownames) != 1L) stopf("keep.rownames must be length 1")
  if (is.na(keep.rownames)) stopf("keep.rownames must not be NA")
  # as.data.frame.xts will handle copying, and
  #   the error check above ensures as.data.frame.xts is applied
  r = setDT(as.data.frame(x, row.names=NULL))
  if (identical(keep.rownames, FALSE)) return(r[])
  index_nm = if (is.character(keep.rownames)) keep.rownames else "index"
  if (index_nm %chin% names(x)) stopf("Input xts object should not have '%s' column because it would result in duplicate column names. Rename '%s' column in xts or use `keep.rownames` to change the index column name.", index_nm, index_nm)
  r[, c(index_nm) := zoo::index(x), env=list(x=x)]
  setcolorder(r, c(index_nm, setdiff(names(r), index_nm)))
  # save to end to allow for key=index_nm
  setkeyv(r, key)
  r[]
}

as.xts.data.table = function(x, numeric.only = TRUE, ...) {
  stopifnot(requireNamespace("xts"), !missing(x), is.data.table(x))
  if (!xts::is.timeBased(x[[1L]])) stopf("data.table must have a time based column in first position, use `setcolorder` function to change the order, or see ?timeBased for supported types")
  r = x[, -1L]# exclude first col, xts index
  if (numeric.only) {
    colsNumeric = vapply_1b(r, is.numeric)
    if (!all(colsNumeric)) warningf("Following columns are not numeric and will be omitted: %s", brackify(names(colsNumeric)[!colsNumeric]))
    r = r[, .SD, .SDcols = names(colsNumeric)[colsNumeric]]
  }
  xts::xts(as.matrix(r), order.by = if (inherits(x[[1L]], "IDate")) as.Date(x[[1L]]) else x[[1L]])
}
# nocov end
