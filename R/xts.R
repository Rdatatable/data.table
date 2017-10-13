as.data.table.xts <- function(x, keep.rownames = TRUE, ...) {
    stopifnot(requireNamespace("xts"), !missing(x), xts::is.xts(x))
    r = setDT(as.data.frame(x, row.names=NULL))
    if (!keep.rownames) return(r[])
    if ("index" %in% names(x)) stop("Input xts object should not have 'index' column because it would result in duplicate column names. Rename 'index' column in xts or use `keep.rownames=FALSE` and add index manually as another column.")
    r[, "index" := zoo::index(x)]
    setcolorder(r, c("index", setdiff(names(r), "index")))[]
}

as.xts.data.table <- function(x, ...) {
    stopifnot(requireNamespace("xts"), !missing(x), is.data.table(x))
    if (!xts::is.timeBased(x[[1L]])) stop("data.table must have a time based column in first position, use `setcolorder` function to change the order, or see ?timeBased for supported types")
    colsNumeric = vapply_1b(x, is.numeric)[-1L] # exclude first col, xts index
    if (any(!colsNumeric)) warning(paste("Following columns are not numeric and will be omitted:", paste(names(colsNumeric)[!colsNumeric], collapse = ", ")))
    r = setDF(x[, .SD, .SDcols = names(colsNumeric)[colsNumeric]])
    return(xts::as.xts(r, order.by = x[[1L]]))
}
