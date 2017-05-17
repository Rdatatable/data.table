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
    if (!any((index_class <- class(x[[1L]])) %in% c("POSIXct","Date"))) stop("data.table must have a POSIXct, Date or IDate column on first position, use `setcolorder` function.")
    colsNumeric = vapply_1b(x, is.numeric)[-1L] # exclude first col, xts index
    if (any(!colsNumeric)) warning(paste("Following columns are not numeric and will be omitted:", paste(names(colsNumeric)[!colsNumeric], collapse=", ")))
    r = setDF(x[, .SD, .SDcols=names(colsNumeric)[colsNumeric]])
    xts::as.xts(r, order.by=if ("IDate" %in% index_class) as.Date(x[[1L]]) else x[[1L]])
}
