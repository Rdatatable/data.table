as.data.table.xts <- function(x, keep.rownames = TRUE, ...){
  stopifnot(requireNamespace("xts"), !missing(x), xts::is.xts(x))
  if(!keep.rownames) return(setDT(as.data.frame(x, row.names=FALSE))[])
  if("index" %in% names(x)) stop("Input xts object should not have 'index' column because it would result in duplicate column names. Rename 'index' column in xts or use `keep.rownames=FALSE` and add index manually as another column.")
  r = setDT(as.data.frame(x, row.names=FALSE))
  index = NULL # fix for "no visible binding for global variable index"
  r[, index := zoo::index(x)]
  setcolorder(r,c("index",names(r)[names(r)!="index"]))[]
}

as.xts.data.table <- function(x){
  stopifnot(requireNamespace("xts"), !missing(x), is.data.table(x))
  if(!any(class(x[[1]]) %in% c("POSIXct","Date"))) stop("data.table must have a POSIXct or Date column on first position, use `setcolorder` function.")
  colsNumeric = sapply(x, is.numeric)[-1] # exclude first col, xts index
  if(any(!colsNumeric)) warning(paste("Following columns are not numeric and will be omitted:",paste(names(colsNumeric)[!colsNumeric],collapse=", ")))
  r = setDF(x[,.SD,.SDcols=names(colsNumeric)[colsNumeric]])
  xts::as.xts(r, order.by=x[[1]])
}
