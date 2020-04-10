nrow2 = function(x) {
  if (!length(x)) return(0L)
  if (is.data.table(x)) nrow(x) else if (is.list(x)) length(x[[1L]]) else stop("nrow2 expects data.table or list")
}

uniqlist = function (l, order=integer()) {
  # used in "[.data.table" when doing groupby (!byjoin) to find the groups using byval
  # (length(byval) && length(byval[[1L]])) && (bysameorder || byindex)
  # and in duplicated.data.table when
  # haskey(x) && length(by) <= length(key(x)) && all(head(key(x), length(by)) == by)
  uniq(l, order, internal=TRUE)
}

uniq = function(x, order=integer(), internal=FALSE) {
  if (!isTRUE(internal) && !missing(order)) {
    if (!is.integer(order))
      stop("'order' must be an integer")
    if (length(order) != nrow2(x))
      stop("'order' must be same length as nrow of 'x'")
    if (anyDuplicated(order))
      stop("'order' should not contain duplicates")
    if (anyNA(order))
      stop("'order' should not contain NAs")
  }
  .Call(Cuniq, x, order)
}

# implemented for returning the lengths of groups obtained from uniqlist (for internal use only)
uniqlengths = function(x, len) {
  # check for type happens in C, but still converting to integer here to be sure.
  ans = .Call(Cuniqlengths, as.integer(x), as.integer(len))
  ans
}
