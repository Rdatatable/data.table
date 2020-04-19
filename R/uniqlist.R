nrow2 = function(x) {
  if (!length(x)) return(0L)
  if (is.data.table(x)) nrow(x) else if (is.list(x)) length(x[[1L]]) else stop("nrow2 expects data.table or list")
}

uniqlist = function (l, order = -1L) {
  # used in "[.data.table" when doing groupby (!byjoin) to find the groups using byval
  # (length(byval) && length(byval[[1L]])) && (bysameorder || byindex)
  # and in duplicated.data.table when
  # haskey(x) && length(by) <= length(key(x)) && all(head(key(x), length(by)) == by)

  # those are only for backward compatibility, probably not really used anywhere, will keep 1962.010 and 1962.011 happy
  if (!is.list(l)) stop("l not type list")
  if (!length(l)) return(list(0L))
  # this is for compatibility to new uniq C code
  if (identical(order, -1L)) order = integer()

  funiq(l, order, safe=FALSE)
}

uniq = function(x, order=integer()) {
  if (!is.list(x))
    stop("'x' must be a data.table type object");
  if (!is.integer(order)) {
    if (is.numeric(order))
      order = as.integer(order)
    else
      stop("'order' must be an integer")
  }
  if (length(order) && length(order)!=nrow2(x))
    stop("'order' must be same length as nrow of 'x'")
  funiq(x, order, safe=TRUE)
}

# use safe=F when you are sure that 'order' is in 1:nrow(x)
# otherwise it segfaults, thus internal
funiq = function(x, order=integer(), safe=FALSE) {
  .Call(Cuniq, x, order, safe)
}

# implemented for returning the lengths of groups obtained from uniqlist (for internal use only)
uniqlengths = function(x, len) {
  # check for type happens in C, but still converting to integer here to be sure.
  ans = .Call(Cuniqlengths, as.integer(x), as.integer(len))
  ans
}
