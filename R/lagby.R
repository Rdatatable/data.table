lagby = function(..., by, fill) {
  x = list(...)
  lagbyv(x, by, fill)
}

lagbyv = function(x, by, fill) {
  if (missing(fill) && is.list(x))
    fill = as.list(rep(NA_real_, length(x)))
  else if (missing(fill) && is.atomic(x))
    fill = list(NA_real_)
  if (is.list(fill) && any(lengths(fill) != 1L))
    stopf("Each element in fill must be of length 1L.")
  if (is.list(x) && length(fill) != length(x))
    stopf("fill needs to be same length as x for filling first group.")
  if (is.list(x) && any(idx <- lengths(x) != length(by)))
    stopf(ngettext(sum(idx),
                   "Element %d in x must have the same length as by (%d)",
                   "Element %d in x must have the same length as by (%d)"),
          brackify(which(idx)), length(by), domain=NA)
  if (!is.list(x) && length(x) != length(by))
    stopf("x must be same length as by.")
  #in case a factor is filled with a new level
  if (is.list(x)) {
    for (k in seq_along(x)) {
      if (is.factor(x[[k]])) {
        levels(x[[k]]) = c(levels(x[[k]]), fill[[k]])
      }
    }
  } else if (is.factor(x)) {
    levels(x) = c(levels(x), fill[[1L]])
  }
  .Call(Clagby, x, by, fill)
}
