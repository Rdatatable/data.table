frankv = function(x, cols=seq_along(x), order=1L, na.last=TRUE, ties.method=c("average", "first", "last", "random", "max", "min", "dense")) {
  ties.method = match.arg(ties.method)
  if (!length(na.last)) stop('length(na.last) = 0')
  if (length(na.last) != 1L) {
    warning("length(na.last) > 1, only the first element will be used")
    na.last = na.last[1L]
  }
  keep = (na.last == "keep")
  na.last = as.logical(na.last)
  as_list = function(x) {
    xx = vector("list", 1L)
    .Call(Csetlistelt, xx, 1L, x)
    xx
  }
  if (is.atomic(x)) {
    if (!missing(cols) && !is.null(cols))
      stop("x is a single vector, non-NULL 'cols' doesn't make sense")
    cols = 1L
    x = as_list(x)
  } else {
    cols = colnamesInt(x, cols, check_dups=TRUE)
    if (!length(cols))
      stop("x is a list, 'cols' can not be 0-length")
  }
  x = .shallow(x, cols) # shallow copy even if list..
  setDT(x)
  cols = seq_along(cols)
  if (is.na(na.last)) {
    set(x, j = "..na_prefix..", value = is_na(x, cols))
    order = if (length(order) == 1L) c(1L, rep(order, length(cols))) else c(1L, order)
    cols = c(ncol(x), cols)
    nas  = x[[ncol(x)]]
  }
  if (ties.method == "random") {
    if (is.na(na.last)) {
      idx = which_(nas, FALSE)
      n = length(idx)  # changed in #4243 to match base R to pass test 1369
    } else {
      idx = NULL
      n = nrow(x)
    }
    set(x, idx, '..stats_runif..', stats::runif(n))
    order = if (length(order) == 1L) c(rep(order, length(cols)), 1L) else c(order, 1L)
    cols = c(cols, ncol(x))
  }
  xorder  = forderv(x, by=cols, order=order, sort=TRUE, retGrp=TRUE, na.last=if (isFALSE(na.last)) na.last else TRUE)
  xstart  = attr(xorder, 'starts', exact=TRUE)
  xsorted = FALSE
  if (!length(xorder)) {
    xsorted = TRUE
    xorder  = seq_along(x[[1L]])
  }
  ans = switch(ties.method,
    average = , min = , max =, dense =, last = {
      rank = .Call(Cfrank, xorder, xstart, uniqlengths(xstart, length(xorder)), ties.method)
    },
    first = , random = {
      if (xsorted) xorder else forderv(xorder)
    }
  )
  # take care of na.last="keep"
  V1 = NULL # for R CMD CHECK warning
  if (isTRUE(keep)) {
    ans = (setDT(as_list(ans))[which_(nas, TRUE), V1 := NA])[[1L]]
  } else if (is.na(na.last)) {
    ans = ans[which_(nas, FALSE)]
  }
  ans
}

frank = function(x, ..., na.last=TRUE, ties.method=c("average", "first", "last", "random", "max", "min", "dense")) {
  cols = substitute(list(...))[-1L]
  if (identical(as.character(cols), "NULL")) {
    cols  = NULL
    order = 1L
  } else if (length(cols)) {
    cols=as.list(cols)
    order=rep(1L, length(cols))
    for (i in seq_along(cols)) {
      v=as.list(cols[[i]])
      if (length(v) > 1L && v[[1L]] == "+") v=v[[-1L]]
      else if (length(v) > 1L && v[[1L]] == "-") {
        v=v[[-1L]]
        order[i] = -1L
      }
      cols[[i]]=as.character(v)
    }
    cols=unlist(cols, use.names=FALSE)
  } else {
    cols=colnames(x)
    order=if (is.null(cols)) 1L else rep(1L, length(cols))
  }
  frankv(x, cols=cols, order=order, na.last=na.last, ties.method=ties.method)

}
