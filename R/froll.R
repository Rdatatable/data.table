# helpers for partial2adaptive
trimn = function(n, len, align) {
  n = min(n, len) ## so frollsum(1:2, 3, partial=TRUE) works
  if (align=="right")
    c(seq_len(n), rep.int(n, len-n))
  else
    c(rep.int(n, len-n), rev(seq_len(n)))
}
trimnadaptive = function(n, align) {
  if (align=="right")
    pmin(n, seq_along(n))
  else
    pmin(n, rev(seq_along(n)))
}

# partial2adaptive helper function
## tune provided 'n' via partial=TRUE to adaptive=TRUE by prepared adaptive 'n' as shown in ?froll examples
# partial2adaptive(1:4, 2, "right", adaptive=FALSE)
# partial2adaptive(1:4, 2:3, "right", adaptive=FALSE)
# partial2adaptive(list(1:4, 2:5), 2:3, "right", adaptive=FALSE)
# frollsum(1:4, 2, partial=FALSE, adaptive=FALSE)
# frollsum(1:4, 2, partial=TRUE, adaptive=FALSE)
# frollsum(1:4, 2:3, partial=FALSE, adaptive=FALSE)
# frollsum(1:4, 2:3, partial=TRUE, adaptive=FALSE)
# frollsum(list(1:4, 2:5), 2:3, partial=FALSE, adaptive=FALSE)
# frollsum(list(1:4, 2:5), 2:3, partial=TRUE, adaptive=FALSE)
partial2adaptive = function(x, n, align, adaptive) {
  ## do not quote argument x and n arg names because frollapply has them in uppercase
  if (align=="center")
    stopf("'partial' cannot be used together with align='center'")
  if (is.list(x)) {
    if (!is.data.frame(x) && !equal.lengths(x)) ## froll
      stopf("'partial' does not support variable length of columns in x")
    else if (all.data.frame(x) && !equal.nrows(x)) ## frollapply by.column=F, single DT already wrapped into list
      stopf("'partial' does not support variable nrow of data.tables in x")
  }
  len = if (is.list(x)) {
    if (is.data.frame(x[[1L]])) ## frollapply by.column
      nrow(x[[1L]])
    else ## froll, this will work for both x list and x dt on input
      length(x[[1L]])
  } else length(x)
  verbose = getOption("datatable.verbose")
  if (!adaptive) {
    if (is.list(n))
      stopf("n must be an integer, list is accepted for adaptive TRUE")
    if (!is.numeric(n))
      stopf("n must be an integer vector or a list of integer vectors")
    if (verbose)
      catf("partial2adaptive: froll partial=TRUE trimming n and redirecting to adaptive=TRUE\n")
    if (length(n) > 1L) {
      lapply(n, len, align, FUN=trimn)
    } else {
      trimn(n, len, align)
    }
  } else {
    if (!(is.numeric(n) || (is.list(n) && all(vapply_1b(n, is.numeric)))))
      stopf("n must be an integer vector or a list of integer vectors")
    if (is.list(n) && length(unique(lengths(n))) != 1L)
      stopf("adaptive windows provided in n must not to have different lengths")
    if ((is.list(n) && length(n[[1L]]) != len) || (is.numeric(n) && length(n) != len))
      stopf("length of vectors in x must match to length of adaptive window in n")
    if (verbose)
      catf("partial2adaptive: froll adaptive=TRUE and partial=TRUE trimming n\n")
    if (!is.list(n))
      n = list(n)
    lapply(n, align, FUN=trimnadaptive)
  }
}

make.roll.names = function(x.len, n.len, n, x.nm, n.nm, fun, adaptive) {
  if (is.null(n.nm)) {
    if (!adaptive) {
      if (!is.numeric(n))
        stopf("internal error: misuse of make.names, n must be numeric for !adaptive") ## nocov
      n.nm = paste0("roll", fun, as.character(as.integer(n)))
    } else {
      n.nm = paste0("aroll", fun, seq_len(n.len))
    }
  } else if (!length(n.nm) && !adaptive)
    stopf("internal error: misuse of make.names, non-null length 0 n is not possible for !adaptive") ## nocov
  if (is.null(x.nm)) {
    x.nm = paste0("V", seq_len(x.len))
  }
  ans = if (length(x.nm)) { ## is.list(x) && !is.data.frame(x)
    if (length(n.nm)) { ## !adaptive || is.list(n)
      paste(rep(x.nm, each=length(n.nm)), n.nm, sep="_")
    } else { ## adaptive && is.numeric(n)
      x.nm
    }
  } else { ## (by.column && is.atomic(x)) || (!by.column && is.data.frame(x))
    if (length(n.nm)) { ## !adaptive || is.list(n)
      n.nm
    } else { ## adaptive && is.numeric(n)
      NULL
    }
  }
  if (!is.null(ans) && length(ans) != x.len*n.len)
    stopf("internal error: make.names generated names of wrong length") ## nocov
  ans
}

froll = function(fun, x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA) {
  stopifnot(!missing(fun), is.character(fun), length(fun)==1L, !is.na(fun))
  if (!missing(hasNA)) {
    if (!is.na(has.nf))
      stopf("hasNA is deprecated, use has.nf instead")
    warning("hasNA is deprecated, use has.nf instead")
    has.nf = hasNA
  } # remove check on next major release
  algo = match.arg(algo)
  align = match.arg(align)
  if (isTRUE(give.names)) {
    orig = list(n=n, adaptive=adaptive)
    xnam = if (is.list(x)) names(x) else character()
    nnam = if (isTRUE(adaptive)) {
      if (is.list(n)) names(n) else character()
    } else names(n)
    nx = if (is.list(x)) length(x) else 1L
    nn = if (isTRUE(adaptive)) {
      if (is.list(n)) length(n) else 1L
    } else length(n)
  }
  if (isTRUE(partial)) {
    if (!length(n))
      stopf("n must be non 0 length")
    n = partial2adaptive(x, n, align, adaptive)
    adaptive = TRUE
  }
  leftadaptive = isTRUE(adaptive) && align=="left"
  if (leftadaptive) {
    verbose = getOption("datatable.verbose")
    rev2 = function(x) if (is.list(x)) lapply(x, rev) else rev(x)
    if (verbose)
      catf("froll: adaptive=TRUE && align='left' pre-processing for align='right'\n")
    x = rev2(x)
    n = rev2(n)
    align = "right"
  } ## support for left adaptive added in #5441
  ans = .Call(CfrollfunR, fun, x, n, fill, algo, align, na.rm, has.nf, adaptive)
  if (leftadaptive) {
    if (verbose)
      catf("froll: adaptive=TRUE && align='left' post-processing from align='right'\n")
    ans = rev2(ans)
  }
  if (isTRUE(give.names) && is.list(ans)) {
    nms = make.roll.names(x.len=nx, n.len=nn, n=orig$n, x.nm=xnam, n.nm=nnam, fun=fun, adaptive=orig$adaptive)
    setattr(ans, "names", nms)
  }
  ans
}

frollmean = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA) {
  froll(fun="mean", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, hasNA=hasNA, give.names=give.names)
}
frollsum = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA) {
  froll(fun="sum", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, hasNA=hasNA, give.names=give.names)
}
frollmax = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA) {
  froll(fun="max", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, hasNA=hasNA, give.names=give.names)
}
