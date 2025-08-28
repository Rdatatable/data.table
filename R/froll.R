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
  if (!length(n))
    stopf("n must be non 0 length")
  if (align=="center")
    stopf("'partial' cannot be used together with align='center'")
  if (is.list(x) && length(unique(lengths(x))) != 1L)
    stopf("'partial' does not support variable length of columns in 'x'")
  len = if (is.list(x)) length(x[[1L]]) else length(x)
  verbose = getOption("datatable.verbose")
  if (!adaptive) {
    if (is.list(n))
      stopf("n must be an integer, list is accepted for adaptive TRUE")
    if (!is.numeric(n))
      stopf("n must be an integer vector or a list of integer vectors")
    if (verbose)
      catf("partial2adaptive: froll partial=TRUE trimming 'n' and redirecting to adaptive=TRUE\n")
    if (length(n) > 1L) {
      ## c(2,3) -> list(c(1,2,2,2),c(1,2,3,3)) ## for x=1:4
      lapply(n, len, align, FUN=trimn)
    } else {
      ## 3 -> c(1,2,3,3) ## for x=1:4
      trimn(n, len, align)
    }
  } else {
    if (!(is.numeric(n) || (is.list(n) && all(vapply_1b(n, is.numeric)))))
      stopf("n must be an integer vector or a list of integer vectors")
    if (length(unique(lengths(n))) != 1L)
      stopf("adaptive window provided in 'n' must not to have different lengths")
    if (is.numeric(n) && length(n) != len)
      stopf("length of 'n' argument must be equal to number of observations provided in 'x'")
    if (is.list(n) && length(n[[1L]]) != len)
      stopf("length of vectors in 'x' must match to length of adaptive window in 'n'")
    if (verbose)
      catf("partial2adaptive: froll adaptive=TRUE and partial=TRUE trimming 'n'\n")
    if (is.numeric(n)) {
      ## c(3,3,3,2) -> c(1,2,3,2) ## for x=1:4
      trimnadaptive(n, align)
    } else {
      ## list(c(3,3,3,2),c(4,2,3,3)) -> list(c(1,2,3,2),c(1,2,3,3)) ## for x=1:4
      lapply(n, align, FUN = trimnadaptive)
    }
  }
}

froll = function(fun, x, n, fill=NA, algo, align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, FUN, rho, give.names=FALSE) {
  align = match.arg(align)
  if (isTRUE(give.names))
    orig = list(n=n, adaptive=adaptive)
  if (isTRUE(partial)) {
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
  if (missing(FUN))
    ans = .Call(CfrollfunR, fun, x, n, fill, algo, align, na.rm, has.nf, adaptive)
  else
    ans = .Call(CfrollapplyR, FUN, x, n, fill, align, adaptive, rho)
  if (leftadaptive) {
    if (verbose)
      catf("froll: adaptive=TRUE && align='left' post-processing from align='right'\n")
    ans = rev2(ans)
  }
  if (isTRUE(give.names) && is.list(ans)) {
    n = orig$n
    adaptive = orig$adaptive
    nx = names(x)
    nn = names(n)
    if (is.null(nx)) nx = paste0("V", if (is.atomic(x)) 1L else seq_along(x))
    if (is.null(nn)) nn = if (adaptive) paste0("N", if (is.atomic(n)) 1L else seq_along(n)) else paste("roll", as.character(n), sep="_")
    setattr(ans, "names",  paste(rep(nx, each=length(nn)), nn, sep="_"))
  }
  ans
}

frollfun = function(fun, x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, hasNA, give.names=FALSE) {
  stopifnot(!missing(fun), is.character(fun), length(fun)==1L, !is.na(fun))
  if (!missing(hasNA)) {
    if (!is.na(has.nf))
      stopf("hasNA is deprecated, use has.nf instead")
    warningf("hasNA is deprecated, use has.nf instead")
    has.nf = hasNA
  } # remove check on next major release
  algo = match.arg(algo)
  froll(fun=fun, x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, give.names=give.names)
}

frollmean = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, hasNA, give.names=FALSE) {
  frollfun(fun="mean", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, hasNA=hasNA, give.names=give.names)
}
frollsum = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, hasNA, give.names=FALSE) {
  frollfun(fun="sum", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, hasNA=hasNA, give.names=give.names)
}
frollmax = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, hasNA, give.names=FALSE) {
  frollfun(fun="max", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, hasNA=hasNA, give.names=give.names)
}

frollapply = function(x, n, FUN, ..., fill=NA, align=c("right","left","center"), adaptive=FALSE, partial=FALSE, give.names=FALSE) {
  FUN = match.fun(FUN)
  rho = new.env()
  froll(FUN=FUN, rho=rho, x=x, n=n, fill=fill, align=align, adaptive=adaptive, partial=partial, give.names=give.names)
}
