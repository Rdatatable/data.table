partial2adaptive = function(x, n, align, adaptive) {
  if (align=="center")
    stopf("'partial' cannot be used together with align='center'")
  if (is.list(x) && length(unique(vapply(x, length, 0L)))!=1L)
    stopf("'partial' does not support variable length of columns in 'x'")
  if (!(is.numeric(n) || (is.list(n) && all(vapply(n, is.numeric, FALSE)))))
    stopf("n must be integer vector or list of integer vectors")
  len = if (is.list(x)) length(x[[1L]]) else length(x)
  verbose = getOption("datatable.verbose")
  if (!adaptive) {
    n = as.list(n) ## test 6006.032
    if (verbose)
      cat("partial2adaptive: froll partial=TRUE trimming 'n' and redirecting to adaptive=TRUE\n")
    trimn = function(n, len, align) {
      n = min(n, len)
      if (align=="right")
        c(seq.int(n), rep(n, len-n))
      else
        c(rep(n, len-n), rev(seq.int(n)))
    }
    sapply(n, len, align, FUN=trimn, simplify=FALSE)
  } else {
    if (!is.list(n)) n = list(n)
    if (length(unique(vapply(n, length, 0L)))!=1L)
      stopf("adaptive window provided in 'n' must not to have different lengths")
    if (length(n[[1L]]) != len)
      stopf("length of vectors in 'x' must match to length of adaptive window in 'n'")
    if (verbose)
      cat("partial2adaptive: froll adaptive=TRUE and partial=TRUE trimming 'n'\n")
    triman = function(n, align) {
      if (align=="right")
        pmin(n, seq_along(n))
      else
        pmin(n, rev(seq_along(n)))
    }
    sapply(n, align, FUN=triman, simplify=FALSE)
  }
}

froll = function(fun, x, n, fill=NA, algo, align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, FUN, rho, give.names=FALSE) {
  align = match.arg(align)
  if (isTRUE(give.names))
    orig = list(n=n, adaptive=adaptive)
  if (isTRUE(partial)) {
    n = partial2adaptive(x, n, align, adaptive)
    adaptive = TRUE
  } ## support for partial added in #5441
  leftadaptive = isTRUE(adaptive) && align=="left"
  if (leftadaptive) {
    verbose = getOption("datatable.verbose")
    rev2 = function(x) if (is.list(x)) sapply(x, rev, simplify=FALSE) else rev(x)
    if (verbose)
      cat("froll: adaptive=TRUE && align='left' pre-processing for align='right'\n")
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
      cat("froll: adaptive=TRUE && align='left' post-processing from align='right'\n")
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
    warning("hasNA is deprecated, use has.nf instead")
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
  if (isTRUE(adaptive) && base::getRversion() < "3.4.0") ## support SET_GROWABLE_BIT
    stopf("frollapply adaptive=TRUE requires at least R 3.4.0"); # nocov
  FUN = match.fun(FUN)
  rho = new.env()
  froll(FUN=FUN, rho=rho, x=x, n=n, fill=fill, align=align, adaptive=adaptive, partial=partial, give.names=give.names)
}
