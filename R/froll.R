partial2adaptive = function(x, n, align) {
  if (align=="center")
    stopf("'partial' cannot be used together with align='center'")
  if (is.list(x) && length(unique(vapply(x, length, 0L)))!=1L)
    stopf("'partial' does not support variable length of columns in 'x'")
  if (!(is.numeric(n) || (is.list(n) && all(vapply(n, is.numeric, FALSE)))))
    stopf("n must be integer vector or list of integer vectors")
  len = if (is.list(x)) length(x[[1L]]) else length(x)
  verbose = getOption("datatable.verbose")
  if (verbose)
    cat("partial2adaptive: froll partial=TRUE trimming 'n' and redirecting to adaptive=TRUE\n")
  trimn = function(n, len, align) {
    n = min(n, len)
    if (align=="right")
      c(seq.int(n), rep(n, len-n))
    else
      c(rep(n, len-n), rev(seq.int(n)))
  }
  if (is.list(n)) {
    sapply(n, len, align, FUN=trimn)
  } else {
    trimn(n, len, align)
  }
}

froll = function(fun, x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE, partial=FALSE) {
  stopifnot(!missing(fun), is.character(fun), length(fun)==1L, !is.na(fun))
  algo = match.arg(algo)
  align = match.arg(align)
  if (isTRUE(partial)) {
    if (isTRUE(adaptive))
      stopf("'partial' argument cannot be used together with 'adaptive'")
    n = partial2adaptive(x, n, align)
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
  ans = .Call(CfrollfunR, fun, x, n, fill, algo, align, na.rm, hasNA, adaptive)
  if (!leftadaptive)
    ans
  else {
    if (verbose)
      cat("froll: adaptive=TRUE && align='left' post-processing from align='right'\n")
    rev2(ans)
  }
}

frollmean = function(x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE, partial=FALSE) {
  froll(fun="mean", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive, partial=partial)
}
frollsum = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE, partial=FALSE) {
  froll(fun="sum", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive, partial=partial)
}
frollmax = function(x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE, partial=FALSE) {
  froll(fun="max", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive, partial=partial)
}

frollapply = function(x, n, FUN, ..., fill=NA, align=c("right", "left", "center"), adaptive=FALSE, partial=FALSE) {
  FUN = match.fun(FUN)
  align = match.arg(align)
  if (isTRUE(partial)) {
    if (isTRUE(adaptive))
      stopf("'partial' argument cannot be used together with 'adaptive'")
    n = partial2adaptive(x, n, align)
    adaptive = TRUE
  }
  leftadaptive = isTRUE(adaptive) && align=="left"
  if (leftadaptive) {
    verbose = getOption("datatable.verbose")
    rev2 = function(x) if (is.list(x)) sapply(x, rev, simplify=FALSE) else rev(x)
    if (verbose)
      cat("frollapply: adaptive=TRUE && align='left' pre-processing for align='right'\n")
    x = rev2(x)
    n = rev2(n)
    align = "right"
  }
  rho = new.env()
  ans = .Call(CfrollapplyR, FUN, x, n, fill, align, adaptive, rho)
  if (!leftadaptive)
    ans
  else {
    if (verbose)
      cat("frollapply: adaptive=TRUE && align='left' post-processing from align='right'\n")
    rev2(ans)
  }
}
