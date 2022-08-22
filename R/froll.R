froll = function(fun, x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE) {
  stopifnot(!missing(fun), is.character(fun), length(fun)==1L, !is.na(fun))
  algo = match.arg(algo)
  align = match.arg(align)
  leftadaptive = isTRUE(adaptive) && align=="left"  ## support for left added in #5441
  if (leftadaptive) {
    rev2 = function(x) if (is.list(x)) sapply(x, rev, simplify=FALSE) else rev(x)
    verbose = getOption("datatable.verbose")
    if (verbose)
      cat("froll: adaptive=TRUE && align='left' pre-processing for align='right'\n")
    x = rev2(x)
    n = rev2(n)
    align = "right"
  }
  ans = .Call(CfrollfunR, fun, x, n, fill, algo, align, na.rm, hasNA, adaptive)
  if (!leftadaptive)
    ans
  else {
    if (verbose)
      cat("froll: adaptive=TRUE && align='left' post-processing from align='right'\n")
    rev2(ans)
  }
}

frollmean = function(x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE) {
  froll(fun="mean", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive)
}
frollsum = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE) {
  froll(fun="sum", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive)
}
frollmax = function(x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE) {
  froll(fun="max", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive)
}
frollapply = function(x, n, FUN, ..., fill=NA, align=c("right", "left", "center"), adaptive) {
  FUN = match.fun(FUN)
  align = match.arg(align)
  if (!missing(adaptive))
    stopf("frollapply does not support 'adaptive' argument")
  rho = new.env()
  ans = .Call(CfrollapplyR, FUN, x, n, fill, align, rho)
  ans
}
