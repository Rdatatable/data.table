froll = function(fun, x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE) {
  stopifnot(!missing(fun), is.character(fun), length(fun)==1L, !is.na(fun))
  algo = match.arg(algo)
  align = match.arg(align)
  ans = .Call(CfrollfunR, fun, x, n, fill, algo, align, na.rm, hasNA, adaptive)
  ans
}

frollmean = function(x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE) {
  froll(fun="mean", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive)
}
frollsum = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE) {
  froll(fun="sum", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive)
}
frollapply = function(x, n, FUN, ..., fill=NA, align=c("right", "left", "center")) {
  FUN = match.fun(FUN)
  align = match.arg(align)
  rho = new.env()
  ans = .Call(CfrollapplyR, FUN, x, n, fill, align, rho)
  ans
}
