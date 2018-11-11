froll <- function(fun, x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE, verbose=getOption("datatable.verbose")) {
  stopifnot(!missing(fun), is.character(fun), length(fun)==1L, !is.na(fun))
  algo = match.arg(algo)
  align = match.arg(align)
  ans = .Call(CfrollfunR, fun, x, n, fill, algo, align, na.rm, hasNA, adaptive, verbose)
  ans
}

frollmean <- function(x, n, fill=NA, algo=c("fast", "exact"), align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE, verbose=getOption("datatable.verbose")) {
  froll(fun="mean", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive, verbose=verbose)
}
