froll <- function(fun, x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), partial=FALSE, na.rm=FALSE, hasNA=NA, adaptive=FALSE, verbose=getOption("datatable.verbose")) {
  stopifnot(!missing(fun), is.character(fun), length(fun)==1L, !is.na(fun))
  align = match.arg(align)
  ans = .Call(CfrollfunR, fun, x, n, fill, exact, align, partial, na.rm, hasNA, adaptive, verbose)
  ans
}

frollmean <- function(x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), partial=FALSE, na.rm=FALSE, hasNA=NA, adaptive=FALSE, verbose=getOption("datatable.verbose")) {
  froll(fun="mean", x=x, n=n, fill=fill, exact=exact, align=align, partial=partial, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive, verbose=verbose)
}

frollsum <- function(x, n, fill=NA, exact=FALSE, align=c("right", "left", "center"), partial=FALSE, na.rm=FALSE, hasNA=NA, adaptive=FALSE, verbose=getOption("datatable.verbose")) {
  froll(fun="sum", x=x, n=n, fill=fill, exact=exact, align=align, partial=partial, na.rm=na.rm, hasNA=hasNA, adaptive=adaptive, verbose=verbose)
}
