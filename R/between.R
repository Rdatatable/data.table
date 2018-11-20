# is x[i] in between lower[i] and upper[i] ?
between <- function(x,lower,upper,incbounds=TRUE) {
  is_strictly_numeric <- function(x) is.numeric(x) && !"integer64" %chin% class(x)
  if (is_strictly_numeric(x) && is_strictly_numeric(lower) &&
    is_strictly_numeric(upper) && length(lower) == 1L && length(upper) == 1L) {
    # faster parallelised version for int/double for most common scenario
    .Call(Cbetween, x, lower, upper, incbounds)
  } else {
    if(incbounds) x>=lower & x<=upper
    else x>lower & x<upper
  }
}

# %between% is vectorised, #534.
"%between%" <- function(x, y) {
  if ((l <- length(y)) != 2L) {
    ysub = substitute(y)
    stop("RHS has length() ", l, "; expecting length 2. ",
         if (is.call(ysub) && ysub[[1L]] == 'c')
           sprintf("Perhaps you meant %s? ",
                   capture.output(print(`[[<-`(ysub, 1L, quote(list))))),
         "The first element should be the lower bound(s); ",
         "the second element should be the upper bound(s).")
  }
  between(x, y[[1L]], y[[2L]], incbounds=TRUE)
}
# If we want non inclusive bounds with %between%, just +1 to the left, and -1 to the right (assuming integers)

# issue FR #707
# is x[i] found anywhere within [lower, upper] range?
inrange <- function(x,lower,upper,incbounds=TRUE) {
  query = setDT(list(x=x))
  subject = setDT(list(l=lower, u=upper))
  ops = if (incbounds) c(4L, 2L) else c(5L, 3L) # >=,<= and >,<
  verbose = getOption("datatable.verbose")
  if (verbose) {last.started.at=proc.time();cat("forderv(query) took ... ");flush.console()}
  xo = forderv(query)
  if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
  ans = bmerge(shallow(subject), query, 1L:2L, c(1L,1L), xo,
      0, c(FALSE, TRUE), 0L, "all", ops, integer(0L),
      1L, verbose) # fix for #1819, turn on verbose messages
  options(datatable.verbose=FALSE)
  setDT(ans[c("starts", "lens")], key=c("starts", "lens"))
  options(datatable.verbose=verbose)
  if (verbose) {last.started.at=proc.time();cat("Generating final logical vector ... ");flush.console()}
  .Call(Cinrange, idx <- vector("logical", length(x)), xo, ans[["starts"]], ans[["lens"]])
  if (verbose) {cat("done in",timetaken(last.started.at),"\n"); flush.console}
  idx
}

"%inrange%" <- function(x,y) inrange(x,y[[1L]],y[[2L]],incbounds=TRUE)
