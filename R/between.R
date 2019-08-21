# is x[i] in between lower[i] and upper[i] ?
between = function(x, lower, upper, incbounds=TRUE, NAbounds=TRUE) {
  if (!isTRUEorNA(NAbounds)) stop("NAbounds must be TRUE or NA")
  if (is.logical(x)) stop("between has been x of type logical")
  if (is.logical(lower)) lower = as.integer(lower)   # typically NA (which is logical type)
  if (is.logical(upper)) upper = as.integer(upper)   # typically NA (which is logical type)
  is.px = function(x) inherits(x, "POSIXct")
  is.i64 = function(x) inherits(x, "integer64")   # this is true for nanotime too
  # POSIX special handling to auto coerce character
  if (is.px(x) && !is.null(tz<-attr(x, "tzone", exact=TRUE)) && nzchar(tz) &&
      (is.character(lower) || is.character(upper))) {
    try_posix_cast = function(x, tz) {tryCatch(
      list(status=0L, value=as.POSIXct(x, tz = tz)),
      error = function(e) list(status=1L, value=NULL, message=e[["message"]])
    )}
    if (is.character(lower)) {
      ans = try_posix_cast(lower, tz)
      if (ans$status==0L) lower = ans$value
      else stop("'between' function the 'x' argument is a POSIX class while 'lower' was not, coercion to POSIX failed with: ", ans$message)
    }
    if (is.character(upper)) {
      ans = try_posix_cast(upper, tz)
      if (ans$status==0L) upper = ans$value
      else stop("'between' function the 'x' argument is a POSIX class while 'upper' was not, coercion to POSIX failed with: ", ans$message)
    }
    stopifnot(is.px(x), is.px(lower), is.px(upper)) # nocov # internal
  }
  # POSIX check timezone match
  if (is.px(x) && is.px(lower) && is.px(upper)) {
    tz_match = function(x, y, z) { # NULL match "", else all identical
      ((is.null(x) || !nzchar(x)) && (is.null(y) || !nzchar(y)) && (is.null(z) || !nzchar(z))) ||
        (identical(x, y) && identical(x, z))
    }
    if (!tz_match(attr(x, "tzone", exact=TRUE), attr(lower, "tzone", exact=TRUE), attr(upper, "tzone", exact=TRUE))) {
      stop("'between' function arguments have mismatched timezone attribute, align all arguments to same timezone")
    }
  }
  if (is.i64(x)) {
    if (!requireNamespace("bit64", quietly=TRUE)) stop("trying to use integer64 class when 'bit64' package is not installed") # nocov
    if (!is.i64(lower) && is.numeric(lower)) lower = bit64::as.integer64(lower)
    if (!is.i64(upper) && is.numeric(upper)) upper = bit64::as.integer64(upper)
  } else if (is.i64(lower) || is.i64(upper)) {
    stop("'lower' and/or 'upper' are integer64 class while 'x' is not. Please align classes before passing to 'between'.")
  }
  is.supported = function(x) is.numeric(x) || is.character(x) || is.px(x)
  if (is.supported(x) && is.supported(lower) && is.supported(upper)) {
    # faster parallelised version for int/double.
    # Cbetween supports length(lower)==1 (recycled) and (from v1.12.0) length(lower)==length(x).
    # length(upper) can be 1 or length(x) independently of lower
    .Call(Cbetween, x, lower, upper, incbounds, NAbounds)
  } else {
    if (isTRUE(getOption("datatable.verbose"))) cat("optimised between not available for this data type, fallback to slow R routine\n")
    if (isTRUE(NAbounds)) stop("Not yet implemented NAbounds for this non-numeric and non-character type")
    if (incbounds) x>=lower & x<=upper
    else x>lower & x<upper
  }
}

# %between% is vectorised, #534.
"%between%" = function(x, y) {
  ysub = substitute(y)
  if (is.call(ysub) && ysub[[1L]]==".") {
    ysub[[1L]]=quote(list)
    y = eval.parent(ysub)
  }
  if ((l <- length(y)) != 2L) {
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
inrange = function(x,lower,upper,incbounds=TRUE) {
  query = setDT(list(x=x))
  subject = setDT(list(l=lower, u=upper))
  ops = if (incbounds) c(4L, 2L) else c(5L, 3L) # >=,<= and >,<
  verbose = isTRUE(getOption("datatable.verbose"))
  if (verbose) {last.started.at=proc.time();cat("forderv(query) took ... ");flush.console()}
  if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
  ans = bmerge(shallow(subject), query, 1L:2L, c(1L,1L),
      0, c(FALSE, TRUE), 0L, "all", ops, verbose) # fix for #1819, turn on verbose messages
  xo = ans$xo
  options(datatable.verbose=FALSE)
  setDT(ans[c("starts", "lens")], key=c("starts", "lens"))
  options(datatable.verbose=verbose)
  if (verbose) {last.started.at=proc.time();cat("Generating final logical vector ... ");flush.console()}
  .Call(Cinrange, idx <- vector("logical", length(x)), xo, ans[["starts"]], ans[["lens"]])
  if (verbose) {cat("done in",timetaken(last.started.at),"\n"); flush.console}
  idx
}

"%inrange%" = function(x,y) inrange(x,y[[1L]],y[[2L]],incbounds=TRUE)
