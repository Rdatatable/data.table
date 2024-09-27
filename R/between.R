# is x[i] in between lower[i] and upper[i] ?
between = function(x, lower, upper, incbounds=TRUE, NAbounds=TRUE, check=FALSE) {
  if (is.logical(x)) stopf("between has been passed an argument x of type logical")
  if (is.logical(lower)) lower = as.integer(lower)   # typically NA (which is logical type)
  if (is.logical(upper)) upper = as.integer(upper)   # typically NA (which is logical type)
  is.px = function(x) inherits(x, "POSIXct")
  is.i64 = function(x) inherits(x, "integer64")   # this is true for nanotime too
  # POSIXct special handling to auto coerce character
  if (is.px(x) && (is.character(lower) || is.character(upper))) {
    tz = attr(x, "tzone", exact=TRUE)
    if (is.null(tz)) tz = ""
    if (is.character(lower)) lower = tryCatch(as.POSIXct(lower, tz=tz), error=function(e)stopf(
      "The 'x' argument of the 'between' function is POSIXct while '%s' was not, coercion to POSIXct failed with: %s", 'lower', conditionMessage(e)))
    if (is.character(upper)) upper = tryCatch(as.POSIXct(upper, tz=tz), error=function(e)stopf(
      "The 'x' argument of the 'between' function is POSIXct while '%s' was not, coercion to POSIXct failed with: %s", 'upper', conditionMessage(e)))
    stopifnot(is.px(x), is.px(lower), is.px(upper)) # nocov # internal
  }
  # POSIX check timezone match
  if (is.px(x) && is.px(lower) && is.px(upper)) {
    tzs = sapply(list(x,lower,upper), function(x) {
      tt = attr(x,"tzone",exact=TRUE)
      if (is.null(tt)) "" else tt
    })
    # lower/upper should be more tightly linked than x/lower, so error
    #   if the former don't match but only inform if they latter don't
    if (tzs[2L]!=tzs[3L]) {
      stopf("'between' lower= and upper= are both POSIXct but have different tzone attributes: %s. Please align their time zones.", brackify(tzs[2:3], quote=TRUE))
      # otherwise the check in between.c that lower<=upper can (correctly) fail for this reason
    }
    if (tzs[1L]!=tzs[2L]) {
      messagef("'between' arguments are all POSIXct but have mismatched tzone attributes: %s. The UTC times will be compared.", brackify(tzs, quote=TRUE))
      # the underlying numeric is always UTC anyway in POSIXct so no coerce is needed; just compare as-is. As done by CoSMoS::example(analyzeTS), #3581
    }
  }
  if (is.i64(x)) {
    if (!requireNamespace("bit64", quietly=TRUE)) stopf("trying to use integer64 class when 'bit64' package is not installed") # nocov
    if (!is.i64(lower) && is.numeric(lower)) lower = bit64::as.integer64(lower)
    if (!is.i64(upper) && is.numeric(upper)) upper = bit64::as.integer64(upper)
  }
  is.supported = function(x) is.numeric(x) || is.character(x) || is.px(x)
  if (is.supported(x) && is.supported(lower) && is.supported(upper)) {
    # faster parallelised version for int/double/character
    # Cbetween supports length(lower)==1 (recycled) and (from v1.12.0) length(lower)==length(x).
    # length(upper) can be 1 or length(x) independently of lower
    .Call(Cbetween, x, lower, upper, incbounds, NAbounds, check)
  } else {
    if (isTRUE(getOption("datatable.verbose"))) catf("optimised between not available for this data type, fallback to slow R routine\n")
    if (isTRUE(NAbounds) && (anyNA(lower) || anyNA(upper))) stopf("Not yet implemented NAbounds=TRUE for this non-numeric and non-character type")
    if (check && any(lower>upper, na.rm=TRUE)) stopf("Some lower>upper for this non-numeric and non-character type")
    if (incbounds) x>=lower & x<=upper  # this & is correct not &&
    else           x> lower & x< upper
  }
}

# %between% is vectorised, #534.
"%between%" = function(x, y) {
  ysub = substitute(y)
  if (ysub %iscall% ".") {
    ysub[[1L]]=quote(list)
    y = eval.parent(ysub)
  }
  if ((l <- length(y)) != 2L) {
    suggestion <- if (ysub %iscall% 'c') gettextf("Perhaps you meant %s? ", capture.output(print(`[[<-`(ysub, 1L, quote(list))))) else ""
    stopf("RHS has length() %d; expecting length 2. %sThe first element should be the lower bound(s); the second element should be the upper bound(s).", l, suggestion)
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
  if (verbose) {last.started.at=proc.time();catf("forderv(query) took ... ");flush.console()}
  if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
  ans = bmerge(shallow(subject), query, 1L:2L, c(1L,1L),
      0, c(FALSE, TRUE), 0L, "all", ops, verbose) # fix for #1819, turn on verbose messages
  xo = ans$xo
  options(datatable.verbose=FALSE)
  setDT(ans[c("starts", "lens")], key=c("starts", "lens"))
  options(datatable.verbose=verbose)
  if (verbose) {last.started.at=proc.time();catf("Generating final logical vector ... ");flush.console()}
  .Call(Cinrange, idx <- vector("logical", length(x)), xo, ans[["starts"]], ans[["lens"]])
  if (verbose) {catf("done in %s\n",timetaken(last.started.at)); flush.console()}
  idx
}

"%inrange%" = function(x,y) inrange(x,y[[1L]],y[[2L]],incbounds=TRUE)
