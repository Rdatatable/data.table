foverlaps = function(x, y, by.x=if (!is.null(key(x))) key(x) else key(y), by.y=key(y), maxgap=0L, minoverlap=1L, type=c("any", "within", "start", "end", "equal"), mult=c("all", "first", "last"), nomatch=getOption("datatable.nomatch", NA), which=FALSE, verbose=getOption("datatable.verbose")) {

  if (!is.data.table(y) || !is.data.table(x)) stop("y and x must both be data.tables. Use `setDT()` to convert list/data.frames to data.tables by reference or as.data.table() to convert to data.tables by copying.")
  maxgap = as.integer(maxgap); minoverlap = as.integer(minoverlap)
  which = as.logical(which)
  .unsafe.opt() #3585
  nomatch = if (is.null(nomatch)) 0L else as.integer(nomatch)
  if (!length(maxgap) || length(maxgap) != 1L || is.na(maxgap) || maxgap < 0L)
    stop("maxgap must be a non-negative integer value of length 1")
  if (!length(minoverlap) || length(minoverlap) != 1L || is.na(minoverlap) || minoverlap < 1L)
    stop("minoverlap must be a positive integer value of length 1")
  if (!length(which) || length(which) != 1L || is.na(which))
    stop("which must be a logical vector of length 1. Either TRUE/FALSE")
  if (!length(nomatch) || length(nomatch) != 1L || (!is.na(nomatch) && nomatch!=0L))
    stop("nomatch must either be NA or NULL")
  type = match.arg(type)
  mult = match.arg(mult)
  # if (maxgap > 0L || minoverlap > 1L) # for future implementation
  if (maxgap != 0L || minoverlap != 1L)
    stop("maxgap and minoverlap arguments are not yet implemented.")
  if (is.null(by.y))
    stop("'y' must be keyed (i.e., sorted, and, marked as sorted). Call setkey(y, ...) first, see ?setkey. Also check the examples in ?foverlaps.")
  if (length(by.x) < 2L || length(by.y) < 2L)
    stop("'by.x' and 'by.y' should contain at least two column names (or numbers) each - corresponding to 'start' and 'end' points of intervals. Please see ?foverlaps and examples for more info.")
  if (is.numeric(by.x)) {
    if (any(by.x < 0L) || any(by.x > length(x)))
      stop("Invalid numeric value for 'by.x'; it should be a vector with values 1 <= by.x <= length(x)")
    by.x = names(x)[by.x]
  }
  if (is.numeric(by.y)) {
    if (any(by.y < 0L) || any(by.y > length(y)))
      stop("Invalid numeric value for 'by.y'; it should be a vector with values 1 <= by.y <= length(y)")
    by.y = names(y)[by.y]
  }
  if (!is.character(by.x))
    stop("A non-empty vector of column names or numbers is required for by.x")
  if (!is.character(by.y))
    stop("A non-empty vector of column names or numbers is required for by.y")
  if (!identical(by.y, key(y)[seq_along(by.y)]))
    stop("The first ", length(by.y), " columns of y's key must be identical to the columns specified in by.y.")
  if (anyNA(chmatch(by.x, names(x))))
    stop("Elements listed in 'by.x' must be valid names in data.table 'x'")
  if (anyDuplicated(by.x) || anyDuplicated(by.y))
    stop("Duplicate columns are not allowed in overlap joins. This may change in the future.")
  if (length(by.x) != length(by.y))
    stop("length(by.x) != length(by.y). Columns specified in by.x should correspond to columns specified in by.y and should be of same lengths.")
  if (any(dup.x<-duplicated(names(x)))) #1730 - handling join possible but would require workarounds on setcolorder further, it is really better just to rename dup column
    stop("x has some duplicated column name(s): ",paste(unique(names(x)[dup.x]),collapse=","),". Please remove or rename the duplicate(s) and try again.")
  if (any(dup.y<-duplicated(names(y))))
    stop("y has some duplicated column name(s): ",paste(unique(names(y)[dup.y]),collapse=","),". Please remove or rename the duplicate(s) and try again.")
  xnames = by.x; xintervals = tail(xnames, 2L)
  ynames = by.y; yintervals = tail(ynames, 2L)
  xval1 = x[[xintervals[1L]]]; xval2 = x[[xintervals[2L]]]
  yval1 = y[[yintervals[1L]]]; yval2 = y[[yintervals[2L]]]
  if (!storage.mode(xval1) %chin% c("double", "integer") || !storage.mode(xval2) %chin% c("double", "integer") || is.factor(xval1) || is.factor(xval2)) # adding factors to the bunch, #2645
    stop("The last two columns in by.x should correspond to the 'start' and 'end' intervals in data.table 'x' and must be integer/numeric type.")
  if ( isTRUEorNA(any(xval2 - xval1 < 0L)) ) {
    # better error messages as suggested by @msummersgill in #3007. Thanks for the code too. Placing this inside so that it only runs if the general condition is satisfied. Should error anyway here.. So doesn't matter even if runs all if-statements; takes about 0.2s for anyNA check on 200 million elements .. acceptable speed for stoppage, I think, at least for now.
    if ( anyNA(xval1) ) {
      stop("NA values in data.table 'x' start column: '", xintervals[1L],"'. All rows with NA values in the range columns must be removed for foverlaps() to work.")
    } else if ( anyNA(xval2) ) {
      stop("NA values in data.table 'x' end column: '", xintervals[2L],"'. All rows with NA values in the range columns must be removed for foverlaps() to work.")
    } else stop("All entries in column ", xintervals[1L], " should be <= corresponding entries in column ", xintervals[2L], " in data.table 'x'.")
  }
  if (!storage.mode(yval1) %chin% c("double", "integer") || !storage.mode(yval2) %chin% c("double", "integer") || is.factor(yval1) || is.factor(yval2)) # adding factors to the bunch, #2645
    stop("The last two columns in by.y should correspond to the 'start' and 'end' intervals in data.table 'y' and must be integer/numeric type.")
  if ( isTRUEorNA(any(yval2 - yval1 < 0L) )) {
    if ( anyNA(yval1) ) {
      stop("NA values in data.table 'y' start column: '", yintervals[1L],"'. All rows with NA values in the range columns must be removed for foverlaps() to work.")
    } else if ( anyNA(yval2) ) {
      stop("NA values in data.table 'y' end column: '", yintervals[2L],"'. All rows with NA values in the range columns must be removed for foverlaps() to work.")
    } else stop("All entries in column ", yintervals[1L], " should be <= corresponding entries in column ", yintervals[2L], " in data.table 'y'.")
  }
  # POSIXct interval cols error check
  posx_chk = sapply(list(xval1, xval2, yval1, yval2), inherits, 'POSIXct')
  if (any(posx_chk) && !all(posx_chk)) {
    stop("Some interval cols are of type POSIXct while others are not. Please ensure all interval cols are (or are not) of POSIXct type")
  }
  # #1143, mismatched timezone
  getTZ = function(x) if (is.null(tz <- attr(x, "tzone", exact=TRUE))) "" else tz # "" == NULL AFAICT
  tzone_chk = c(getTZ(xval1), getTZ(xval2), getTZ(yval1), getTZ(yval2))
  if (length(unique(tzone_chk)) > 1L) {
    warning("POSIXct interval cols have mixed timezones. Overlaps are performed on the internal numerical representation of POSIXct objects (always in UTC epoch time), therefore printed values may give the impression that values don't overlap but their internal representations do Please ensure that POSIXct type interval cols have identical 'tzone' attributes to avoid confusion.")
  }
  ## see NOTES below:
  yclass = c(class(yval1), class(yval2))
  isdouble = FALSE; isposix = FALSE
  if ( any(c("numeric", "POSIXct") %chin% yclass) ) {
    # next representive double > x under the given precision (48,56 or 64-bit in data.table) = x*incr
    dt_eps = function() {
      bits = floor(log2(.Machine$double.eps))
      2 ^ (bits + (getNumericRounding() * 8L))
    }
    isdouble = TRUE
    isposix = "POSIXct" %chin% yclass
  }
  ## hopefully all checks are over. Now onto the actual task at hand.
  origx = x; x = shallow(x, by.x)
  origy = y; y = shallow(y, by.y)
  roll = switch(type, start=, end=, equal= 0.0, any=, within= +Inf)
  make_call = function(names, fun=NULL) {
    if (is.character(names))
      names = lapply(names, as.name)
    call = c(substitute(fun, list(fun=fun)), names)
    if (!is.null(fun)) as.call(call) else call
  }
  construct = function(icols, mcols, type=type) {
    icall = make_call(icols)
    setattr(icall, 'names', icols)
    mcall = make_call(mcols, quote(c))
    if (type %chin% c("within", "any")) {
      if (isposix) mcall[[2L]] = call("unclass", mcall[[2L]]) # fix for R-devel change in c.POSIXct
      mcall[[3L]] = substitute(
        # datetimes before 1970-01-01 are represented as -ve numerics, #3349
        if (isposix) unclass(val)*(1L + sign(unclass(val))*dt_eps())
        else if (isdouble) {
          # fix for #1006 - 0.0 occurs in both start and end
          # better fix for 0.0, and other -ves. can't use 'incr'
          # hopefully this doesn't open another can of worms
          (val+dt_eps())*(1L + sign(val)*dt_eps())
        }
        else val+1L, # +1L is for integer/IDate/Date class, for examples
        list(val = mcall[[3L]]))
    }
    make_call(c(icall, pos=mcall), quote(list))
  }
  uycols = switch(type, start = yintervals[1L],
              end = yintervals[2L], any =,
              within =, equal = yintervals)
  call = construct(head(ynames, -2L), uycols, type)
  if (verbose) {last.started.at=proc.time();cat("unique() + setkey() operations done in ...");flush.console()}
  uy = unique(y[, eval(call)]) # this started to fail from R 4.1 due to c(POSIXct, numeric)
  setkey(uy)[, `:=`(lookup = list(list(integer(0L))), type_lookup = list(list(integer(0L))), count=0L, type_count=0L)]
  if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
  matches = function(ii, xx, del, ...) {
    cols = setdiff(names(xx), del)
    xx = .shallow(xx, cols, retain.key = TRUE)
    ans = bmerge(xx, ii, seq_along(xx), seq_along(xx), mult=mult, ops=rep(1L, length(xx)), verbose=verbose, ...)
    # vecseq part should never run here, but still...
    if (ans$allLen1) ans$starts else vecseq(ans$starts, ans$lens, NULL) # nocov
  }
  indices = function(x, y, intervals, ...) {
    if (type == "start") {
      sidx = eidx = matches(x, y, intervals[2L], rollends=c(FALSE,FALSE), ...) ## TODO: eidx can be set to integer(0L)
    } else if (type == "end") {
      eidx = sidx = matches(x, y, intervals[1L], rollends=c(FALSE,FALSE), ...) ## TODO: sidx can be set to integer(0)
    } else {
      sidx = matches(x, y, intervals[2L], rollends=rep(type == "any", 2L), ...)
      eidx = matches(x, y, intervals[1L], rollends=c(FALSE,TRUE), ...)
    }
    list(sidx, eidx)
  }
  # nomatch has no effect here, just for passing arguments consistently to `bmerge`
  .Call(Clookup, uy, nrow(y), indices(uy, y, yintervals, nomatch=0L, roll=roll), maxgap, minoverlap, mult, type, verbose)
  if (maxgap == 0L && minoverlap == 1L) {
    # iintervals = tail(names(x), 2L)    # iintervals not yet used so commented out for now
    if (verbose) {last.started.at=proc.time();cat("binary search(es) done in ...");flush.console()}
    xmatches = indices(uy, x, xintervals, nomatch=0L, roll=roll)
    if (verbose) {cat(timetaken(last.started.at),"\n");flush.console()}
    olaps = .Call(Coverlaps, uy, xmatches, mult, type, nomatch, verbose)
  }
  # nocov start
  else if (maxgap == 0L && minoverlap > 1L) {
    stop("Not yet implemented")
  } else if (maxgap > 0L && minoverlap == 1L) {
    stop("Not yet implemented")
  } else if (maxgap > 0L && minoverlap > 1L) {
    if (maxgap > minoverlap)
      warning("maxgap > minoverlap. maxgap will have no effect here.")
    stop("Not yet implemented")
  }
  # nocov end

  setDT(olaps)
  setnames(olaps, c("xid", "yid"))
  yid = NULL  # for 'no visible binding for global variable' from R CMD check on i clauses below

  # if (type == "any") setorder(olaps) # at times the combine operation may not result in sorted order
  # CsubsetDT bug has been fixed by Matt. So back to using it! Should improve subset substantially.
  if (which) {
    if (mult %chin% c("first", "last"))
      return (olaps$yid)
    else if (!is.na(nomatch))
      return (.Call(CsubsetDT, olaps, which(olaps$yid > 0L), seq_along(olaps)))
    else return (olaps)
  } else {
    if (!is.na(nomatch))
      olaps = .Call(CsubsetDT, olaps, which(olaps$yid > 0L), seq_along(olaps))
    ycols = setdiff(names(origy), head(by.y, -2L))
    idx = chmatch(ycols, names(origx), nomatch=0L)
    ans = .Call(CsubsetDT, origx, olaps$xid, seq_along(origx))
    if (any(idx>0L))
      setnames(ans, names(ans)[idx], paste0("i.", names(ans)[idx]))
    xcols1 = head(by.x, -2L)
    xcols2 = setdiff(names(ans), xcols1)
    ans[, (ycols) := .Call(CsubsetDT, origy, olaps$yid, chmatch(ycols, names(origy)))]
    setcolorder(ans, c(xcols1, ycols, xcols2))
    return (ans[])
  }
}

# Notes: (If there's a better way than the solution I propose here, I'd be glad to apply it.)
# Side note: This post is a great read on floating points: http://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/

# We add 1L to the end coordinate in case of integers to generate lookup to identify interval overlaps properly. And that is great!
# However for double precision, there are two issues we need to take into consideration:

# ---
# Firstly, assuming 64-bit precision, we can't simply add 1L. For e.g., consider:
#   x = data.table(start=0.88, end=0.88)
#   y = data.table(start=0.26, end=0.61, key=c("start", "end"))
# and we'd like to do:
#   foverlaps(x, y, type="any")
# Adding 1 to 0.61 will result in 1.61, and will make sure 0.88 falls between 0.26 and 1.61, and that's wrong!

# POSIXct objects are internally numeric as well.

# So how do we determine the "increment" (1L for integers) for numeric type?
# To get there, we need to understand that the "increment" is a great idea. Just the value isn't correct in case of numerics. Let's consider
# just ordinary "numeric" objects (non-POSIXct). The idea behind increment is to ensure that the "end" coordinate gets incremented to the *next
# distinguishable representative number*!!! In case of integers, the next integer after 5L is 6L. Simple. Increment is 1L (assuming no integer
# overflows). In case of "numeric" that "increment" is (1 + .Machine$double.eps), and this gets *multiplied* (not added) to "end" coordinate.
# Simple again. It fixes the problem, for now (read on).

# NOTE THAT simply doing ("end" + .Machine$double.eps ^ 0.5) is insufficient because this doesn't work as the numbers grow bigger. For e.g., try:
#   identical(3e8, 3e8+.Machine$double.eps^0.5)
#   identical(3e8, 3e8*(1+.Machine$double.eps))

# The important point is that we **need to be ABSOLUTELY sure** that "end coordinate" gets incremented to it's *next representative number*. Why? Consider a 'subject' interval [4,4]. When we collapse this to get the 1D-form and take `unique`, if the "end" coordinate is not distinguishable from the start coordinate, `unique` will return just one value. And this brings us back to square-one (the reason why we needed to add one in the first place!!). For example, consider query = c(5,5) and subject = c(4,4). Now, after collapsing and taking unique, if we end up with just one 4, then performing the join later will result in [5,5] actually matching 4 whose lookup will have valid indices and not NULL resulting in an incorrect overlap. We absolutely need the second 4, and we want it to be greater than the start 4, but just the smallest separation between them possible (so that we don't miss any other numbers that fall in that range).

# For POSIXct objects, we could still do the same. But a) multiplication is not supported for POSIXt objects, so we'll have to unclass it, multiply and convert back.. which is not ideal - timezone, time spent on conversion etc.. and b) all.equal in base considers a tolerance of 0.001 for POSIXt objects, I'm guessing this is for "millisecond" resolution? The problem with (b) is that more than millisecond resolution will return incorrect results again.

# More than millisecond resolution. Results are not stable.
#   tt = c( as.POSIXct('2011-10-11 07:49:36.0003'), as.POSIXct('2011-10-11 07:49:36.0199'), as.POSIXct('2011-10-11 07:49:36.0399'))
#   DT1 = data.table(start=tt, end=tt)
#   DT2 = data.table(start=tt[2], end=tt[2])
#   setkey(DT2)
#   foverlaps(DT1, DT2, which=TRUE)

# So, to put an end to this problem, we'll unclass it, multiply and convert back. In any case, the join does not depend on the timezone, as the internal numeric equivalent seems to be identical irrespective of the time zones.. So that's good news!
# ---
# Secondly, we've 48,56 and 64-bit precision in data.table (through get and setNumericRounding). And default right now is 48-bit. So, we've to make sure that the "end" interval should be multiplied accordingly depending on the current precision. This is taken care of by dt_eps(). Quite simple to follow, really.
# ---

# I believe this is the most correct way of doing it (probably convoluted way, and there are simpler ways... but I've not come across it).

# Tests are added to ensure we cover these aspects (to my knowledge) to ensure that any undesirable changes in the future breaks those tests.

# Conclusion: floating point manipulations are hell!

