setkey = function(x, ..., verbose=getOption("datatable.verbose"), physical=TRUE)
{
  if (is.character(x)) stopf("x may no longer be the character name of the data.table. The possibility was undocumented and has been removed.")
  cols = as.character(substitute(list(...))[-1L])
  if (!length(cols)) { cols=colnames(x) }
  else if (identical(cols,"NULL")) cols=NULL
  setkeyv(x, cols, verbose=verbose, physical=physical)
}

# FR #1442
setindex = function(...) setkey(..., physical=FALSE)
setindexv = function(x, cols, verbose=getOption("datatable.verbose")) {
  if (is.list(cols)) {
    sapply(cols, setkeyv, x=x, verbose=verbose, physical=FALSE)
    return(invisible(x))
  } else {
    setkeyv(x, cols, verbose=verbose, physical=FALSE)
  }
}

setkeyv = function(x, cols, verbose=getOption("datatable.verbose"), physical=TRUE)
{
  if (is.null(cols)) {   # this is done on a data.frame when !cedta at top of [.data.table
    if (physical) setattr(x,"sorted",NULL)
    setattr(x,"index",NULL)  # setkey(DT,NULL) also clears secondary keys. setindex(DT,NULL) just clears secondary keys.
    return(invisible(x))
  }
  if (!missing(verbose)) {
    stopifnot(isTRUEorFALSE(verbose))
    # set the global verbose option because that is fetched from C code without having to pass it through
    oldverbose = options(datatable.verbose=verbose)
    on.exit(options(oldverbose))
  }
  if (!is.data.table(x)) stopf("x is not a data.table")
  if (!is.character(cols)) stopf("cols is not a character vector. Please see further information in ?setkey.")
  if (physical && .Call(C_islocked, x)) stopf("Setting a physical key on .SD is reserved for possible future use; to modify the original data's order by group. Try setindex() instead. Or, set*(copy(.SD)) as a (slow) last resort.")
  if (!length(cols)) {
    warningf("cols is a character vector of zero length. Removed the key, but use NULL instead, or wrap with suppressWarnings() to avoid this warning.")
    setattr(x,"sorted",NULL)
    return(invisible(x))
  }
  if (identical(cols,"")) stopf("cols is the empty string. Use NULL to remove the key.")
  if (!all(nzchar(cols))) stopf("cols contains some blanks.")
  cols = gsub("`", "", cols, fixed = TRUE)
  miss = !(cols %chin% colnames(x))
  if (any(miss)) stopf("some columns are not in the data.table: %s", brackify(cols[miss]))

  if (physical && identical(head(key(x), length(cols)), cols)){ ## for !physical we need to compute groups as well #4387
    ## key is present but x has a longer key. No sorting needed, only attribute is changed to shorter key.
    setattr(x,"sorted",cols)
    return(invisible(x))
  }

  if (".xi" %chin% names(x)) stopf("x contains a column called '.xi'. Conflicts with internal use by data.table.")
  for (i in cols) {
    .xi = x[[i]]  # [[ is copy on write, otherwise checking type would be copying each column
    if (!typeof(.xi) %chin% ORDERING_TYPES) stopf("Column '%s' is type '%s' which is not supported as a key column type, currently.", i, typeof(.xi))
  }
  if (!is.character(cols) || length(cols)<1L) internal_error("'cols' should be character at this point") # nocov

  if (verbose) {
    # we now also retGrp=TRUE #4387 for !physical
    tt = suppressMessages(system.time(o <- forderv(x, cols, sort=TRUE, retGrp=!physical, reuseSorting=TRUE)))  # system.time does a gc, so we don't want this always on, until refcnt is on by default in R
    # suppress needed for tests 644 and 645 in verbose mode
    catf("forder took %.03f sec\n", tt["user.self"]+tt["sys.self"])
  } else {
    o = forderv(x, cols, sort=TRUE, retGrp=!physical, reuseSorting=TRUE)
  }
  if (!physical) { # index COULD BE saved from C forderReuseSorting already, but disabled for now
    maybe_reset_index(x, o, cols)
    return(invisible(x))
  }
  if (length(o)) {
    setattr(x,"index",NULL)   # TO DO: reorder existing indexes likely faster than rebuilding again. Allow optionally. Simpler for now to clear. Only when order changes.
    if (verbose) { last.started.at = proc.time() }
    .Call(Creorder,x,o)
    if (verbose) { catf("reorder took %s\n", timetaken(last.started.at)); flush.console() }
  } else {
    if (verbose) catf("x is already ordered by these columns, no need to call reorder\n")
  } # else empty integer() from forderv means x is already ordered by those cols, nothing to do.
  setattr(x,"sorted",cols)
  invisible(x)
}

key = function(x) attr(x, "sorted", exact=TRUE)

indices = function(x, vectors = FALSE) {
  ans = names(attributes(attr(x, "index", exact=TRUE)))
  if (is.null(ans)) return(ans) # otherwise character() gets returned by next line
  ans = gsub("^__","",ans)     # the leading __ is internal only, so remove that in result
  if (isTRUE(vectors))
    ans = strsplit(ans, "__", fixed = TRUE)
  ans
}

getindex = function(x, name) {
  # name can be "col", or "col1__col2", or c("col1","col2")
  ans = attr(attr(x, 'index', exact=TRUE), paste0("__",name,collapse=""), exact=TRUE)
  if (!is.null(ans) && (!is.integer(ans) || (length(ans)!=nrow(x) && length(ans)!=0L))) {
    internal_error("index '%s' exists but is invalid", name)   # nocov
  }
  c(ans) ## drop starts and maxgrpn attributes
}

haskey = function(x) !is.null(key(x))

# reorder a vector based on 'order' (integer)
# to be used in fastorder instead of x[o], but in general, it's better to replace vector subsetting with this..?
# Basic checks that all items of order are in range 1:n with no NAs are now made inside Creorder.
# FOR INTERNAL USE ONLY
setreordervec = function(x, order) .Call(Creorder, x, order)

# sort = sort.int = sort.list = order = is.unsorted = function(...)
#    stopf("Should never be called by data.table internals. Use is.sorted() on vectors, or forder() for lists and vectors.")
# Nice idea, but users might use these in i or j e.g. blocking order caused tests 304 to fail.
# Maybe just a grep through *.R for use of these function internally would be better (TO DO).

# Don't use base::is.unsorted internally, because :
#    1) it uses locale whereas in data.table we control locale sorting independently (C locale currently, but
#       "sorted" attribute will need an extra attribute "locale" so we can check if key's locale is the current locale)
#    2) wrapper needed, used to be :
#       identical(FALSE,is.unsorted(x)) && !(length(x)==1 && is.na(x))
#       where the && was needed to maintain backwards compatibility after r-devel's change of is.unsorted(NA) to FALSE (was NA) [May 2013].
# The others (order, sort.int etc) are turned off to protect ourselves from using them internally, for speed and for
# consistency; e.g., consistent twiddling of numeric/integer64, NA at the beginning of integer, locale ordering of character vectors.

is.sorted = function(x, by=NULL) {
  if (is.list(x)) {
    if (missing(by)) by = seq_along(x)   # wouldn't make sense when x is a vector; hence by=seq_along(x) is not the argument default
    if (is.character(by)) by = chmatch(by, names(x))
  } else {
    if (!missing(by)) stopf("x is vector but 'by' is supplied")
  }
  .Call(Cissorted, x, as.integer(by))
  # Return value of TRUE/FALSE is relied on in [.data.table quite a bit on vectors. Simple. Stick with that (rather than -1/0/+1)
}

maybe_reset_index = function(x, idx, cols) {
  if (isTRUE(getOption("datatable.forder.auto.index"))) return(invisible())
  if (is.null(attr(x, "index", exact=TRUE))) setattr(x, "index", integer())
  setattr(attr(x, "index", exact=TRUE), paste0("__", cols, collapse=""), idx)
  invisible(x)
}

ORDERING_TYPES = c('logical', 'integer', 'double', 'complex', 'character')
forderv = function(x, by=seq_along(x), retGrp=FALSE, retStats=retGrp, sort=TRUE, order=1L, na.last=FALSE, reuseSorting=getOption("datatable.reuse.sorting", NA)) {
  if (is.atomic(x) || is.null(x)) {  # including forderv(NULL) which returns error consistent with base::order(NULL),
    if (!missing(by) && !is.null(by)) stopf("x is a single vector, non-NULL 'by' doesn't make sense")
    by = NULL
  } else {
    if (!length(x)) return(integer(0L)) # e.g. forderv(data.table(NULL)) and forderv(list()) return integer(0L))
    by = colnamesInt(x, by, check_dups=FALSE)
  }
  order = as.integer(order) # length and contents of order being +1/-1 is checked at C level
  .Call(CforderReuseSorting, x, by, retGrp, retStats, sort, order, na.last, reuseSorting)  # returns integer() if already sorted, regardless of sort=TRUE|FALSE
}

forder = function(..., na.last=TRUE, decreasing=FALSE, method="radix")
{
  if (method != "radix") stopf("data.table has no support for sorting by method='%s'. Use base::order(), not order(), if you really need this.", method)
  stopifnot(is.logical(decreasing), length(decreasing) > 0L, !is.na(decreasing))
  sub = substitute(list(...))
  tt = vapply_1b(sub, function(x) is.null(x) || (is.symbol(x) && !nzchar(x)))
  if (any(tt)) sub[tt] = NULL  # remove any NULL or empty arguments; e.g. test 1962.052: forder(DT, NULL) and forder(DT, )
  if (length(sub)<2L) return(NULL)  # forder() with no arguments returns NULL consistent with base::order
  asc = rep.int(1L, length(sub)-1L)  # ascending (1) or descending (-1) per column
  # the idea here is to intercept - (and unusual --+ deriving from built expressions) before vectors in forder(DT, -colA, colB) so that :
  # 1) - on character vector works; ordinarily in R that fails with type error
  # 2) each column/expression can have its own +/- more easily than having to use a separate decreasing=TRUE/FALSE
  # 3) we can pass the decreasing (-) flag to C and avoid what normally happens in R; i.e. allocate a new vector and negate every element first
  # We intercept the unevaluated expressions and massage them before evaluating in with(DT) scope or not depending on the first item.
  for (i in seq.int(2L, length(sub))) {
    v = sub[[i]]
    while (v %iscall% c('-', '+') && length(v)==2L) {
      if (v[[1L]] == "-") asc[i-1L] = -asc[i-1L]
      sub[[i]] = v = v[[2L]]  # remove the leading +/- which is the 2nd item since length(v)==2; i.e. monadic +/-
    }
  }
  x = eval(sub[[2L]], parent.frame(), parent.frame())
  if (is.list(x)) {
    if (length(x)==0L && is.data.frame(x)) stopf("Attempting to order a 0-column data.table or data.frame.")
    sub[2L] = NULL  # change list(DT, ...) to list(...)
    if (length(sub)==1L) {
      data = x
    } else {
      if (!is.data.frame(x)) stopf("The first item passed to [f]order is a plain list but there are more items. It should be a data.table or data.frame.")
      asc = asc[-1L]
      data = eval(sub, x, parent.frame())
    }
  } else {
    data = eval(sub, parent.frame(), parent.frame())
  }
  if (length(decreasing) > 1L) {
    if (any(asc < 0L)) stopf("Mixing '-' with vector decreasing= is not supported.")
    if (length(decreasing) != length(asc)) stopf("decreasing= has length %d applied to sorting %d columns.", length(decreasing), length(asc))
    orderArg = fifelse(decreasing, -asc, asc)
  } else if (decreasing) {
    orderArg = -asc
  } else {
    orderArg = asc
  }
  o = forderv(data, seq_along(data), retGrp=FALSE, retStats=FALSE, sort=TRUE, order=orderArg, na.last=na.last)
  if (!length(o) && length(data)>=1L) o = seq_along(data[[1L]]) else o
  o
}

fsort = function(x, decreasing=FALSE, na.last=FALSE, internal=FALSE, verbose=FALSE, ...)
{
  containsNAs = FALSE
  if (typeof(x)=="double" && !decreasing && !(containsNAs <- anyNA(x))) {
      if (internal) stopf("Internal code should not be being called on type double")
      return(.Call(Cfsort, x, verbose))
  }
  else {
    # fsort is now exported for testing. Trying to head off complaints "it's slow on integer"
    # The only places internally we use fsort internally (3 calls, all on integer) have had internal=TRUE added for now.
    # TODO: implement integer and character in Cfsort and remove this branch and warning
    if (!internal){
      if (typeof(x)!="double") warningf("Input is not a vector of type double. New parallel sort has only been done for double vectors so far. Using one thread.")
      if (decreasing)  warningf("New parallel sort has not been implemented for decreasing=TRUE so far. Using one thread.")
      if (containsNAs) warningf("New parallel sort has not been implemented for vectors containing NA values so far. Using one thread.")
    }
    orderArg = if (decreasing) -1 else 1
    o = forderv(x, order=orderArg, na.last=na.last)
    return( if (length(o)) x[o] else x )
  }
}

setorder = function(x, ..., na.last=FALSE)
# na.last=FALSE here, to be consistent with data.table's default
# as opposed to DT[order(.)] where na.last=TRUE, to be consistent with base
{
  if (!is.data.frame(x)) stopf("x must be a data.frame or data.table")
  cols = substitute(list(...))[-1L]
  if (identical(as.character(cols),"NULL")) return(x)
  if (length(cols)) {
    cols=as.list(cols)
    order=rep(1L, length(cols))
    for (i in seq_along(cols)) {
      v=as.list(cols[[i]])
      if (length(v) > 1L && v[[1L]] == "+") v=v[[-1L]]
      else if (length(v) > 1L && v[[1L]] == "-") {
        v=v[[-1L]]
        order[i] = -1L
      }
      cols[[i]]=as.character(v)
    }
    cols=unlist(cols, use.names=FALSE)
  } else {
    cols=colnames(x)
    order=rep(1L, length(cols))
  }
  setorderv(x, cols, order, na.last)
}

setorderv = function(x, cols = colnames(x), order=1L, na.last=FALSE)
{
  if (is.null(cols)) return(x)
  if (!is.data.frame(x)) stopf("x must be a data.frame or data.table")
  na.last = as.logical(na.last)
  if (is.na(na.last) || !length(na.last)) stopf('na.last must be logical TRUE/FALSE')
  if (!is.character(cols)) stopf("cols is not a character vector. Please see further information in ?setorder.")
  if (!length(cols)) {
    warningf("cols is a character vector of zero length. Use NULL instead, or wrap with suppressWarnings() to avoid this warning.")
    return(x)
  }
  if (!all(nzchar(cols))) stopf("cols contains some blanks.")     # TODO: probably I'm checking more than necessary here.. there are checks in 'forderv' as well
  # remove backticks from cols
  cols = gsub("`", "", cols, fixed = TRUE)
  miss = !(cols %chin% colnames(x))
  if (any(miss)) stopf("some columns are not in the data.table: %s", brackify(cols[miss]))
  if (".xi" %chin% colnames(x)) stopf("x contains a column called '.xi'. Conflicts with internal use by data.table.")
  for (i in cols) {
    .xi = x[[i]]  # [[ is copy on write, otherwise checking type would be copying each column
    if (!typeof(.xi) %chin% ORDERING_TYPES) stopf("Column '%s' is type '%s' which is not supported for ordering currently.", i, typeof(.xi))
  }
  if (!is.character(cols) || length(cols)<1L) internal_error("'cols' should be character at this point") # nocov

  o = forderv(x, cols, sort=TRUE, retGrp=FALSE, order=order, na.last=na.last)
  if (length(o)) {
    .Call(Creorder, x, o)
    if (is.data.frame(x) && !is.data.table(x)) {
      setattr(x, 'row.names', rownames(x)[o])
    }
    k = key(x)
    if (!identical(head(cols, length(k)), k) || any(head(order, length(k)) < 0L))
      setattr(x, 'sorted', NULL) # if 'forderv' is not 0-length & key is not a same-ordered subset of cols, it means order has changed. So, set key to NULL, else retain key.
    setattr(x, 'index', NULL)  # remove secondary keys too. These could be reordered and retained, but simpler and faster to remove
  }
  invisible(x)
}

binary = function(x) .Call(Cbinary, x)

setNumericRounding = function(x) invisible(.Call(CsetNumericRounding, as.integer(x)))
getNumericRounding = function() .Call(CgetNumericRounding)

SJ = function(...) {
  JDT = as.data.table(list(...))
  setkey(JDT)
}
# S for Sorted, usually used in i to sort the i table

# TO DO?: Use the CJ list() replication method for SJ (inside as.data.table.list?, #2109) too to avoid setalloccol

CJ = function(..., sorted = TRUE, unique = FALSE)
{
  # Pass in a list of unique values, e.g. ids and dates
  # Cross Join will then produce a join table with the combination of all values (cross product).
  # The last vector is varied the quickest in the table, so dates should be last for roll for example
  l = list(...)
  vnames = name_dots(...)$vnames
  if (any(tt <- !nzchar(vnames))) vnames[tt] = paste0("V", which(tt))
  dups = FALSE # fix for #1513
  for (i in seq_along(l)) {
    y = l[[i]]
    if (!length(y)) next
    if (sorted) {
      if (!is.atomic(y)) stopf("'sorted' is TRUE but element %d is non-atomic, which can't be sorted; try setting sorted = FALSE", i)
      o = forderv(y, retGrp=TRUE)
      thisdups = attr(o, 'maxgrpn', exact=TRUE)>1L
      if (thisdups) {
        dups = TRUE
        if (length(o)) l[[i]] = if (unique) y[o[attr(o, "starts", exact=TRUE)]] else y[o]
        else if (unique) l[[i]] = y[attr(o, "starts", exact=TRUE)]  # test 1525.5
      } else {
        if (length(o)) l[[i]] = y[o]
      }
    } else {
      if (unique) l[[i]] = unique(y)
    }
  }
  nrow = prod(lengths(l))
  if (nrow > .Machine$integer.max) stopf("Cross product of elements provided to CJ() would result in %.0f rows which exceeds .Machine$integer.max == %d", nrow, .Machine$integer.max)
  l = .Call(Ccj, l)
  setDT(l)
  l = setalloccol(l)  # a tiny bit wasteful to over-allocate a fixed join table (column slots only), doing it anyway for consistency since
                    # it's possible a user may wish to use SJ directly outside a join and would expect consistent over-allocation
  setnames(l, vnames)
  if (sorted) {
    if (!dups) setattr(l, 'sorted', names(l))
    else setkey(l) # fix #1513
  }
  l
}
