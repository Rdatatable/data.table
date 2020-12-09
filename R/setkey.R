setkey = function(x, ..., verbose=getOption("datatable.verbose"), physical=TRUE)
{
  if (is.character(x)) stop("x may no longer be the character name of the data.table. The possibility was undocumented and has been removed.")
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

# upgrade to error after Mar 2020. Has already been warning since 2012, and stronger warning in Mar 2019 (note in news for 1.12.2); #3399
"key<-" = function(x,value) {
  warning("key(x)<-value is deprecated and not supported. Please change to use setkey() with perhaps copy(). Has been warning since 2012 and will be an error in future.")
  setkeyv(x,value)
  # The returned value here from key<- is then copied by R before assigning to x, it seems. That's
  # why we can't do anything about it without a change in R itself. If we return NULL (or invisible()) from this key<-
  # method, the table gets set to NULL. So, although we call setkeyv(x,cols) here, and that doesn't copy, the
  # returned value (x) then gets copied by R.
  # So, solution is that caller has to call setkey or setkeyv directly themselves, to avoid <- dispatch and its copy.
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
  if (!is.data.table(x)) stop("x is not a data.table")
  if (!is.character(cols)) stop("cols is not a character vector. Please see further information in ?setkey.")
  if (physical && .Call(C_islocked, x)) stop("Setting a physical key on .SD is reserved for possible future use; to modify the original data's order by group. Try setindex() instead. Or, set*(copy(.SD)) as a (slow) last resort.")
  if (!length(cols)) {
    warning("cols is a character vector of zero length. Removed the key, but use NULL instead, or wrap with suppressWarnings() to avoid this warning.")
    setattr(x,"sorted",NULL)
    return(invisible(x))
  }
  if (identical(cols,"")) stop("cols is the empty string. Use NULL to remove the key.")
  if (!all(nzchar(cols))) stop("cols contains some blanks.")
  cols = gsub("`", "", cols, fixed = TRUE)
  miss = !(cols %chin% colnames(x))
  if (any(miss)) stop("some columns are not in the data.table: ", paste(cols[miss], collapse=","))

  ## determine, whether key is already present:
  if (identical(key(x),cols)) {
    if (!physical) {
      ## create index as integer() because already sorted by those columns
      if (is.null(attr(x, "index", exact=TRUE))) setattr(x, "index", integer())
      setattr(attr(x, "index", exact=TRUE), paste0("__", cols, collapse=""), integer())
    }
    return(invisible(x))
  } else if(identical(head(key(x), length(cols)), cols)){
    if (!physical) {
      ## create index as integer() because already sorted by those columns
      if (is.null(attr(x, "index", exact=TRUE))) setattr(x, "index", integer())
      setattr(attr(x, "index", exact=TRUE), paste0("__", cols, collapse=""), integer())
    } else {
      ## key is present but x has a longer key. No sorting needed, only attribute is changed to shorter key.
      setattr(x,"sorted",cols)
    }
    return(invisible(x))
  }

  if (".xi" %chin% names(x)) stop("x contains a column called '.xi'. Conflicts with internal use by data.table.")
  for (i in cols) {
    .xi = x[[i]]  # [[ is copy on write, otherwise checking type would be copying each column
    if (!typeof(.xi) %chin% ORDERING_TYPES) stop("Column '",i,"' is type '",typeof(.xi),"' which is not supported as a key column type, currently.")
  }
  if (!is.character(cols) || length(cols)<1L) stop("Internal error. 'cols' should be character at this point in setkey; please report.") # nocov

  newkey = paste0(cols, collapse="__")
  if (!any(indices(x) == newkey)) {
    if (verbose) {
      tt = suppressMessages(system.time(o <- forderv(x, cols, sort=TRUE, retGrp=FALSE)))  # system.time does a gc, so we don't want this always on, until refcnt is on by default in R
      # suppress needed for tests 644 and 645 in verbose mode
      cat("forder took", tt["user.self"]+tt["sys.self"], "sec\n")
    } else {
      o = forderv(x, cols, sort=TRUE, retGrp=FALSE)
    }
  } else {
    if (verbose) cat("setkey on columns ", brackify(cols), " using existing index '", newkey, "'\n", sep="")
    o = getindex(x, newkey)
  }
  if (!physical) {
    if (is.null(attr(x, "index", exact=TRUE))) setattr(x, "index", integer())
    setattr(attr(x, "index", exact=TRUE), paste0("__", cols, collapse=""), o)
    return(invisible(x))
  }
  setattr(x,"index",NULL)   # TO DO: reorder existing indexes likely faster than rebuilding again. Allow optionally. Simpler for now to clear.
  if (length(o)) {
    if (verbose) { last.started.at = proc.time() }
    .Call(Creorder,x,o)
    if (verbose) { cat("reorder took", timetaken(last.started.at), "\n"); flush.console() }
  } else {
    if (verbose) cat("x is already ordered by these columns, no need to call reorder\n")
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
    stop("Internal error: index '",name,"' exists but is invalid")   # nocov
  }
  ans
}

haskey = function(x) !is.null(key(x))

# reorder a vector based on 'order' (integer)
# to be used in fastorder instead of x[o], but in general, it's better to replace vector subsetting with this..?
# Basic checks that all items of order are in range 1:n with no NAs are now made inside Creorder.
# FOR INTERNAL USE ONLY
setreordervec = function(x, order) .Call(Creorder, x, order)

# sort = sort.int = sort.list = order = is.unsorted = function(...)
#    stop("Should never be called by data.table internals. Use is.sorted() on vectors, or forder() for lists and vectors.")
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
    if (!missing(by)) stop("x is vector but 'by' is supplied")
  }
  .Call(Cissorted, x, as.integer(by))
  # Return value of TRUE/FALSE is relied on in [.data.table quite a bit on vectors. Simple. Stick with that (rather than -1/0/+1)
}

ORDERING_TYPES = c('logical', 'integer', 'double', 'complex', 'character')
forderv = function(x, by=seq_along(x), retGrp=FALSE, sort=TRUE, order=1L, na.last=FALSE)
{
  if (is.atomic(x)) {  # including forderv(NULL) which returns error consistent with base::order(NULL),
    if (!missing(by) && !is.null(by)) stop("x is a single vector, non-NULL 'by' doesn't make sense")
    by = NULL
  } else {
    if (!length(x)) return(integer(0L)) # e.g. forderv(data.table(NULL)) and forderv(list()) return integer(0L))
    by = colnamesInt(x, by, check_dups=FALSE)
    if (length(order) == 1L) order = rep(order, length(by))
  }
  order = as.integer(order) # length and contents of order being +1/-1 is checked at C level
  .Call(Cforder, x, by, retGrp, sort, order, na.last)  # returns integer() if already sorted, regardless of sort=TRUE|FALSE
}

forder = function(..., na.last=TRUE, decreasing=FALSE)
{
  sub = substitute(list(...))
  tt = sapply(sub, function(x) is.null(x) || (is.symbol(x) && !nzchar(x)))
  if (any(tt)) sub[tt] = NULL  # remove any NULL or empty arguments; e.g. test 1962.052: forder(DT, NULL) and forder(DT, )
  if (length(sub)<2L) return(NULL)  # forder() with no arguments returns NULL consistent with base::order
  asc = rep.int(1L, length(sub)-1L)  # ascending (1) or descending (-1) per column
  # the idea here is to intercept - (and unusual --+ deriving from built expressions) before vectors in forder(DT, -colA, colB) so that :
  # 1) - on character vector works; ordinarily in R that fails with type error
  # 2) each column/expression can have its own +/- more easily that having to use a separate decreasing=TRUE/FALSE
  # 3) we can pass the decreasing (-) flag to C and avoid what normally happens in R; i.e. allocate a new vector and apply - to every element first
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
    if (length(x)==0L && is.data.frame(x)) stop("Attempting to order a 0-column data.table or data.frame.")
    sub[2L] = NULL  # change list(DT, ...) to list(...)
    if (length(sub)==1L) {
      data = x
    } else {
      if (!is.data.frame(x)) stop("The first item passed to [f]order is a plain list but there are more items. It should be a data.table or data.frame.")
      asc = asc[-1L]
      data = eval(sub, x, parent.frame())
    }
  } else {
    data = eval(sub, parent.frame(), parent.frame())
  }
  stopifnot(isTRUEorFALSE(decreasing))
  o = forderv(data, seq_along(data), sort=TRUE, retGrp=FALSE, order= if (decreasing) -asc else asc, na.last)
  if (!length(o) && length(data)>=1L) o = seq_along(data[[1L]]) else o
  o
}

fsort = function(x, decreasing=FALSE, na.last=FALSE, internal=FALSE, verbose=FALSE, ...)
{
  containsNAs = FALSE
  if (typeof(x)=="double" && !decreasing && !(containsNAs <- anyNA(x))) {
      if (internal) stop("Internal code should not be being called on type double")
      return(.Call(Cfsort, x, verbose))
  }
  else {
    # fsort is now exported for testing. Trying to head off complaints "it's slow on integer"
    # The only places internally we use fsort internally (3 calls, all on integer) have had internal=TRUE added for now.
    # TODO: implement integer and character in Cfsort and remove this branch and warning
    if (!internal){
      if (typeof(x)!="double") warning("Input is not a vector of type double. New parallel sort has only been done for double vectors so far. Using one thread.")
      if (decreasing)  warning("New parallel sort has not been implemented for decreasing=TRUE so far. Using one thread.")
      if (containsNAs) warning("New parallel sort has not been implemented for vectors containing NA values so far. Using one thread.")
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
  if (!is.data.frame(x)) stop("x must be a data.frame or data.table")
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
  if (!is.data.frame(x)) stop("x must be a data.frame or data.table")
  na.last = as.logical(na.last)
  if (is.na(na.last) || !length(na.last)) stop('na.last must be logical TRUE/FALSE')
  if (!is.character(cols)) stop("cols is not a character vector. Please see further information in ?setorder.")
  if (!length(cols)) {
    warning("cols is a character vector of zero length. Use NULL instead, or wrap with suppressWarnings() to avoid this warning.")
    return(x)
  }
  if (!all(nzchar(cols))) stop("cols contains some blanks.")     # TODO: probably I'm checking more than necessary here.. there are checks in 'forderv' as well
  # remove backticks from cols
  cols = gsub("`", "", cols, fixed = TRUE)
  miss = !(cols %chin% colnames(x))
  if (any(miss)) stop("some columns are not in the data.table: ", paste(cols[miss], collapse=","))
  if (".xi" %chin% colnames(x)) stop("x contains a column called '.xi'. Conflicts with internal use by data.table.")
  for (i in cols) {
    .xi = x[[i]]  # [[ is copy on write, otherwise checking type would be copying each column
    if (!typeof(.xi) %chin% ORDERING_TYPES) stop("Column '",i,"' is type '",typeof(.xi),"' which is not supported for ordering currently.")
  }
  if (!is.character(cols) || length(cols)<1L) stop("Internal error. 'cols' should be character at this point in setkey; please report.") # nocov

  o = forderv(x, cols, sort=TRUE, retGrp=FALSE, order=order, na.last=na.last)
  if (length(o)) {
    .Call(Creorder, x, o)
    if (is.data.frame(x) & !is.data.table(x)) {
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

setNumericRounding = function(x) {.Call(CsetNumericRounding, as.integer(x)); invisible()}
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
  if (isFALSE(getOption("datatable.CJ.names", TRUE))) {  # default TRUE from v1.12.0, FALSE before. TODO: remove option in v1.13.0 as stated in news
    if (is.null(vnames <- names(l))) vnames = paste0("V", seq_len(length(l)))
    else if (any(tt <- vnames=="")) vnames[tt] = paste0("V", which(tt))
  } else {
    vnames = name_dots(...)$vnames
    if (any(tt <- vnames=="")) vnames[tt] = paste0("V", which(tt))
  }
  dups = FALSE # fix for #1513
  for (i in seq_along(l)) {
    y = l[[i]]
    if (!length(y)) next
    if (sorted) {
      if (!is.atomic(y)) stop("'sorted' is TRUE but element ", i, " is non-atomic, which can't be sorted; try setting sorted = FALSE")
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
  nrow = prod( vapply_1i(l, length) )  # lengths(l) will work from R 3.2.0
  if (nrow > .Machine$integer.max) stop(gettextf("Cross product of elements provided to CJ() would result in %.0f rows which exceeds .Machine$integer.max == %d", nrow, .Machine$integer.max, domain='R-data.table'))
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

