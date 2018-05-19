setkey <- function(x, ..., verbose=getOption("datatable.verbose"), physical=TRUE)
{
  if (is.character(x)) stop("x may no longer be the character name of the data.table. The possibility was undocumented and has been removed.")
  cols = as.character(substitute(list(...))[-1L])
  if (!length(cols)) cols=colnames(x)
  else if (identical(cols,"NULL")) cols=NULL
  setkeyv(x, cols, verbose=verbose, physical=physical)
}

# FR #1442
setindex <- function(...) setkey(..., physical=FALSE)
setindexv <- function(x, cols, verbose=getOption("datatable.verbose")) {
  if (is.list(cols)) {
    sapply(cols, setkeyv, x=x, verbose=verbose, physical=FALSE)
    return(invisible(x))
  } else {
    setkeyv(x, cols, verbose=verbose, physical=FALSE)
  }
}

set2key <- function(...) {
  stop("set2key() is now deprecated. Please use setindex() instead.")
}
set2keyv <- function(...) {
  stop("set2keyv() is now deprecated. Please use setindexv() instead.")
}
key2 <- function(x) {
  stop("key2() is now deprecated. Please use indices() instead.")
}

setkeyv <- function(x, cols, verbose=getOption("datatable.verbose"), physical=TRUE)
{
  if (is.null(cols)) {   # this is done on a data.frame when !cedta at top of [.data.table
    if (physical) setattr(x,"sorted",NULL)
    setattr(x,"index",NULL)  # setkey(DT,NULL) also clears secondary keys. setindex(DT,NULL) just clears secondary keys.
    return(invisible(x))
  }
  if (!is.data.table(x)) stop("x is not a data.table")
  if (!is.character(cols)) stop("cols is not a character vector. Please see further information in ?setkey.")
  if (physical && identical(attr(x,".data.table.locked"),TRUE)) stop("Setting a physical key on .SD is reserved for possible future use; to modify the original data's order by group. Try setindex() instead. Or, set*(copy(.SD)) as a (slow) last resort.")
  if (!length(cols)) {
    warning("cols is a character vector of zero length. Removed the key, but use NULL instead, or wrap with suppressWarnings() to avoid this warning.")
    setattr(x,"sorted",NULL)
    return(invisible(x))
  }
  if (identical(cols,"")) stop("cols is the empty string. Use NULL to remove the key.")
  if (!all(nzchar(cols))) stop("cols contains some blanks.")
  if (!length(cols)) {
    cols = colnames(x)   # All columns in the data.table, usually a few when used in this form
  } else {
    # remove backticks from cols
    cols <- gsub("`", "", cols)
    miss = !(cols %in% colnames(x))
    if (any(miss)) stop("some columns are not in the data.table: ", paste(cols[miss], collapse=","))
  }

  ## determine, whether key is already present:
  if (identical(key(x),cols)) {
    if (!physical) {
      ## create index as integer() because already sorted by those columns
      if (is.null(attr(x,"index",exact=TRUE))) setattr(x, "index", integer())
      setattr(attr(x,"index",exact=TRUE), paste0("__", cols, collapse=""), integer())
    }
    return(invisible(x))
  } else if(identical(head(key(x), length(cols)), cols)){
    if (!physical) {
      ## create index as integer() because already sorted by those columns
      if (is.null(attr(x,"index",exact=TRUE))) setattr(x, "index", integer())
      setattr(attr(x,"index",exact=TRUE), paste0("__", cols, collapse=""), integer())
    } else {
      ## key is present but x has a longer key. No sorting needed, only attribute is changed to shorter key.
      setattr(x,"sorted",cols)
    }
    return(invisible(x))
  }

  if (".xi" %chin% names(x)) stop("x contains a column called '.xi'. Conflicts with internal use by data.table.")
  for (i in cols) {
    .xi = x[[i]]  # [[ is copy on write, otherwise checking type would be copying each column
    if (!typeof(.xi) %chin% c("integer","logical","character","double")) stop("Column '",i,"' is type '",typeof(.xi),"' which is not supported as a key column type, currently.")
  }
  if (!is.character(cols) || length(cols)<1L) stop("'cols' should be character at this point in setkey")
  if (verbose) {
    tt = system.time(o <- forderv(x, cols, sort=TRUE, retGrp=FALSE))  # system.time does a gc, so we don't want this always on, until refcnt is on by default in R
    cat("forder took", tt["user.self"]+tt["sys.self"], "sec\n")
  } else {
    o <- forderv(x, cols, sort=TRUE, retGrp=FALSE)
  }
  if (!physical) {
    if (is.null(attr(x,"index",exact=TRUE))) setattr(x, "index", integer())
    setattr(attr(x,"index",exact=TRUE), paste0("__", cols, collapse=""), o)
    return(invisible(x))
  }
  setattr(x,"index",NULL)   # TO DO: reorder existing indexes likely faster than rebuilding again. Allow optionally. Simpler for now to clear.
  if (length(o)) {
    if (verbose) {
      tt = system.time(.Call(Creorder,x,o))
      cat("reorder took", tt["user.self"]+tt["sys.self"], "sec\n")
    } else {
      .Call(Creorder,x,o)
    }
  } else {
    if (verbose) cat("x is already ordered by these columns, no need to call reorder\n")
  } # else empty integer() from forderv means x is already ordered by those cols, nothing to do.
  setattr(x,"sorted",cols)
  invisible(x)
}

key <- function(x) attr(x,"sorted",exact=TRUE)

indices <- function(x, vectors = FALSE) {
  ans = names(attributes(attr(x,"index",exact=TRUE)))
  if (is.null(ans)) return(ans) # otherwise character() gets returned by next line
  ans <- gsub("^__","",ans)
  if (isTRUE(vectors))
    ans <- strsplit(ans, "__", fixed = TRUE)
  ans
}

"key<-" <- function(x,value) {
  warning("The key(x)<-value form of setkey can copy the whole table. This is due to <- in R itself. Please change to setkeyv(x,value) or setkey(x,...) which do not copy and are faster. See help('setkey'). You can safely ignore this warning if it is inconvenient to change right now. Setting options(warn=2) turns this warning into an error, so you can then use traceback() to find and change your key<- calls.")
  setkeyv(x,value)
  # The returned value here from key<- is then copied by R before assigning to x, it seems. That's
  # why we can't do anything about it without a change in R itself. If we return NULL (or invisible()) from this key<-
  # method, the table gets set to NULL. So, although we call setkeyv(x,cols) here, and that doesn't copy, the
  # returned value (x) then gets copied by R.
  # So, solution is that caller has to call setkey or setkeyv directly themselves, to avoid <- dispatch and its copy.
}

haskey <- function(x) !is.null(key(x))

# reverse a vector by reference (no copy)
setrev <- function(x) .Call(Csetrev, x)

# reorder a vector based on 'order' (integer)
# to be used in fastorder instead of x[o], but in general, it's better to replace vector subsetting with this..?
# Basic checks that all items of order are in range 1:n with no NAs are now made inside Creorder.
# FOR INTERNAL USE ONLY
setreordervec <- function(x, order) .Call(Creorder, x, order)

# sort = sort.int = sort.list = order = is.unsorted <- function(...)
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

is.sorted <- function(x, by=seq_along(x)) {
  if (is.list(x)) {
    warning("Use 'if (length(o<-forderv(DT,by))) ...' for efficiency in one step, so you have o as well if not sorted.")
    # could pass through a flag for forderv to return early on first FALSE. But we don't need that internally
    # since internally we always then need ordering, an it's better in one step. Don't want inefficiency to creep in.
    # This is only here for user/debugging use to check/test valid keys; e.g. data.table:::is.sorted(DT,by)
    0L == length(forderv(x,by,retGrp=FALSE,sort=TRUE))
  } else {
    if (!missing(by)) stop("x is vector but 'by' is supplied")
    .Call(Cfsorted, x)
  }
  # Cfsorted could be named CfIsSorted, but since "sorted" is an adjective not verb, it's clear; e.g., Cfsort would sort it ("sort" is verb).
  # Return value of TRUE/FALSE is relied on in [.data.table quite a bit on vectors. Simple. Stick with that (rather than -1/0/+1)
  # Important to call forder.c::fsorted here, for consistent character ordering and numeric/integer64 twiddling.
}

forderv <- function(x, by=seq_along(x), retGrp=FALSE, sort=TRUE, order=1L, na.last=FALSE)
{
  if (!(sort || retGrp)) stop("At least one of retGrp or sort must be TRUE")
  na.last = as.logical(na.last)
  if (!length(na.last)) stop('length(na.last) = 0')
  if (length(na.last) != 1L) {
    warning("length(na.last) > 1, only the first element will be used")
    na.last = na.last[1L]
  }
  # TO DO: export and document forder
  if (is.atomic(x)) {
    if (!missing(by) && !is.null(by)) stop("x is a single vector, non-NULL 'by' doesn't make sense")
    by = NULL
    if ( !missing(order) && (length(order) != 1L || !(order %in% c(1L, -1L))) )
      stop("x is a single vector, length(order) must be =1 and it's value should be 1 (ascending) or -1 (descending).")
  } else {
    if (!length(x)) return(integer(0L)) # to be consistent with base::order. this'll make sure forderv(NULL) will result in error
                       # (as base does) but forderv(data.table(NULL)) and forderv(list()) will return integer(0L))
    if (is.character(by)) {
      w = chmatch(by, names(x))
      if (anyNA(w)) stop("'by' contains '",by[is.na(w)][1],"' which is not a column name")
      by = w
    }
    else if (typeof(by)=="double" && isReallyReal(by)) {
      stop("'by' is type 'double' but one or more items in it are not whole integers")
    }
    by = as.integer(by)
    if ( (length(order) != 1L && length(order) != length(by)) || any(!order %in% c(1L, -1L)) )
      stop("x is a list, length(order) must be either =1 or =length(by) and each value should be 1 or -1 for each column in 'by', corresponding to ascending or descending order, respectively. If length(order) == 1, it will be recycled to length(by).")
    if (length(order) == 1L) order = rep(order, length(by))
  }
  order = as.integer(order)
  .Call(Cforder, x, by, retGrp, sort, order, na.last)  # returns integer() if already sorted, regardless of sort=TRUE|FALSE
}

forder <- function(x, ..., na.last=TRUE, decreasing=FALSE)
{
  if (!is.data.table(x)) stop("x must be a data.table.")
  if (ncol(x) == 0L) stop("Attempting to order a 0-column data.table.")
  if (is.na(decreasing) || !is.logical(decreasing)) stop("'decreasing' must be logical TRUE or FALSE")
  cols = substitute(list(...))[-1L]
  if (identical(as.character(cols),"NULL") || !length(cols)) return(NULL) # to provide the same output as base::order
  ans = x
  order = rep(1L, length(cols))
  if (length(cols)) {
    ans = vector("list", length(cols))
    cols = as.list(cols)
    xcols = names(x)
    for (i in seq_along(cols)) {
      v=cols[[i]]
      if (i == 1L && is.call(v) && length(v) == 2L && v[[1L]] == "list") return(1L) # to be consistent with base, see comment below under while loop
      while (is.call(v) && length(v) == 2L && v[[1L]] != "list") {
        # take care of "--x", "{-x}", "(---+x)" etc., cases and also "list(y)". 'list(y)' is ambiguous though. In base, with(DT, order(x, list(y))) will error
        # that 'arguments are not of same lengths'. But with(DT, order(list(x), list(y))) will return 1L, which is very strange. On top of that, with(DT,
        # order(x, as.list(10:1)) would return 'unimplemented type list'. It's all very inconsistent. But we HAVE to be consistent with base HERE.
        if (!as.character(v[[1L]]) %chin% c("+", "-")) break   # FIX for bug #5583
        if (v[[1L]] == "-") order[i] = -order[i]
        v = v[[-1L]]
      }
      if (is.name(v)) {
        ix <- chmatch(as.character(v), xcols, nomatch=0L)
        if (ix != 0L) ans <- point(ans, i, x, ix) # see 'point' in data.table.R and C-version pointWrapper in assign.c - avoid copies
        else {
          v = as.call(list(as.name("list"), v))
          ans <- point(ans, i, eval(v, x, parent.frame()), 1L)
        }
      } else {
        if (!is.object(eval(v, x, parent.frame()))) {
          v   = as.call(list(as.name("list"), v))
          ans = point(ans, i, eval(v, x, parent.frame()), 1L) # eval has to make a copy here (not due to list(.), but due to ex: "4-5*y"), unavoidable.
        } else ans = point(ans, i, list(unlist(eval(v, x, parent.frame()))), 1L)
      } # else stop("Column arguments to order by in 'forder' should be of type name/symbol (ex: quote(x)) or call (ex: quote(-x), quote(x+5*y))")
    }
  }
  cols = seq_along(ans)
  # Supported column types are checked at C level
  o = forderv(ans, cols, sort=TRUE, retGrp=FALSE, order= if (decreasing) -order else order, na.last)
  if (!length(o)) o = seq_along(ans[[1L]]) else o
  o
}

fsort <- function(x, decreasing = FALSE, na.last = FALSE, internal=FALSE, verbose=FALSE, ...)
{
  containsNAs <- FALSE
  if (typeof(x)=="double" && !decreasing) {
    containsNAs <- anyNA(x) ## just do this if all other conditions are met since it is relatively expensive
  }
  if(typeof(x)=="double" && !decreasing && !containsNAs){
      if (internal) stop("Internal code should not be being called on type double")
      return(.Call(Cfsort, x, verbose))
  }
  else {
    # fsort is now exported for testing. Trying to head off complaints "it's slow on integer"
    # The only places internally we use fsort internally (3 calls, all on integer) have had internal=TRUE added for now.
    # TODO: implement integer and character in Cfsort and remove this branch and warning
    if (!internal){
      if(typeof(x) != "double") warning("Input is not a vector of type double. New parallel sort has only been done for double vectors so far. Using one thread.")
      if(decreasing)  warning("New parallel sort has not been implemented for decreasing=TRUE so far. Using one thread.")
      if(containsNAs) warning("New parallel sort has not been implemented for vectors containing NA values so far. Using one thread.")
    }
    orderArg = if(decreasing) -1 else 1
    o = forderv(x, order=orderArg, na.last=na.last)
    return( if (length(o)) x[o] else x )   # TO DO: document this shortcut for already-sorted
  }
}

setorder <- function(x, ..., na.last=FALSE)
# na.last=FALSE here, to be consistent with data.table's default
# as opposed to DT[order(.)] where na.last=TRUE, to be consistent with base
{
  if (!is.data.frame(x)) stop("x must be a data.frame or data.table.")
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

setorderv <- function(x, cols, order=1L, na.last=FALSE)
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
  if (!length(cols)) {
    cols = colnames(x)   # All columns in the data.table, usually a few when used in this form
  } else {
    # remove backticks from cols
    cols <- gsub("`", "", cols)
    miss = !(cols %in% colnames(x))
    if (any(miss)) stop("some columns are not in the data.table: ", paste(cols[miss], collapse=","))
  }
  if (".xi" %in% colnames(x)) stop("x contains a column called '.xi'. Conflicts with internal use by data.table.")
  for (i in cols) {
    .xi = x[[i]]  # [[ is copy on write, otherwise checking type would be copying each column
    if (!typeof(.xi) %chin% c("integer","logical","character","double")) stop("Column '",i,"' is type '",typeof(.xi),"' which is not supported for ordering currently.")
  }
  if (!is.character(cols) || length(cols)<1L) stop("'cols' should be character at this point in setkey.")

  o = forderv(x, cols, sort=TRUE, retGrp=FALSE, order=order, na.last=na.last)
  if (length(o)) {
    .Call(Creorder, x, o)
    if (is.data.frame(x) & !is.data.table(x)) {
      .Call(Creorder, rn <- rownames(x), o)
      setattr(x, 'row.names', rn)
    }
    setattr(x, 'sorted', NULL) # if 'forderv' is not 0-length, it means order has changed. So, set key to NULL, else retain key.
    setattr(x, 'index', NULL)  # remove secondary keys too. These could be reordered and retained, but simpler and faster to remove
  }
  invisible(x)
}

binary <- function(x) .Call(Cbinary, x)

setNumericRounding <- function(x) {.Call(CsetNumericRounding, as.integer(x)); invisible()}
getNumericRounding <- function() .Call(CgetNumericRounding)

SJ <- function(...) {
  JDT = as.data.table(list(...))
  setkey(JDT)
}
# S for Sorted, usually used in i to sort the i table

# TO DO?: Use the CJ list() replication method for SJ (inside as.data.table.list?, #2109) too to avoid alloc.col

CJ <- function(..., sorted = TRUE, unique = FALSE)
{
  # Pass in a list of unique values, e.g. ids and dates
  # Cross Join will then produce a join table with the combination of all values (cross product).
  # The last vector is varied the quickest in the table, so dates should be last for roll for example
  l = list(...)
  emptyList <- FALSE ## fix for #2511
  if(any(sapply(l, length) == 0L)){
    ## at least one column is empty The whole thing will be empty in the end
    emptyList <- TRUE
    l <- lapply(l, "[", 0L)
  }
  if (unique && !emptyList) l = lapply(l, unique)

  dups = FALSE # fix for #1513
  if (length(l)==1L && !emptyList && sorted && length(o <- forderv(l[[1L]])))
    l[[1L]] = l[[1L]][o]
  else if (length(l) > 1L && !emptyList) {
    # using rep.int instead of rep speeds things up considerably (but attributes are dropped).
    attribs = lapply(l, attributes)  # remember attributes for resetting after rep.int
    n = vapply(l, length, 0L) #lengths(l) will work from R 3.2.0
    nrow = prod(n)
    if (nrow > .Machine$integer.max) {
      stop("Cross product of elements provided to CJ() would result in ",nrow," rows which exceeds .Machine$integer.max == ",.Machine$integer.max)
    }
    x = c(rev(take(cumprod(rev(n)))), 1L)
    for (i in seq_along(x)) {
      y = l[[i]]
      # fix for #1513
      if (sorted) {
        if (length(o <- forderv(y, retGrp=TRUE))) y = y[o]
        if (!dups) dups = attr(o, 'maxgrpn') > 1L
      }
      if (i == 1L)
        l[[i]] = rep.int(y, times = rep.int(x[i], n[i]))   # i.e. rep(y, each=x[i])
      else if (i == length(n))
        l[[i]] = rep.int(y, times = nrow/(x[i]*n[i]))
      else
        l[[i]] = rep.int(rep.int(y, times = rep.int(x[i], n[i])), times = nrow/(x[i]*n[i]))
      if (!is.null(attribs[[i]])){
        attributes(l[[i]]) <- attribs[[i]] # reset all attributes that were destroyed by rep.int
      }
    }
  }
  setattr(l, "row.names", .set_row_names(length(l[[1L]])))
  setattr(l, "class", c("data.table", "data.frame"))

  if (is.null(vnames <- names(l)))
    vnames = vector("character", length(l))
  if (any(tt <- vnames == "")) {
    vnames[tt] = paste0("V", which(tt))
    setattr(l, "names", vnames)
  }
  l <- alloc.col(l)  # a tiny bit wasteful to over-allocate a fixed join table (column slots only), doing it anyway for consistency, and it's possible a user may wish to use SJ directly outside a join and would expect consistent over-allocation.
  if (sorted) {
    if (!dups) setattr(l, 'sorted', names(l))
    else setkey(l) # fix #1513
  }
  l
}
