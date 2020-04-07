as.data.table = function(x, keep.rownames=FALSE, ...)
# cannot add new args before dots otherwise revdeps which implement their methods will start to warn; e.g. riskRegression in #3581
{
  if (is.null(x))
    return(null.data.table())
  UseMethod("as.data.table")
}

as.data.table.default = function(x, ...){
  as.data.table(as.data.frame(x, ...), ...) # we cannot assume as.data.frame will do copy, thus setDT changed to as.data.table #3230
}

as.data.table.factor = as.data.table.ordered =
as.data.table.integer = as.data.table.numeric =
as.data.table.logical = as.data.table.character =
as.data.table.Date = as.data.table.ITime = function(x, keep.rownames=FALSE, key=NULL, ...) {
  if (is.matrix(x)) {
    return(as.data.table.matrix(x, ...))
  }
  tt = deparse(substitute(x))[1L]
  nm = names(x)
  # FR #2356 - transfer names of named vector as "rn" column if required
  if (!identical(keep.rownames, FALSE) & !is.null(nm))
    x = list(nm, unname(x))
  else x = list(x)
  if (tt == make.names(tt)) {
    # can specify col name to keep.rownames, #575
    nm = if (length(x) == 2L) if (is.character(keep.rownames)) keep.rownames[1L] else "rn"
    setattr(x, 'names', c(nm, tt))
  }
  as.data.table.list(x, FALSE, key)
}

# as.data.table.table - FR #361
as.data.table.table = function(x, keep.rownames=FALSE, key=NULL, ...) {
  # Fix for bug #43 - order of columns are different when doing as.data.table(with(DT, table(x, y)))
  val = rev(dimnames(provideDimnames(x)))
  if (is.null(names(val)) || !any(nzchar(names(val))))
    setattr(val, 'names', paste0("V", rev(seq_along(val))))
  ans = data.table(do.call(CJ, c(val, sorted=FALSE)), N = as.vector(x), key=key)
  setcolorder(ans, c(rev(head(names(ans), -1L)), "N"))
  ans
}

as.data.table.matrix = function(x, keep.rownames=FALSE, key=NULL, ...) {
  if (!identical(keep.rownames, FALSE)) {
    # can specify col name to keep.rownames, #575
    ans = data.table(rn=rownames(x), x, keep.rownames=FALSE)
    if (is.character(keep.rownames))
      setnames(ans, 'rn', keep.rownames[1L])
    return(ans)
  }
  d = dim(x)
  ncols = d[2L]
  ic = seq_len(ncols)
  if (!ncols) return(null.data.table())

  value = vector("list", ncols)
  if (mode(x) == "character") {
    # fix for #745 - A long overdue SO post: http://stackoverflow.com/questions/17691050/data-table-still-converts-strings-to-factors
    for (i in ic) value[[i]] = x[, i]                  # <strike>for efficiency.</strike> For consistency - data.table likes and prefers "character"
  }
  else {
    for (i in ic) value[[i]] <- as.vector(x[, i])       # to drop any row.names that would otherwise be retained inside every column of the data.table
  }
  col_labels = dimnames(x)[[2L]]
  setDT(value)
  if (length(col_labels) == ncols) {
    if (any(empty <- !nzchar(col_labels)))
      col_labels[empty] = paste0("V", ic[empty])
    setnames(value, col_labels)
  } else {
    setnames(value, paste0("V", ic))
  }
  # setkey now to allow matrix column names as key
  setkeyv(value, key)
  value
}

# as.data.table.array - #1418
as.data.table.array = function(x, keep.rownames=FALSE, key=NULL, sorted=TRUE, value.name="value", na.rm=TRUE, ...) {
  dx = dim(x)
  if (length(dx) <= 2L)
    stop("as.data.table.array method should only be called for arrays with 3+ dimensions; use the matrix method for 2-dimensional arrays")
  if (!is.character(value.name) || length(value.name)!=1L || is.na(value.name) || !nzchar(value.name))
    stop("Argument 'value.name' must be scalar character, non-NA and at least one character")
  if (!is.logical(sorted) || length(sorted)!=1L || is.na(sorted))
    stop("Argument 'sorted' must be scalar logical and non-NA")
  if (!is.logical(na.rm) || length(na.rm)!=1L || is.na(na.rm))
    stop("Argument 'na.rm' must be scalar logical and non-NA")
  if (!missing(sorted) && !is.null(key))
    stop("Please provide either 'key' or 'sorted', but not both.")

  dnx = dimnames(x)
  # NULL dimnames will create integer keys, not character as in table method
  val = if (is.null(dnx)) {
    lapply(dx, seq.int)
  } else if (any(nulldnx<-sapply(dnx, is.null))) {
    dnx[nulldnx] = lapply(dx[nulldnx], seq.int) #3636
    dnx
  } else dnx
  val = rev(val)
  if (is.null(names(val)) || all(!nzchar(names(val))))
    setattr(val, 'names', paste0("V", rev(seq_along(val))))
  if (value.name %chin% names(val))
    stop("Argument 'value.name' should not overlap with column names in result: ", brackify(rev(names(val))))
  N = NULL
  ans = data.table(do.call(CJ, c(val, sorted=FALSE)), N=as.vector(x))
  if (isTRUE(na.rm))
    ans = ans[!is.na(N)]
  setnames(ans, "N", value.name)
  dims = rev(head(names(ans), -1L))
  setcolorder(ans, c(dims, value.name))
  if (isTRUE(sorted) && is.null(key)) key = dims
  setkeyv(ans, key)
  ans[]
}

as.data.table.list = function(x,
  keep.rownames=FALSE,
  key=NULL,
  check.names=FALSE,
  .named=NULL,  # (internal) whether the argument was named in the data.table() or cbind() call calling this as.data.table.list()
                # e.g. cbind(foo=DF1, bar=DF2) have .named=c(TRUE,TRUE) due to the foo= and bar= and trigger "prefix." for non-vector items
  ...)
{
  n = length(x)
  eachnrow = integer(n)          # vector of lengths of each column. may not be equal if silent repetition is required.
  eachncol = integer(n)
  missing.check.names = missing(check.names)
  origListNames = if (missing(.named)) names(x) else NULL  # as.data.table called directly, not from inside data.table() which provides .named, #3854
  for (i in seq_len(n)) {
    xi = x[[i]]
    if (is.null(xi)) next    # eachncol already initialized to 0 by integer() above
    if (!is.null(dim(xi)) && missing.check.names) check.names=TRUE
    if ("POSIXlt" %chin% class(xi)) {
      warning("POSIXlt column type detected and converted to POSIXct. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.")
      xi = x[[i]] = as.POSIXct(xi)
    } else if (is.matrix(xi) || is.data.frame(xi)) {
      if (!is.data.table(xi)) {
        xi = x[[i]] = as.data.table(xi, keep.rownames=keep.rownames)  # we will never allow a matrix to be a column; always unpack the columns
      }
      # else avoid dispatching to as.data.table.data.table (which exists and copies)
    } else if (is.table(xi)) {
      xi = x[[i]] = as.data.table.table(xi, keep.rownames=keep.rownames)
    } else if (is.function(xi)) {
      xi = x[[i]] = list(xi)
    }
    eachnrow[i] = NROW(xi)    # for a vector (including list() columns) returns the length
    eachncol[i] = NCOL(xi)    # for a vector returns 1
  }
  ncol = sum(eachncol)  # hence removes NULL items silently (no error or warning), #842.
  if (ncol==0L) return(null.data.table())
  nrow = max(eachnrow)
  ans = vector("list",ncol)  # always return a new VECSXP
  recycle = function(x, nrow) {
    if (length(x)==nrow) {
      return(copy(x))
      # This copy used to be achieved via .Call(CcopyNamedInList,x) at the top of data.table(). It maintains pre-Rv3.1.0
      # behavior, for now. See test 548.2. The copy() calls duplicate() at C level which (importantly) also expands ALTREP objects.
      # TODO: port this as.data.table.list() to C and use MAYBE_REFERENCED(x) || ALTREP(x) to save some copies.
      #       That saving used to be done by CcopyNamedInList but the copies happened again as well, so removing CcopyNamedInList is
      #       not worse than before, and gets us in a better centralized place to port as.data.table.list to C and use MAYBE_REFERENCED
      #       again in future, for #617.
    }
    if (identical(x,list())) vector("list", nrow) else rep(x, length.out=nrow)   # new objects don't need copy
  }
  vnames = character(ncol)
  k = 1L
  n_null = 0L
  for(i in seq_len(n)) {
    xi = x[[i]]
    if (is.null(xi)) { n_null = n_null+1L; next }
    if (eachnrow[i]>1L && nrow%%eachnrow[i]!=0L)   # in future: eachnrow[i]!=nrow
      warning("Item ", i, " has ", eachnrow[i], " rows but longest item has ", nrow, "; recycled with remainder.")
    if (eachnrow[i]==0L && nrow>0L && is.atomic(xi))   # is.atomic to ignore list() since list() is a common way to initialize; let's not insist on list(NULL)
      warning("Item ", i, " has 0 rows but longest item has ", nrow, "; filled with NA")  # the rep() in recycle() above creates the NA vector
    if (is.data.table(xi)) {   # matrix and data.frame were coerced to data.table above
      prefix = if (!isFALSE(.named[i]) && isTRUE(nchar(names(x)[i])>0L)) paste0(names(x)[i],".") else ""  # test 2058.12
      for (j in seq_along(xi)) {
        ans[[k]] = recycle(xi[[j]], nrow)
        vnames[k] = paste0(prefix, names(xi)[j])
        k = k+1L
      }
    } else {
      nm = names(x)[i]
      vnames[k] = if (length(nm) && !is.na(nm) && nm!="") nm else paste0("V",i-n_null)  # i (not k) tested by 2058.14 to be the same as the past for now
      ans[[k]] = recycle(xi, nrow)
      k = k+1L
    }
  }
  if (any(vnames==".SD")) stop("A column may not be called .SD. That has special meaning.")
  if (check.names) vnames = make.names(vnames, unique=TRUE)
  setattr(ans, "names", vnames)
  setDT(ans, key=key) # copy ensured above; also, setDT handles naming
  if (length(origListNames)==length(ans)) setattr(ans, "names", origListNames)  # PR 3854 and tests 2058.15-17
  ans
}

# don't retain classes before "data.frame" while converting
# from it.. like base R does. This'll break test #527 (see
# tests and as.data.table.data.frame) I've commented #527
# for now. This addresses #1078 and #1128
.resetclass = function(x, class) {
  if (length(class)!=1L)
    stop("class must be length 1") # nocov
  cx = class(x)
  n  = chmatch(class, cx)   # chmatch accepts length(class)>1 but next line requires length(n)==1
  unique( c("data.table", "data.frame", tail(cx, length(cx)-n)) )
}

as.data.table.data.frame = function(x, keep.rownames=FALSE, key=NULL, ...) {
  if (!identical(keep.rownames, FALSE)) {
    # can specify col name to keep.rownames, #575
    ans = data.table(rn=rownames(x), x, keep.rownames=FALSE, key=key)
    if (is.character(keep.rownames))
      setnames(ans, 'rn', keep.rownames[1L])
    return(ans)
  }
  if (any(vapply_1i(x, function(xi) length(dim(xi))))) { # not is.atomic because is.atomic(matrix) is true
    # a data.frame with a column that is data.frame needs to be expanded; test 2013.4
    return(as.data.table.list(x, keep.rownames=keep.rownames, ...))
  }
  ans = copy(x)  # TO DO: change this deep copy to be shallow.
  setattr(ans, "row.names", .set_row_names(nrow(x)))

  ## NOTE: This test (#527) is no longer in effect ##
  # for nlme::groupedData which has class c("nfnGroupedData","nfGroupedData","groupedData","data.frame")
  # See test 527.
  ##

  # fix for #1078 and #1128, see .resetclass() for explanation.
  setattr(ans, "class", .resetclass(x, "data.frame"))
  setalloccol(ans)
  setkeyv(ans, key)
  ans
}

as.data.table.data.table = function(x, ...) {
  # as.data.table always returns a copy, automatically takes care of #473
  if (any(vapply_1i(x, function(xi) length(dim(xi))))) { # for test 2089.2
    return(as.data.table.list(x, ...))
  }
  x = copy(x) # #1681
  # fix for #1078 and #1128, see .resetclass() for explanation.
  setattr(x, 'class', .resetclass(x, "data.table"))
  return(x)
}
