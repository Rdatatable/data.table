as.data.table <-function(x, keep.rownames=FALSE, key=NULL, ...)
{
  if (is.null(x))
    return(null.data.table())
  UseMethod("as.data.table")
}

as.data.table.default <- function(x, ...){
  as.data.table(as.data.frame(x, ...), ...) # we cannot assume as.data.frame will do copy, thus setDT changed to as.data.table #3230
}

as.data.table.factor <- as.data.table.ordered <-
as.data.table.integer <- as.data.table.numeric <-
as.data.table.logical <- as.data.table.character <-
as.data.table.Date <- as.data.table.ITime <- function(x, keep.rownames=FALSE, key=NULL, ...) {
  if (is.matrix(x)) {
    return(as.data.table.matrix(x, ...))
  }
  tt = deparse(substitute(x))[1L]
  nm = names(x)
  # FR #2356 - transfer names of named vector as "rn" column if required
  if (!identical(keep.rownames, FALSE) & !is.null(nm))
    x <- list(nm, unname(x))
  else x <- list(x)
  if (tt == make.names(tt)) {
    # can specify col name to keep.rownames, #575
    nm = if (length(x) == 2L) if (is.character(keep.rownames)) keep.rownames[1L] else "rn"
    setattr(x, 'names', c(nm, tt))
  }
  as.data.table.list(x, FALSE, key)
}

# as.data.table.table - FR #4848
as.data.table.table <- function(x, keep.rownames=FALSE, key=NULL, ...) {
  # Fix for bug #5408 - order of columns are different when doing as.data.table(with(DT, table(x, y)))
  val = rev(dimnames(provideDimnames(x)))
  if (is.null(names(val)) || !any(nzchar(names(val))))
    setattr(val, 'names', paste0("V", rev(seq_along(val))))
  ans <- data.table(do.call(CJ, c(val, sorted=FALSE)), N = as.vector(x), key=key)
  setcolorder(ans, c(rev(head(names(ans), -1L)), "N"))
  ans
}

as.data.table.matrix <- function(x, keep.rownames=FALSE, key=NULL, ...) {
  if (!identical(keep.rownames, FALSE)) {
    # can specify col name to keep.rownames, #575
    ans = data.table(rn=rownames(x), x, keep.rownames=FALSE)
    if (is.character(keep.rownames))
      setnames(ans, 'rn', keep.rownames[1L])
    return(ans)
  }
  d <- dim(x)
  ncols <- d[2L]
  ic <- seq_len(ncols)
  if (!ncols) return(null.data.table())

  value <- vector("list", ncols)
  if (mode(x) == "character") {
    # fix for #745 - A long overdue SO post: http://stackoverflow.com/questions/17691050/data-table-still-converts-strings-to-factors
    for (i in ic) value[[i]] <- x[, i]                  # <strike>for efficiency.</strike> For consistency - data.table likes and prefers "character"
  }
  else {
    for (i in ic) value[[i]] <- as.vector(x[, i])       # to drop any row.names that would otherwise be retained inside every column of the data.table
  }
  col_labels <- dimnames(x)[[2L]]
  setDT(value)
  if (length(col_labels) == ncols) {
    if (any(empty <- !nzchar(col_labels)))
      col_labels[empty] <- paste0("V", ic[empty])
    setnames(value, col_labels)
  } else {
    setnames(value, paste0("V", ic))
  }
  # setkey now to allow matrix column names as key
  setkeyv(value, key)
  value
}

# as.data.table.array - #1418
as.data.table.array <- function(x, keep.rownames=FALSE, key=NULL, sorted=TRUE, value.name="value", na.rm=TRUE, ...) {
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
  val = rev(if (is.null(dnx)) lapply(dx, seq.int) else dnx)
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

as.data.table.list <- function(x, keep.rownames=FALSE, key=NULL, ...) {
  wn = sapply(x,is.null)
  if (any(wn)) x = x[!wn]
  if (!length(x)) return( null.data.table() )
  # fix for #833, as.data.table.list with matrix/data.frame/data.table as a list element..
  # TODO: move this entire logic (along with data.table() to C
  for (i in seq_along(x)) {
    dims = dim(x[[i]])
    if (!is.null(dims)) {
      ans = do.call("data.table", x)
      setnames(ans, make.unique(names(ans)))
      return(ans)
    }
  }
  n = vapply(x, length, 0L)
  mn = max(n)
  x = copy(x)
  idx = which(n < mn)
  if (length(idx)) {
    for (i in idx) {
      # any is.null(x[[i]]) were removed above, otherwise warning when a list element is NULL
      if (inherits(x[[i]], "POSIXlt")) {
        warning("POSIXlt column type detected and converted to POSIXct. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.")
        x[[i]] = as.POSIXct(x[[i]])
      }
      # Implementing FR #4813 - recycle with warning when nr %% nrows[i] != 0L
      if (!n[i] && mn)
        warning("Item ", i, " is of size 0 but maximum size is ", mn, ", therefore recycled with 'NA'")
      else if (n[i] && mn %% n[i] != 0L)
        warning("Item ", i, " is of size ", n[i], " but maximum size is ", mn, " (recycled leaving a remainder of ", mn%%n[i], " items)")
      x[[i]] = rep(x[[i]], length.out=mn)
    }
  }
  # fix for #842
  if (mn > 0L) {
    nz = which(n > 0L)
    xx = point(vector("list", length(nz)), seq_along(nz), x, nz)
    if (!is.null(names(x)))
      setattr(xx, 'names', names(x)[nz])
    x = xx
  }
  setDT(x, key=key) # copy ensured above
  if (is.null(names(x))) setnames(x, paste0("V", seq_len(length(x))))
  x
}

# don't retain classes before "data.frame" while converting
# from it.. like base R does. This'll break test #527 (see
# tests and as.data.table.data.frame) I've commented #527
# for now. This addresses #1078 and #1128
.resetclass <- function(x, class) {
  if (length(class)!=1L)
    stop("class must be length 1") # nocov
  cx = class(x)
  n  = chmatch(class, cx)   # chmatch accepts legth(class)>1 but next line requires length(n)==1
  unique( c("data.table", "data.frame", tail(cx, length(cx)-n)) )
}

as.data.table.data.frame <- function(x, keep.rownames=FALSE, key=NULL, ...) {
  if (!identical(keep.rownames, FALSE)) {
    # can specify col name to keep.rownames, #575
    ans = data.table(rn=rownames(x), x, keep.rownames=FALSE, key=key)
    if (is.character(keep.rownames))
      setnames(ans, 'rn', keep.rownames[1L])
    return(ans)
  }
  ans = copy(x)  # TO DO: change this deep copy to be shallow.
  setattr(ans, "row.names", .set_row_names(nrow(x)))

  ## NOTE: This test (#527) is no longer in effect ##
  # for nlme::groupedData which has class c("nfnGroupedData","nfGroupedData","groupedData","data.frame")
  # See test 527.
  ##

  # fix for #1078 and #1128, see .resetclass() for explanation.
  setattr(ans, "class", .resetclass(x, "data.frame"))
  alloc.col(ans)
  setkeyv(ans, key)
  ans
}

as.data.table.data.table <- function(x, ...) {
  # as.data.table always returns a copy, automatically takes care of #473
  x = copy(x) # #1681
  # fix for #1078 and #1128, see .resetclass() for explanation.
  setattr(x, 'class', .resetclass(x, "data.table"))
  return(x)
}
