as.data.table <-function(x, keep.rownames=FALSE, ...)
{
  if (is.null(x))
    return(null.data.table())
  UseMethod("as.data.table")
}

as.data.table.default <- function(x, ...){
  as.data.table(as.data.frame(x, ...)) # we cannot assume as.data.frame will do copy, thus setDT changed to as.data.table #3230
}

as.data.table.factor <- as.data.table.ordered <-
as.data.table.integer <- as.data.table.numeric <-
as.data.table.logical <- as.data.table.character <-
as.data.table.Date <- as.data.table.ITime <- function(x, keep.rownames=FALSE, ...) {
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
  as.data.table.list(x, FALSE)
}

# as.data.table.table - FR #4848
as.data.table.table <- function(x, keep.rownames=FALSE, ...) {
  # Fix for bug #5408 - order of columns are different when doing as.data.table(with(DT, table(x, y)))
  val = rev(dimnames(provideDimnames(x)))
  if (is.null(names(val)) || !any(nzchar(names(val))))
    setattr(val, 'names', paste0("V", rev(seq_along(val))))
  ans <- data.table(do.call(CJ, c(val, sorted=FALSE)), N = as.vector(x))
  setcolorder(ans, c(rev(head(names(ans), -1L)), "N"))
  ans
}

as.data.table.matrix <- function(x, keep.rownames=FALSE, ...) {
  if (!identical(keep.rownames, FALSE)) {
    # can specify col name to keep.rownames, #575
    ans = data.table(rn=rownames(x), x, keep.rownames=FALSE)
    if (is.character(keep.rownames))
      setnames(ans, 'rn', keep.rownames[1L])
    return(ans)
  }
  d <- dim(x)
  nrows <- d[1L]
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
  if (length(col_labels) == ncols) {
    if (any(empty <- !nzchar(col_labels)))
      col_labels[empty] <- paste0("V", ic[empty])
    setattr(value, "names", col_labels)
  } else {
    setattr(value, "names", paste0("V", ic))
  }
  setattr(value,"row.names",.set_row_names(nrows))
  setattr(value,"class",c("data.table","data.frame"))
  alloc.col(value)
}

# as.data.table.array - #1418
as.data.table.array <- function(x, keep.rownames=FALSE, sorted=TRUE, value.name="value", na.rm=TRUE, ...) {
  dx = dim(x)
  if (length(dx) <= 2L)
    stop("as.data.table.array method should be only called for arrays with 3+ dimensions, for 2 dimensions matrix method should be used")
  if (!is.character(value.name) || length(value.name)!=1L || is.na(value.name) || !nzchar(value.name))
    stop("Argument 'value.name' must be scalar character, non-NA and non zero char")
  if (!is.logical(sorted) || length(sorted)!=1L || is.na(sorted))
    stop("Argument 'sorted' must be scalar logical and non-NA")
  if (!is.logical(na.rm) || length(na.rm)!=1L || is.na(na.rm))
    stop("Argument 'na.rm' must be scalar logical and non-NA")

  dnx = dimnames(x)
  # NULL dimnames will create integer keys, not character as in table method
  val = rev(if (is.null(dnx)) lapply(dim(x), seq.int) else dnx)
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
  if (isTRUE(sorted))
    setkeyv(ans, dims)
  ans[]
}

as.data.table.list <- function(x, keep.rownames=FALSE, ...) {
  n = length(x)
  eachnrow = integer(n)          # vector of lengths of each column. may not be equal if silent repetition is required.
  eachncol = integer(n)
  for (i in seq_len(n)) {
    xi = x[[i]]
    if (is.null(xi)) next    # stop("column or argument ",i," is NULL")
    if ("POSIXlt" %chin% class(xi)) {
      warning("POSIXlt column type detected and converted to POSIXct. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.")
      xi = x[[i]] = as.POSIXct(xi)
    } else if (is.matrix(xi) || is.data.frame(xi)) {  # including data.table (a data.frame, too)
      xi = x[[i]] = as.data.table(xi, keep.rownames=keep.rownames)  # we will never allow a matrix to be a column; always unpack the columns
    } else if (is.table(xi)) {
      xi = x[[i]] = as.data.table.table(xi, keep.rownames=keep.rownames)
    } else if (is.function(xi)) {
      xi = x[[i]] = list(xi)
    }
    eachnrow[i] = NROW(xi)    # for a vector (including list() columns) returns the length
    eachncol[i] = NCOL(xi)    # for a vector returns 1
  }
  ncol = sum(eachncol)
  if (ncol==0L) return(null.data.table())
  nrow = max(eachnrow)
  ans = vector("list",ncol)  # always return a new VECSXP
  recycle = function(x, nrow) {
    if (length(x)==nrow) return(x)
    if (identical(x,list())) vector("list", nrow) else rep(x, length.out=nrow)
  }
  vnames = vector("list", n)
  k = 1L
  for(i in seq_len(n)) {
    xi = x[[i]]
    if (is.null(xi)) next
    nm = names(x)[i]
    if (!length(nm) || is.na(nm)) nm = ""
    if (eachnrow[i]>1L && nrow%%eachnrow[i]!=0L)   # in future: eachnrow[i]!=nrow
      warning("Item ", i, " has ", eachnrow[i], " rows but longest item has ", nrow, "; recycled with remainder.")
    if (is.data.table(xi)) {   # matrix and data.frame were coerced to data.table above
      vnames[[i]] = names(xi)  #if (nm!="" && n>1L) paste(nm, names(xi), sep=".") else names(xi)
      for (j in seq_along(xi)) {
        ans[[k]] = recycle(xi[[j]], nrow)
        k=k+1L
      }
    } else {
      vnames[[i]] = nm
      ans[[k]] = recycle(xi, nrow)
      k=k+1L
    }
  }
  vnames = unlist(vnames)
  w = which(vnames=="")
  if (length(w)) vnames[w] = paste0("V",w)
  setattr(ans,"names",vnames)
  setattr(ans,"row.names",.set_row_names(nrow))
  setattr(ans,"class",c("data.table","data.frame"))
  alloc.col(ans)

  # fix for #842.   Not sure what this is for.  Revisit in PR in 1.12.4 if needed, else remove.
  #if (mn > 0L) {
  #  nz = which(n > 0L)
  #  xx = point(vector("list", length(nz)), seq_along(nz), x, nz)
  #  if (!is.null(names(x)))
  #    setattr(xx, 'names', names(x)[nz])
  #  x = xx
  #}
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

as.data.table.data.frame <- function(x, keep.rownames=FALSE, ...) {
  if (!identical(keep.rownames, FALSE)) {
    # can specify col name to keep.rownames, #575
    ans = data.table(rn=rownames(x), x, keep.rownames=FALSE)
    if (is.character(keep.rownames))
      setnames(ans, 'rn', keep.rownames[1L])
    return(ans)
  }
  if (any(!sapply(x,is.atomic))) {
    # a data.frame with a column that is data.frame needs to be expanded; test 2013.4
    return(as.data.table.list(x, keep.rownames=keep.rownames, ...))
  }
  ans = copy(x)  # TO DO: change this deep copy to be shallow.
  setattr(ans,"row.names",.set_row_names(nrow(x)))

  ## NOTE: This test (#527) is no longer in effect ##
  # for nlme::groupedData which has class c("nfnGroupedData","nfGroupedData","groupedData","data.frame")
  # See test 527.
  ##

  # fix for #1078 and #1128, see .resetclass() for explanation.
  setattr(ans, "class", .resetclass(x, "data.frame"))
  alloc.col(ans)
}

as.data.table.data.table <- function(x, ...) {
  # as.data.table always returns a copy, automatically takes care of #473
  x = copy(x) # #1681
  # fix for #1078 and #1128, see .resetclass() for explanation.
  setattr(x, 'class', .resetclass(x, "data.table"))
  return(x)
}
