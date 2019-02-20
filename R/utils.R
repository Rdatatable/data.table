# all non-exported / unused internal (utility) functions

# R 3.5.0 made isTRUE inefficient :
#   `is.logical(x) && length(x)==1L && !is.na(x) && x`
# Before R 3.5.0, isTRUE was defined as simply:
#   identical(TRUE,x)
# which was simpler. The new definition uses 8 allocations of the length-1 intermediaries. Probably doesn't matter but might sometimes.
# It was changed in R so that isTRUE(c(a=TRUE)) returned TRUE: https://github.com/wch/r-source/commit/828997ac6ecfb73aaa0aae9d1d0584a4ffc50881#diff-b41e3f9f1d389bb6f7a842cd5a3308b8
# Which we don't need. We prefer simplicity and efficiency of identical().
isTRUE        <- function(x) identical(x,TRUE)  # just as base::isTRUE before R 3.5.0. Masks base::isTRUE for internal use.
isFALSE       <- function(x) identical(x,FALSE) # provide isFALSE before it was added in R 3.5.0; e.g. R 3.1.0 support
isTRUEorNA    <- function(x) identical(x,TRUE) || identical(x,NA)  # also ensures x must be logical length 1
isTRUEorFALSE <- function(x) identical(x,TRUE) || identical(x,FALSE)

# which.first
which.first <- function(x)
{
  if (!is.logical(x)) {
    stop("x not boolean")
  }
  match(TRUE, x)
}

# which.last
which.last <- function(x)
{
  if (!is.logical(x)) {
    stop("x not boolean")
  }
  length(x) - match(TRUE, rev(x)) + 1L
}

# trim
trim <- function(x) {
  # Removes whitespace at the beginning and end of strings
  # Assigning to x[] to retain the original dimensions, rownames and colnames
  x[] = gsub(" +$", "", x)
  x[] = gsub("^ +", "", x)
  x
}

# take (I don't see it being used anywhere)
take <- function(x, n=1L)
{
  # returns the head of head, without the last n observations
  # convenient when inlining expressions
  # NROW, for vectors, returns the vector length, but we cater for non-table like lists also here
  # TO DO: allow taking along any dimension e.g. all but last column, rather than all but last row.
  if (is.list(x) && !is.data.frame(x)  && !is.data.table(x)) l = length(x)
  else l = NROW(x)
  if (l < n) stop("Cannot take ",n," from ",l)
  head(x, l-n)
}
# TODO: Implement take as UseMethod. Specific methods for each type.

require_bit64 = function() {
  # called in fread and print when they see integer64 columns are present
  if (!requireNamespace("bit64",quietly=TRUE))
  warning("Some columns are type 'integer64' but package bit64 is not installed. Those columns will print as strange looking floating point data. There is no need to reload the data. Simply install.packages('bit64') to obtain the integer64 print method and print the data again.")
}

# vapply for return value character(1)
vapply_1c <- function (x, fun, ..., use.names = TRUE) {
  vapply(X = x, FUN = fun, ..., FUN.VALUE = NA_character_, USE.NAMES = use.names)
}

# vapply for return value logical(1)
vapply_1b <- function (x, fun, ..., use.names = TRUE) {
  vapply(X = x, FUN = fun, ..., FUN.VALUE = NA, USE.NAMES = use.names)
}

# vapply for return value integer(1)
vapply_1i <- function (x, fun, ..., use.names = TRUE) {
  vapply(X = x, FUN = fun, ..., FUN.VALUE = NA_integer_, USE.NAMES = use.names)
}

more = function(f) system(paste("more",f))    # nocov  (just a dev helper)

# helper used to auto-name columns in data.table(x,y) as c("x","y"), CJ(x,y) and similar
# naming of unnested matrices still handled by data.table()
name_dots <- function(...) {
  dot_sub <- as.list(substitute(list(...)))[-1L]
  vnames = names(dot_sub)
  if (is.null(vnames)) {
    vnames = rep.int("", length(dot_sub))
    novname = rep.int(TRUE, length(dot_sub))
  } else {
    vnames[is.na(vnames)] = ""
    if (any(vnames==".SD")) stop("A column may not be called .SD. That has special meaning.")
    novname = vnames==""
  }
  for (i in which(novname)) {
    if ((tmp <- deparse(dot_sub[[i]])[1L]) == make.names(tmp))
      vnames[i] = tmp
  }
  still_empty = vnames==""
  if (any(still_empty)) vnames[still_empty] = paste0("V", which(still_empty))
  list(vnames=vnames, novname=novname)
}

# convert a vector like c(1, 4, 3, 2) into a string like [1, 4, 3, 2]
#   (common aggregation method for error messages)
brackify = function(x) {
  # arbitrary cutoff
  if (length(x) > 10L) x = c(x[1:10], '...')
  sprintf('[%s]', paste(x, collapse = ', '))
}

# patterns done via NSE in melt.data.table and .SDcols in `[.data.table`
do_patterns = function(pat_sub, all_cols) {
  # received as substitute(patterns(...))
  pat_sub = as.list(pat_sub)[-1L]
  # identify cols = argument if present
  idx = which(names(pat_sub) == "cols")
  if (length(idx)) {
    cols = eval(pat_sub[["cols"]], parent.frame(2L))
    pat_sub = pat_sub[-idx]
  } else cols = all_cols
  pats = lapply(pat_sub, eval, parent.frame(2L))
  matched = patterns(pats, cols=cols)
  # replace with lengths when R 3.2.0 dependency arrives
  if (length(idx <- which(sapply(matched, length) == 0L)))
    stop('Pattern', if (length(idx) > 1L) 's', ' not found: [',
         paste(pats[idx], collapse = ', '), ']')

  return(matched)
}
