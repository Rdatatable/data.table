# all non-exported / unused internal (utility) functions

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

# helper used to auto-name columns in data.table(x, y) as x, y (and similar)
#   from the original comment in data.table.R:
#     Intention here is that data.table(X,Y) will automatically put X and Y
#     as the column names.  For longer expressions, name the arguments to
#     data.table(). But in a call to [.data.table, wrap in list() e.g.
#     DT[,list(a=mean(v),b=foobarzoo(zang))] will get the col names
#   naming of unnested matrices still handled by data.table()
name_dots <- function(...) {
  dot_sub <- as.list(substitute(list(...)))[-1L]
  vnames = names(dot_sub)
  l = list(...)
  if (is.null(vnames)) vnames = rep.int("", length(l))
  vnames[is.na(vnames)] = ""
  novname = vnames==""
  if (any(idx <- !novname)) {
    if (any(vnames[idx] == ".SD")) stop("A column may not be called .SD. That has special meaning.")
  }
  for (i in which(novname)) {
    # if (ncol(as.data.table(x[[i]])) <= 1) { # cbind call in test 230 fails if I write ncol(as.data.table(eval(tt[[i]], parent.frame()))) <= 1, no idea why... (keep this for later even though all tests pass with ncol(.).. because base uses as.data.frame(.))
    if (is.null(ncol(l[[i]]))) {
      if ((tmp <- deparse(dot_sub[[i]])[1L]) == make.names(tmp))
        vnames[i] <- tmp
    }
  }
  still_empty = vnames==""
  if (any(still_empty)) vnames[still_empty] = paste0("V", which(still_empty))
  list(vnames = vnames,
       novname = novname)
}
