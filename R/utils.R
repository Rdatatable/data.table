# all non-exported / unused internal (utility) functions

# R 3.5.0 made isTRUE longer but more efficient :
#   `is.logical(x) && length(x)==1L && !is.na(x) && x`
# Before R 3.5.0, isTRUE was defined as simply:
#   identical(TRUE,x)
# See PR#3421 for timings.
# It was changed in R so that isTRUE(c(a=TRUE)) returned TRUE: https://github.com/wch/r-source/commit/828997ac6ecfb73aaa0aae9d1d0584a4ffc50881#diff-b41e3f9f1d389bb6f7a842cd5a3308b8
if (base::getRversion() < "3.5.0") {
  isTRUE  = function(x) is.logical(x) && length(x)==1L && !is.na(x) && x    # backport R's new implementation of isTRUE
  isFALSE = function(x) is.logical(x) && length(x)==1L && !is.na(x) && !x   # backport isFALSE that was added in R 3.5.0
}
isTRUEorNA    = function(x) is.logical(x) && length(x)==1L && (is.na(x) || x)
isTRUEorFALSE = function(x) is.logical(x) && length(x)==1L && !is.na(x)
allNA = function(x) .Call(C_allNAR, x)
# helper for nan argument (e.g. nafill): TRUE -> treat NaN as NA
nan_is_na = function(x) {
  if (length(x) != 1L) stop("Argument 'nan' must be length 1")
  if (identical(x, NA) || identical(x, NA_real_)) return(TRUE)
  if (identical(x, NaN)) return(FALSE)
  stop("Argument 'nan' must be NA or NaN")
}

if (base::getRversion() < "3.2.0") {  # Apr 2015
  isNamespaceLoaded = function(x) x %chin% loadedNamespaces()
}

# which.first
which.first = function(x)
{
  if (!is.logical(x)) {
    stop("x not boolean")
  }
  match(TRUE, x)
}

# which.last
which.last = function(x)
{
  if (!is.logical(x)) {
    stop("x not boolean")
  }
  length(x) - match(TRUE, rev(x)) + 1L
}

require_bit64_if_needed = function(DT) {
  # called in fread and print.data.table
  if (!isNamespaceLoaded("bit64") && any(sapply(DT,inherits,"integer64"))) {
    # nocov start
    # a test was attempted to cover the requireNamespace() by using unloadNamespace() first, but that fails when nanotime is loaded because nanotime also uses bit64
    if (!requireNamespace("bit64",quietly=TRUE)) {
      warning("Some columns are type 'integer64' but package bit64 is not installed. Those columns will print as strange looking floating point data. There is no need to reload the data. Simply install.packages('bit64') to obtain the integer64 print method and print the data again.")
    }
    # nocov end
  }
}

# vapply for return value character(1)
vapply_1c = function (x, fun, ..., use.names = TRUE) {
  vapply(X = x, FUN = fun, ..., FUN.VALUE = NA_character_, USE.NAMES = use.names)
}

# vapply for return value logical(1)
vapply_1b = function (x, fun, ..., use.names = TRUE) {
  vapply(X = x, FUN = fun, ..., FUN.VALUE = NA, USE.NAMES = use.names)
}

# vapply for return value integer(1)
vapply_1i = function (x, fun, ..., use.names = TRUE) {
  vapply(X = x, FUN = fun, ..., FUN.VALUE = NA_integer_, USE.NAMES = use.names)
}

more = function(f) system(paste("more",f))    # nocov  (just a dev helper)

# helper used to auto-name columns in data.table(x,y) as c("x","y"), CJ(x,y) and similar
# naming of unnested matrices still handled by data.table()
name_dots = function(...) {
  dot_sub = as.list(substitute(list(...)))[-1L]
  vnames = names(dot_sub)
  if (is.null(vnames)) {
    vnames = character(length(dot_sub))
  } else {
    vnames[is.na(vnames)] = ""
  }
  notnamed = vnames==""
  if (any(notnamed)) {
    syms = sapply(dot_sub, is.symbol)  # save the deparse() in most cases of plain symbol
    for (i in which(notnamed)) {
      tmp = if (syms[i]) as.character(dot_sub[[i]]) else deparse(dot_sub[[i]])[1L]
      if (tmp == make.names(tmp)) vnames[i]=tmp
    }
  }
  list(vnames=vnames, .named=!notnamed)
}

# convert a vector like c(1, 4, 3, 2) into a string like [1, 4, 3, 2]
#   (common aggregation method for error messages)
brackify = function(x, quote=FALSE) {
  # arbitrary
  CUTOFF = 10L
  # keep one more than needed to trigger dots if needed
  if (quote && is.character(x)) x = paste0("'",head(x,CUTOFF+1L),"'")
  if (length(x) > CUTOFF) x = c(x[1:CUTOFF], '...')
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

# check UTC status
is_utc = function(tz) {
  # via grep('UTC|GMT', OlsonNames(), value = TRUE); ordered by "prior" frequency
  utc_tz = c("UTC", "GMT", "Etc/UTC", "Etc/GMT", "GMT-0", "GMT+0", "GMT0")
  if (is.null(tz)) tz = Sys.timezone()
  return(tz %chin% utc_tz)
}

# very nice idea from Michael to avoid expression repetition (risk) in internal code, #4226
"%iscall%" = function(e, f) { is.call(e) && e[[1L]] %chin% f }

# nocov start #593 always return a data.table
edit.data.table = function(name, ...) {
  setDT(NextMethod('edit', name))[]
}
# nocov end

col_helper = function(x, colsub, mode = NA_character_) {
  ## helper that could replace some common code used for by, .SDcols, j, duplicated, unique, dcast, 
  ## or anything that looks for columns
  
  #' @param x required; a data.frame, data.table, tibble, or list.
  #' @param colsub takes the same arguments as .SDcols such as
  #' `patterns()`, `var1:var2`, a variable, or a vector of characters.
  #' When TRUE, will return seq_len(length(x))
  #' @param mode takes character vector of length 1. In the short-term
  #' there will be different modes based on the call. In the long-term, it's just
  #' to help the end-user what caused the error
  #' @return Integer vector with a boolean attribute, `"negate"` which is TRUE when
  #' `colsub[[1L]]` is `-` or `!`.
  
  # see @jangorecki #4174 for how to get the vast majority of this into C
  
  x_len = length(x)
  origsub = colsub
  
  # FR #4979 - negative numeric and character indices for SDcols
  # fix for #5190. colsub[[1L]] gave error when it's a symbol.
  negate_cols = is.call(colsub) && (colsub[[1L]] == "!" || (colsub[[1L]] == "-" && length(colsub) == 2L))
  if (negate_cols) colsub = colsub[[2L]]

  if (is.call(colsub)){
    # fix for #1216, make sure the parentheses are peeled from expr of the form (((1:4)))
    if (colsub[[1L]] == "(") {
      colsub = as.list(colsub)[[-1L]]
      while(length(colsub) > 1L && colsub[[1L]] == "(") colsub = colsub[[-1L]]
      # give users a second chance with negation for (-1)
      if (length(colsub) == 2L && colsub[[1L]] == "-" && is.numeric(colsub[[2L]])) {
        negate_cols = TRUE ##what about -(-1)???
        colsub = colsub[[2L]]
      }
    }
    if (length(colsub) == 3L && colsub[[1L]] == ":") {
      if (is.name(colsub[[2L]]) && is.name(colsub[[3L]])){
        # cols is of the format a:c
        rnge = chmatch(c(as.character(colsub[[2L]]), as.character(colsub[[3L]])), names(x))
        if (anyNA(rnge)) stop(gettextf("Not all columns were found in the %s column select statement: [%s]", mode, deparse(origsub), domain = "R-data.table"))
        cols = rnge[1L]:rnge[2L] 
      } else if (is.character(colsub[[2L]]) || is.character(colsub[[3L]])){
        stop(gettextf("When selecting a range of columns in the var_1:var_2 format, quotation marks should not be used. Instead of %s try %s:%s", deparse(origsub), deparse(as.name(colsub[[2L]])), deparse(as.name(colsub[[3L]])), domain = "R-data.table"))
      } else {
        # cols is of the format 1:3 or would also allow min(var):max(var)
        cols = eval(colsub, parent.frame(2L), parent.frame(2L))
      }
    } else if (length(colsub) > 1 && colsub[[1L]] == "patterns") {
      # each pattern gives a new filter condition, intersect the end result
      x_names = names(x)
      cols = Reduce(intersect, lapply(as.list(colsub)[-1L], function(col) grep(eval(col, parent.frame(2L)), x_names)))
    } else {
      cols = eval(colsub, parent.frame(2L), parent.frame(2L))
    }
  } else {
    cols = eval(colsub, parent.frame(2L), parent.frame(2L))
  }
  
  # allow filtering via function in .SDcols, #3950
  if (is.function(cols)) {
    cols = lapply(x, cols)
    if (any(idx <- vapply_1i(cols, length) > 1L | vapply_1c(cols, typeof) != 'logical' | vapply_1b(cols, anyNA)))
      stop(gettextf("When %s is a function, it is applied to each column; the output of this function must be a non-missing boolean scalar signalling inclusion/exclusion of the column. However, these conditions were not met for: %s", mode, brackify(names(x)[idx]), domain = "R-data.table"))
    cols = unlist(cols, use.names = FALSE)
  }
  
  if (anyNA(cols))
    stop(gettextf("%s missing at the following indices: %s", mode, brackify(which(is.na(cols)))))
  
  if (is.logical(cols)) {
    if ((col_len <- length(cols)) != x_len) {
      ## TODO change to error in 2022
      warning(gettextf("When %s is a logical vector, each column should have a TRUE or FALSE entry. The current logical vector of length %d will be repeated to length of data.table. Warning will change to error in the next verion.", mode, col_len, domain = "R-data.table"))
      cols = rep(cols, length.out = x_len)
    } 
    ansvals = which_(cols, !negate_cols)
  } else if (is.numeric(cols)) {
    cols = as.integer(cols)
    if (any(idx <- abs(cols)>x_len | cols == 0L))
      stop(gettextf("%s is numeric but out of bounds [1, %d] at: %s", mode, x_len, brackify(which(idx)), domain = "R-data.table"))
    if (length(unique(sign(cols))) > 1L) stop(gettextf("%s is numeric but has both +ve and -ve indices", mode, domain = "R-data.table"))
    ansvals = if (negate_cols) setdiff(seq_len(x_len), cols) else cols
  } else {
    if (!is.character(cols)) stop(gettextf("%s should be column numbers or names", mode, domain = "R-data.table"))
    x_names = names(x)
    if (!all(idx <- cols %chin% x_names))
      stop(gettextf("Some items of %s are not column names: %s", mode, brackify(cols[!idx]), domain = "R-data.table"))
    ansvars = if (negate_cols) setdiff(x_names, cols) else cols
    ansvals = chmatch(ansvars, x_names)
  }
  attr(ansvals, "negate") = negate_cols
  return(ansvals)
}
