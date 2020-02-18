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

col_helper = function(x, colsub, origin_call = NULL) {
  ## helper that could replace some common code used for by, .SDcols, j, duplicated, unique, dcast, 
  ## or anything that looks for columns
  
  #' @param x required; a data.frame, data.table, tibble, or list.
  #' @param colsub takes the same arguments as .SDcols such as
  #' `patterns()`, `var1:var2`, a variable, or a vector of characters.
  #' When TRUE, will return seq_len(length(x))
  #' @param origin_call takes character vector of length 1. In the short-term
  #' there will be different modes based on the call. In the long-term, it's just
  #' to help the end-user what caused the error
  #' @return Integer vector with an attribute of whether negation was in the call
  
  # see @jangorecki #4174 for how to get the vast majority of this into C
  
  x_len = length(x)
  
  if (length(colsub) == 1L && isTRUE(colsub)){
    ansvals = seq_len(x_len)
    attr(ansvals, "negate") = FALSE
    return(ansvals)
  }
  
  # FR #4979 - negative numeric and character indices for SDcols
  # fix for #5190. colsub[[1L]] gave error when it's a symbol.
  negate_cols = is.call(colsub) && deparse(colsub[[1L]], 500L, backtick=FALSE) %in% c("!", "-")
  if (negate_cols) colsub = colsub[[2L]]
  
  if (is.call(colsub)){
    # fix for #1216, make sure the parentheses are peeled from expr of the form (((1:4)))
    while(colsub %iscall% "(") colsub = as.list(colsub)[[-1L]]
    if (length(colsub) == 3L && colsub[[1L]] == ":") {
      if (is.name(colsub[[2L]])){
        # cols is of the format a:c
        rnge = chmatch(c(as.character(colsub[[2L]]), as.character(colsub[[3L]])), names(x))
        cols = rnge[1L]:rnge[2L]
      } else {
        # cols is of the format 1:3
        cols = eval(colsub)
      }
    } else if (colsub[[1L]] == "patterns") {
      # each pattern gives a new filter condition, intersect the end result
      colsub = as.list(colsub)[-1L]
      if (length(colsub) == 1L) { #e.g. patterns("only_one_pattern")
        cols = grep(eval(colsub[[1L]], parent.frame(2L)), names(x))
      } else { #e.g. patterns("one", "or", "more", "patterns") 
        x_names = names(x)
        cols = Reduce(intersect, lapply(colsub, function(col) grep(eval(col, parent.frame(2L)), x_names)))
      }
    } else {
      cols = eval(colsub, parent.frame(2L))
    }
  } else {
    # if colsub is also a name in the list, we prioritize it over anything in the environment
    if (exists(as.character(colsub), where = x)) 
      cols = as.character(colsub) 
    else 
      cols = eval(colsub, parent.frame(2L), parent.frame(2L))
  }
                                        
  # allow filtering via function in .SDcols, #3950
  if (is.function(cols)) {
    cols = lapply(x, cols)
    if (any(idx <- vapply_1i(cols, length) > 1L | vapply_1c(cols, typeof) != 'logical' | vapply_1b(cols, anyNA)))
      stop("When .SDcols is a function, it is applied to each column; the output of this function must be a non-missing boolean scalar signalling inclusion/exclusion of the column. However, these conditions were not met for: ", brackify(names(x)[idx]))
    cols = unlist(cols, use.names = FALSE)
  }
  
  if (origin_call == "by") {
    #check for weird comma delimited "V1,V2,V3" and give warning message
    #in the by eval we'll check if by = list(...) otherwise we can come here.
  }
  
  if (anyNA(cols))
    stop(origin_call, " missing at the following indices: ", brackify(which(is.na(cols))))
  
  if (is.logical(cols)) {
    if (length(cols) != x_len) {
      ## TODO change to error in 2022
      warning("When ", origin_call, " is a logical vector, each column should have a TRUE or FALSE entry. Vector will be repeated to length of data.table. Warning will change to error in year 2022.")
      cols = rep(cols, length.out = x_len)
    }
    cols = which_(cols)
  }
                                        
  if (is.numeric(cols)) {
    cols = as.integer(cols)
    if (length(cols) == 1L && cols < 0L && abs(cols) <= x_len) cols = setdiff(seq_len(x), abs(cols))
    if (length(unique(sign(cols))) > 1L) stop(origin_call, " is numeric but has both +ve and -ve indices")
    if (any(idx <- abs(cols)>x_len | abs(cols)<1L))
      stop(".SDcols is numeric but out of bounds [1, ", x_len, "] at: ", brackify(which(idx)))
    ansvals = if (negate_cols) setdiff(seq_len(x_len), cols) else cols
  } else {
    if (!is.character(cols)) stop(origin_call, " should be column numbers or names")
    x_names = names(x)
    if (!all(idx <- cols %in% x_names))
      stop("Some items of ", origin_call, " are not column names: ", brackify(cols[!idx]))
    ansvars = if (negate_cols) setdiff(x_names, cols) else cols
    ansvals = chmatch(ansvars, x_names)
  }
  attr(ansvals, "negate") = negate_cols
  return(ansvals)
}
