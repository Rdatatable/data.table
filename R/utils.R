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
  if (length(x) != 1L) stopf("Argument 'nan' must be length 1")
  if (identical(x, NA) || identical(x, NA_real_)) return(TRUE)
  if (identical(x, NaN)) return(FALSE)
  stopf("Argument 'nan' must be NA or NaN")
}

if (base::getRversion() < "3.2.0") {  # Apr 2015
  isNamespaceLoaded = function(x) x %chin% loadedNamespaces()
}

if (!exists('startsWith', 'package:base', inherits=FALSE)) {  # R 3.3.0; Apr 2016
  startsWith = function(x, stub) substr(x, 1L, nchar(stub))==stub
}
# endsWith no longer used from #5097 so no need to backport; prevent usage to avoid dev delay until GLCI's R 3.1.0 test
endsWith = function(...) stop("Internal error: use endsWithAny instead of base::endsWith")

startsWithAny = function(x,y) .Call(CstartsWithAny, x, y, TRUE)
endsWithAny = function(x,y) .Call(CstartsWithAny, x, y, FALSE)
# For fread.R #5097 we need if any of the prefixes match, which one, and can return early on the first match
# Hence short and simple ascii-only at C level

# which.first
which.first = function(x)
{
  if (!is.logical(x)) {
    stopf("x not boolean")
  }
  match(TRUE, x)
}

# which.last
which.last = function(x)
{
  if (!is.logical(x)) {
    stopf("x not boolean")
  }
  length(x) - match(TRUE, rev(x)) + 1L
}

require_bit64_if_needed = function(DT) {
  # called in fread and print.data.table
  if (!isNamespaceLoaded("bit64") && any(vapply_1b(DT, inherits, "integer64"))) {
    # nocov start
    # a test was attempted to cover the requireNamespace() by using unloadNamespace() first, but that fails when nanotime is loaded because nanotime also uses bit64
    if (!requireNamespace("bit64",quietly=TRUE)) {
      warningf("Some columns are type 'integer64' but package bit64 is not installed. Those columns will print as strange looking floating point data. There is no need to reload the data. Simply install.packages('bit64') to obtain the integer64 print method and print the data again.")
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
    syms = vapply_1b(dot_sub, is.symbol)  # save the deparse() in most cases of plain symbol
    for (i in which(notnamed)) {
      tmp = if (syms[i]) as.character(dot_sub[[i]]) else deparse(dot_sub[[i]], nlines=1L)[1L]
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
  sprintf('[%s]', toString(x))
}

# patterns done via NSE in melt.data.table and .SDcols in `[.data.table`
# was called do_patterns() before PR#4731
eval_with_cols = function(orig_call, all_cols) {
  parent = parent.frame(2L)
  fun_uneval = orig_call[[1L]]
  # take fun from either calling env (parent) or from data.table
  fun = tryCatch({
    maybe_fun = eval(fun_uneval, parent)
    # parent env could have a non-function with this name, which we
    # should ignore.
    stopifnot(is.function(maybe_fun))
    maybe_fun
  }, error=function(e) {
    eval(fun_uneval)#take function from data.table namespace.
  })
  if (!is.primitive(fun)) {
    named_call = match.call(fun, orig_call)
    if ("cols" %in% names(formals(fun)) && !"cols" %in% names(named_call)) {
      named_call[["cols"]] = all_cols
    }
    named_call[[1L]] = fun
    eval(named_call, parent)
  }
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

rss = function() {  #5515 #5517
  # nocov start
  cmd = paste0("ps -o rss --no-headers ", Sys.getpid()) # ps returns KB
  ans = tryCatch(as.numeric(system(cmd, intern=TRUE)), warning=function(w) NA_real_, error=function(e) NA_real_)
  if (length(ans)!=1L || !is.numeric(ans)) ans=NA_real_ # just in case
  round(ans / 1024, 1L)  # return MB
  # nocov end
}

