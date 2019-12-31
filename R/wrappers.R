
# Very small (e.g. one line) R functions that just call C.
# One file wrappers.R to avoid creating lots of small .R files.

fcoalesce   = function(...) .Call(Ccoalesce, list(...), FALSE)
setcoalesce = function(...) .Call(Ccoalesce, list(...), TRUE)

fifelse = function(test, yes, no, na=NA) .Call(CfifelseR, test, yes, no, na)
fcase   = function(..., default=NA) .Call(CfcaseR, default, parent.frame(), as.list(substitute(list(...)))[-1L])

colnamesInt = function(x, cols, check_dups=FALSE) .Call(CcolnamesInt, x, cols, check_dups)
coerceFill = function(x) .Call(CcoerceFillR, x)

testMsg = function(status=0L, nx=2L, nk=2L) .Call(CtestMsgR, as.integer(status)[1L], as.integer(nx)[1L], as.integer(nk)[1L])

which_eq = function(x, value, negate=FALSE, intersect=NULL) .Call(Cwhich_eqR, x, value, negate, intersect) ## only for benchmarking
fintersect = function(x, y) .Call(CfintersectR, x, y) ## only for benchmarking
exprList = function(x) {
  stopifnot(is.language(x))
  len = 1L
  expr = x
  repeat {
    if (!is.call(expr) || expr[[1L]]!=quote("&")) break
    len = len+1L
    expr = expr[[2L]]
  }
  ans = vector("list", len)
  for (i in seq_len(len)) {
    s = rep(2L, len-i)
    if (i > 1L) s = c(s, 3L)
    ans[[i]] = x[[s]]
    #ans[[i]] = list(op = as.character(expr[[1L]]), lhs = expr[[2L]], rhs = expr[[3L]])
  }
  ans
} ## TODO: handle in C
isAndingR = function(x) {
  stopifnot(is.language(x))
  is.call(x) && `||`(
    `||`(
      isAndingR(x[[2L]]),
      x[[1L]]==quote("==") # leaf op node
    ),
    x[[1L]]==quote("&")
  )
} ## TODO: handle in C
fwhichOpt = function(x) .Call(CfwhichOptR, substitute(x)) ## only for dev testing
fwhich = function(x) .Call(CfwhichR, substitute(x), parent.frame())
