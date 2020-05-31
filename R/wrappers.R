
# Very small (e.g. one line) R functions that just call C.
# One file wrappers.R to avoid creating lots of small .R files.

fcoalesce   = function(...) .Call(Ccoalesce, list(...), FALSE)
setcoalesce = function(...) .Call(Ccoalesce, list(...), TRUE)

fifelse = function(test, yes, no, na=NA) .Call(CfifelseR, test, yes, no, na)
fcase   = function(..., default=NA) .Call(CfcaseR, default, parent.frame(), as.list(substitute(list(...)))[-1L])

colnamesInt = function(x, cols, check_dups=FALSE) .Call(CcolnamesInt, x, cols, check_dups)
coerceFill = function(x) .Call(CcoerceFillR, x)

testMsg = function(status=0L, nx=2L, nk=2L) .Call(CtestMsgR, as.integer(status)[1L], as.integer(nx)[1L], as.integer(nk)[1L])

fwhich = function(x) {
  xsub = substitute(x)
  #escape = character()
  #if (!xsub %iscall% "&") {
  #  if (eq <- xsub %iscall% c("==","!=")) {
  #    if (length(xsub)!=3L)
  #      stop("internal error: == or != operator but length != 3")
  #    #if (is.call(xsub[[2L]]))
  #    #  escape = c(escape, "operator == or != but LHS is a call")
  #    if (!is.name(xsub[[2L]]) && !is.atomic(xsub[[2L]]))
  #      escape = c(escape, "operator == or != but LHS is not symbol or atomic")
  #    #if (is.call(xsub[[3L]]))
  #    #  escape = c(escape, "operator == or != but RHS is a call")
  #    if (!is.name(xsub[[3L]]) && !is.atomic(xsub[[3L]]))
  #      escape = c(escape, "operator == or != but RHS is not symbol or atomic")
  #    if (!length(escape)) {
  #      lhs = if (is.name(xsub[[2L]])) eval(xsub[[2L]], parent.frame()) else xsub[[2L]]
  #      rhs = if (is.name(xsub[[3L]])) eval(xsub[[3L]], parent.frame()) else xsub[[3L]]
  #      if (length(lhs)!=1L && length(rhs)!=1L)
  #        stop("LHS or RHS must be a length 1 in == and != operators")
  #    }
  #  } else if (na <- xsub %iscall% c("is.na","is.nan")) {
  #    if (length(xsub)!=2L)
  #      stop("internal error: is.na or is.nan call but length != 2")
  #    if (!is.name(xsub[[2L]]) && !is.atomic(xsub[[2L]]))
  #      escape = c(escape, "fun is.na or is.nan but argument is not symbol or atomic")
  #    if (!length(escape)) {
  #      arg = if (is.name(xsub[[2L]])) eval(xsub[[2L]], parent.frame()) else xsub[[2L]]
  #    }
  #  } else {
  #    escape = c(escape, "unsupported operator")
  #  }
  #}
  #if (length(escape)) {
  #  if (verbose)
  #    cat("fwhich fallback to R's which: ", paste(escape, collapse=", "), "\n", sep="");
  #  return(eval(substitute(
  #    base::which(.xsub, arr.ind=FALSE, useNames=FALSE),
  #    list(.xsub = xsub)
  #  ), parent.frame()))
  #} else {
    .Call(CfwhichR, xsub, parent.frame())
  #}
}
which_eq = function(x, value, negate=FALSE, intersect=NULL) .Call(Cwhich_eqR, x, value, negate, intersect) ## only for benchmarking
fsintersect = function(x, y) .Call(CfsintersectR, x, y) ## only for benchmarking
#fwhichOpt = function(x) .Call(CfwhichOptR, substitute(x)) ## only for dev testing

"%!in%" = function(x, y) !(x %in% y)
