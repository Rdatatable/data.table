
# Very small (e.g. one line) R functions that just call C.
# One file wrappers.R to avoid creating lots of small .R files.

fcoalesce   = function(...) .Call(Ccoalesce, list(...), FALSE)
setcoalesce = function(...) .Call(Ccoalesce, list(...), TRUE)

fifelse = function(test, yes, no, na=NA) .Call(CfifelseR, test, yes, no, na)
fcase   = function(..., default=NA) .Call(CfcaseR, parent.frame(), as.list(substitute(list(..., TRUE, default)))[-1L])

colnamesInt = function(x, cols, check_dups=FALSE, skip_absent=FALSE) .Call(CcolnamesInt, x, cols, check_dups, skip_absent)

testMsg = function(status=0L, nx=2L, nk=2L) .Call(CtestMsgR, as.integer(status)[1L], as.integer(nx)[1L], as.integer(nk)[1L])

isRealReallyInt = function(x) .Call(CisRealReallyIntR, x)
isReallyReal = function(x) .Call(CisReallyReal, x)

coerceAs = function(x, as, copy=TRUE) .Call(CcoerceAs, x, as, copy)
