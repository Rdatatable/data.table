
# Very small (e.g. one line) R functions that just call C.
# One file wrappers.R to avoid creating lots of small .R files.

fcoalesce   = function(...) .Call(Ccoalesce, list(...), FALSE)
setcoalesce = function(...) .Call(Ccoalesce, list(...), TRUE)

fifelse = function(test, yes, no, na=NA) .Call(CfifelseR, test, yes, no, na)
fcase   = function(..., default=NA) .Call(CfcaseR, default, parent.frame(), as.list(substitute(list(...)))[-1L])

colnamesInt = function(x, cols, check_dups=FALSE) .Call(CcolnamesInt, x, cols, check_dups)
coerceFill = function(x) .Call(CcoerceFillR, x)

testMsg = function(status=0L, nx=2L, nk=2L) .Call(CtestMsgR, as.integer(status)[1L], as.integer(nx)[1L], as.integer(nk)[1L])
