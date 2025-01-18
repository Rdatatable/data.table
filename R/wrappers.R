
# Very small (e.g. one line) R functions that just call C.
# One file wrappers.R to avoid creating lots of small .R files.

fcoalesce   = function(...) .Call(Ccoalesce, list(...), FALSE)
setcoalesce = function(...) .Call(Ccoalesce, list(...), TRUE)

fifelse = function(test, yes, no, na=NA) .Call(CfifelseR, test, yes, no, na)
fcase   = function(..., default=NA) {
  # TODO(R>=3.5.0): Use ...length() to avoid the need for suppressWarnings() here
  default_condition <- suppressWarnings(rep(TRUE, length(switch(1, ...)))) # better than ..1/..elt(1): won't fail for empty fcase()
  arg_list <- as.list(substitute(list(..., default_condition, default)))[-1L]
  .Call(CfcaseR, parent.frame(), arg_list)
}

colnamesInt = function(x, cols, check_dups=FALSE, skip_absent=FALSE) .Call(CcolnamesInt, x, cols, check_dups, skip_absent)

testMsg = function(status=0L, nx=2L, nk=2L) .Call(CtestMsgR, as.integer(status)[1L], as.integer(nx)[1L], as.integer(nk)[1L])

fitsInInt32 = function(x) .Call(CfitsInInt32R, x)
fitsInInt64 = function(x) .Call(CfitsInInt64R, x)

coerceAs = function(x, as, copy=TRUE) .Call(CcoerceAs, x, as, copy)
