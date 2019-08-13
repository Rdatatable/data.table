
# Very small (e.g. one line) R functions that just call C.
# One file wrappers.R to avoid creating lots of small .R files.

coalesce    = function(...) .Call(Ccoalesce, list(...), FALSE)
setcoalesce = function(...) .Call(Ccoalesce, list(...), TRUE)

fifelse = function(test, yes, no) .Call(CfifelseR,test, yes, no)

colnamesInt = function(x, cols, check_dups=FALSE) .Call(CcolnamesInt, x, cols, check_dups)
coerceClass = function(x, out) .Call(CcoerceClassR, x, out)
coerceFill = function(x) coerceClass(x, list(0L, 0, structure(0, class="integer64"))) # temporarily till new tests in place
