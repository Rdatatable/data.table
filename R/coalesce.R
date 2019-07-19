
coalesce    = function(...) .Call(Ccoalesce, list(...), FALSE)
setcoalesce = function(...) .Call(Ccoalesce, list(...), TRUE)

