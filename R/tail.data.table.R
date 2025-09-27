"tail.data.table" <-
function(x, n=6, ...) x[seq(to=nrow(x), length=min(n, nrow(x)))]

