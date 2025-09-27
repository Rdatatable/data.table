"head.data.table" <-
function(x, n=6, ...) x[seq(len=min(n,nrow(x)))]

