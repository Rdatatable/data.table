"NCOL" <-
function (x) 
if (is.array(x) && length(dim(x)) > 1 || is.data.frame(x)) ncol(x) else as.integer(1)
