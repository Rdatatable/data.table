"NROW" <-
function (x) 
if (is.array(x) || is.data.frame(x)) nrow(x) else length(x)
