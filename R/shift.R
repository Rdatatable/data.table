shift <- function(x, n=1L, fill=NA, type=c("lag", "lead"), give.names=FALSE) {
    type = match.arg(type)
    ans = .Call(Cshift, x, as.integer(n), fill, type)
    if (give.names) {
        if (is.null(names(x))) {
        		xsub = substitute(x)
        		if (is.atomic(x) && is.name(xsub)) nx = deparse(xsub, 500L)
        		else nx = paste("V", if (is.atomic(x)) 1L else seq_along(x), sep="")
        }
        else nx = names(x)
        setattr(ans, 'names', do.call("paste", c(CJ(nx, type, n, sorted=FALSE), sep="_")))
    }
    ans
}
