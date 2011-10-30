
duplicated.data.table <- function(x, ...) {
    if (!cedta()) return(NextMethod("duplicated"))
    keys <- attr(x, "sorted")
    if (is.null(keys)) stop("data table must have keys")
    res <- rep(TRUE, nrow(x))
    res[duplist(x[,key(x),with=FALSE])] = FALSE
    res
}

unique.data.table <- function(x, tolerance=.Machine$double.eps ^ 0.5) {
    if (!cedta()) return(NextMethod("unique"))
    # duplist is relatively quick, in C, and copes with NA
    if (haskey(x)) {
        # Already keyed, this should be fast :
        res = x[duplist(x[,key(x),with=FALSE],tolerance=tolerance)]
        setattr(res,"sorted",key(x))
    } else {
        # TO DO: could be faster by grouping/hashing rather than sorting.
        o = fastorder(x)
        f = o[duplist(x,o,tolerance=tolerance)]
        f = f[.Internal(order(na.last=FALSE, decreasing=FALSE, f))]
        res = x[f]
    }
    res
}


