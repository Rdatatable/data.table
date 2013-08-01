
duplicated.data.table <- function(x, incomparables=FALSE,
                                  tolerance=.Machine$double.eps ^ 0.5,
                                  use.key=TRUE, ...) {
    if (!cedta()) return(NextMethod("duplicated"))
    res <- rep(TRUE, nrow(x))
    if (haskey(x) && use.key)
        res[duplist(x[,key(x),with=FALSE],tolerance=tolerance)] = FALSE
    else {
        o = fastorder(x)
        f = o[duplist(x,o,tolerance=tolerance)]
        f = f[sort.list(f, na.last=FALSE, decreasing=FALSE)]
        # TO DO: remove sort.list call by replacing fastorder with fastgroup
        res[f] = FALSE
    }
    res
}

unique.data.table <- function(x, incomparables=FALSE,
                              tolerance=.Machine$double.eps ^ 0.5,
                              use.key=TRUE, ...) {
    if (!cedta()) return(NextMethod("unique"))
    # duplist is relatively quick, in C, and copes with NA
    if (haskey(x) && use.key) {
        # Already keyed, this should be fast :
        rows = duplist(x[,key(x),with=FALSE],tolerance=tolerance)
        res = x[rows]
        setattr(res,"sorted",key(x))
    } else {
        # TO DO: could be faster by grouping/hashing rather than sorting.
        o = fastorder(x)
        f = o[duplist(x,o,tolerance=tolerance)]
        f = f[sort.list(f, na.last=FALSE, decreasing=FALSE)]
        # TO DO: remove sort.list call by replacing fastorder with fastgroup
        res = x[f]
    }
    res
}


