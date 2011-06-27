
duplicated.data.table <- function(x, ...) {
    # Relatively quick test for duplication in data.table keys using diff
    if (!cedta()) return(NextMethod("duplicated"))
    keys <- attr(x, "sorted")
    if (is.null(keys)) stop("data table must have keys")
    res <- rep(TRUE, nrow(x))
    res[duplist(x[,key(x),with=FALSE])] = FALSE
    res
}

unique.data.table <- function(x, ...) {
    if (!cedta()) return(NextMethod("unique"))
    # duplist is relatively quick, in C, and copes with NA
    if (haskey(x)) { 
        res = x[duplist(x[,key(x),with=FALSE])]
        attr(res,"sorted") = key(x)
    } else {
        res = x[duplist(x)]
    }
    res
}


