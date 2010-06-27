
duplicated.data.table <- function(x, ...) {
    # Relatively quick test for duplication in data.table keys using diff
    if (cendta()) return(NextMethod("duplicated"))
    keys <- attr(x, "sorted")
    if (is.null(keys)) stop("data table must have keys")
    res <- c(FALSE, rep(TRUE, nrow(x) - 1))
    idx <- 2:nrow(x)
    for (key in keys)
        res[idx] <- res[idx] & !diff(unclass(x[[key]]))
    res
}

unique.data.table <- function(x, ...) {
    if (cendta()) return(NextMethod("unique"))
    x[!duplicated(x)]
}


