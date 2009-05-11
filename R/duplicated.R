
duplicated.data.table <- function(x, ...) {
    # Relatively quick test for duplication in data.table keys using diff
    keys <- attr(x, "sorted")
    if (is.null(keys)) stop("data table must have keys")
    res <- c(FALSE, rep(TRUE, nrow(x) - 1))
    idx <- 2:nrow(x)
    for (key in keys)
        res[idx] <- res[idx] & !diff(unclass(x[[key]]))
    res
}

unique.data.table <- function(x, ...) x[!duplicated(x)]
