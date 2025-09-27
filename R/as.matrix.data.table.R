"as.matrix.data.table" <-
function(x)
{
    dm <- dim(x)
    cn <- colnames(x)
    if (any(dm == 0))
        return(array(NA, dim = dm, dimnames = list(NULL, cn)))
    p <- dm[2]
    n <- dm[1]
    collabs <- as.list(cn)
    X <- x
    class(X) <- NULL
    non.numeric <- non.atomic <- FALSE
    all.logical <- TRUE
    for (j in 1:p) {
        xj <- X[[j]]
        if (length(dj <- dim(xj)) == 2 && dj[2] > 1) {
            if (inherits(xj, "data.table")) 
                xj <- X[[j]] <- as.matrix(X[[j]])
            dnj <- dimnames(xj)[[2]]
            collabs[[j]] <- paste(collabs[[j]], if (length(dnj) > 
                0) 
                dnj
            else 1:dj[2], sep = ".")
        }
        if (!is.logical(xj)) 
            all.logical <- FALSE
        if (length(levels(xj)) > 0 || !(is.numeric(xj) || is.complex(xj)) || 
            (!is.null(cl <- attr(xj, "class")) && any(cl %in% 
                c("Date", "POSIXct", "POSIXlt")))) 
            non.numeric <- TRUE
        if (!is.atomic(xj)) 
            non.atomic <- TRUE
    }
    if (non.atomic) {
        for (j in 1:p) {
            xj <- X[[j]]
            if (is.recursive(xj)) {
            }
            else X[[j]] <- as.list(as.vector(xj))
        }
    }
    else if (all.logical) {
    }
    else if (non.numeric) {
        for (j in 1:p) {
            if (is.character(X[[j]])) 
                next
            xj <- X[[j]]
            miss <- is.na(xj)
            xj <- if (length(levels(xj))) 
                as.vector(xj)
            else format(xj)
            is.na(xj) <- miss
            X[[j]] <- xj
        }
    }
    X <- unlist(X, recursive = FALSE, use.names = FALSE)
    dim(X) <- c(n, length(X)/n)
    dimnames(X) <- list(NULL, unlist(collabs, use.names = FALSE))
    X
}

