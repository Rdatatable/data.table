"as.data.table.matrix" <-
function(x, keep.rownames=FALSE)
{
    if (keep.rownames) return(data.table(rn=rownames(x), x, keep.rownames=FALSE))
    d <- dim(x)
    nrows <- d[1]
    ir <- seq(length = nrows)
    ncols <- d[2]
    ic <- seq(length = ncols)
    dn <- dimnames(x)
    collabs <- dn[[2]]
    if (any(empty <- nchar(collabs) == 0)) 
        collabs[empty] <- paste("V", ic, sep = "")[empty]
    value <- vector("list", ncols)
    if (mode(x) == "character") {
        for (i in ic) value[[i]] <- as.factor(x[, i])       # for efficiency.
    }
    else {
        for (i in ic) value[[i]] <- as.vector(x[, i])       # to drop any row.names that would otherwise be retained inside every column of the data.table
    }
    if (length(collabs) == ncols) 
        names(value) <- collabs
    else
        names(value) <- paste("V", ic, sep = "")
    class(value) <- "data.table"
    value
}

