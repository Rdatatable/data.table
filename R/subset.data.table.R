"[.data.table" <-
function (x, i, j, drop=FALSE)
{
    # If you do drop=TRUE, you want a single vector, and in that case just do table$col. But that doesn't allow any subsetting, so you might do ,drop=TRUE in that case,  or table[subset]$col but that copies all columns into the subset which may be inefficient
    # TO DO: Allow j to be an expression (or series of expressions) to be evalulated like a call to with()
    # "[.data.table" can't be replaced with a direct call to subset() because we want to allow row and column integer indexing too.
    
    mdrop <- missing(drop)
    if (mode(substitute(i))=="call") {
        # so 'i' can be any expression of column names.
        # i can also include objects from the calling frame e.g. TABLE[ColA %in% Lkp] where Lkp is a vector defined in the calling frame.
        i = eval(substitute(i), envir=x, enclos=parent.frame())
        if (is.logical(i)) i[is.na(i)] = FALSE  # To simplify statement so don't have to do TABLE[!is.na(ColA) & ColA==ColB]
    }
    cols <- names(x)
    if (missing(i)) {
        if (!missing(j))
            x <- x[j]
        cols <- names(x)
        if (any(is.na(cols)))
            stop("undefined columns selected")
    } else {
        if (is.character(i)) stop("data.table has no rownames to subset")
        if (!missing(j)) {
            x <- x[j]
            cols <- names(x)
            if (any(is.na(cols)))
                stop("undefined columns selected")
        }
        for (j in seq(along = x)) {
            xj <- x[[j]]
            x[[j]] <- if (length(dim(xj)) != 2) 
                xj[i]
            else xj[i, , drop = FALSE]
        }
    }
    if (drop) {
        drop <- FALSE
        n <- length(x)
        if (n == 1) {
            x <- x[[1]]
            drop <- TRUE
        }
        else if (n > 1) {
            xj <- x[[1]]
            nrow <- if (length(dim(xj)) == 2) 
                dim(xj)[1]
            else length(xj)
            if (!mdrop && nrow == 1) {
                drop <- TRUE
                names(x) <- cols
            }
        }
    }
    if (!drop) {
        names(x) <- cols
        if (any(duplicated(nm <- names(x)))) 
            names(x) <- make.unique(nm)
    }
    x
}

