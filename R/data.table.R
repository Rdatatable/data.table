"data.table" <-
function(..., keep.rownames=FALSE, check.names = TRUE)
{
    # creates a data.table directly, without the overhead of creating a data.frame first (ie the overhead of rownames)
    # you can convert from matrix and data.frame, to data.table, and retain the rownames in the first column - see as.data.table.matrix()
    # multiple matrices, and data.frames can be passed in (each with rownames) and each of the rownames can be retained as columns
    # TO DO: allow silent repition of input arguments, like data.frame() does
    x <- list(...)
    if (length(x) == 1 && is.list(x[[1]]) && !is.data.frame(x[[1]]) && !is.data.table(x[[1]]) ) {
        x = x[[1]]       # a list was passed in. The idea here is that constructing a list and passing it in, should be the same as passing in each column as arguments to the function.
        if (is.call(substitute(...))) {
            object <- as.list(substitute(...))[-1]  # when a list() appears in the call to data.table, ie  data.table(list(1:10,letters[1:10]))
        } else {
            if (is.null(names(x)))
                object = as.list(rep("",length(x)))
            else
                object = as.list(names(x))
        }
    } else {
        object <- as.list(substitute(list(...)))[-1]  # used to get the expressions as text, for unnamed inputs.  So data.table(X,Y) will automatically put X and Y as the column names.
    }
    n <- length(x)
    if (n < 1)
        return(structure(list(), class = "data.table"))
    vnames <- names(x)
    if (length(vnames) != n)
        vnames <- character(n)
    no.vn <- nchar(vnames) == 0
    vnames <- as.list(vnames)
    nrows <- ncols <- integer(n)        # the nrows and ncols of each of the inputs (could be varying lengths)
    for (i in 1:n) {
        if (is.matrix(x[[i]]) || is.data.frame(x[[i]])) x[[i]] = as.data.table(x[[i]], keep.rownames=keep.rownames)       # TO DO: allow a matrix to be a column of a data.table. This could allow a key'd lookup to a matrix, not just by a single rowname vector, but by a combination of several columns. A matrix column could be stored either by row or by column contiguous in memory.
        if (is.vector(x[[i]])) x[[i]] = as.vector(x[[i]])   # to remove any vector names, otherwise they get silently retaining in the data.table list members.
        xi <- x[[i]]
        ncols[i] <- NCOL(xi)  # Returns 1 for vectors.
        namesi <- colnames(xi)  # works for both data.frame's, matrices and data.tables's
        if (ncols[i] > 1) {
            if (length(namesi) == 0)
                namesi <- 1:ncols[i]
            if (no.vn[i]) 
                vnames[[i]] <- namesi  # puts the column names of the input matrix, data.table, or data.frame into the output
            else vnames[[i]] <- paste(vnames[[i]], namesi, sep = ".")   # if it is named (eg data.table(1:10, "M"=mymat)) then append 'M' to the colnames
        }
        else {
            if (length(namesi) > 0) 
                vnames[[i]] <- namesi   # TO DO: If we do  data.table(1:10, "M"=mymat) where mymat is a one column named column, do we want 'M' appended? At the moment it isn't.
            else if (no.vn[[i]]) {
                # use the variable name itself e.g. data.table(X,Y) will automatically create columns named X and Y
                tmpname <- deparse(object[[i]])[1]
                if (substr(tmpname, 1, 2) == "I(") {
                  ntmpn <- nchar(tmpname)
                  if (substr(tmpname, ntmpn, ntmpn) == ")") 
                    tmpname <- substr(tmpname, 3, ntmpn - 1)
                }
                vnames[[i]] <- tmpname
            }
        }
        nrows[i] <- NROW(xi)    # for a vector returns the length
    }
    nr <- max(nrows)
    if (!all(nrows==nr)) stop("Columns differ in length. Silent repetition not yet implemented.")
    
    #for (i in (1:n)[nrows < nr]) {
    #    ## TO DO INSIDE HERE
    #    xi <- x[[i]]
    #    if (length(xi) == 1 && nrows[i] > 0 && nr%%nrows[i] == 
    #        0) {
    #        xi1 <- xi[[1]]
    #        if (is.vector(xi1) || is.factor(xi1)) {
    #            vlist[[i]] <- list(rep(xi1, length.out = nr))
    #            next
    #        }
    #        if (is.character(xi1) && class(xi1) == "AsIs") {
    #            cl <- class(xi1)
    #            vlist[[i]] <- list(structure(rep(xi1, length.out = nr), 
    #              class = cl))
    #            next
    #        }
    #    }
    #    stop("arguments imply differing number of rows: ", paste(unique(nrows), 
    #        collapse = ", "))
    #}
    # value <- unlist(vlist, recursive = FALSE, use.names = FALSE)
    if (any(ncols>1)) {
        value = list()
        k = 1
        for(i in 1:n) {
            if (is.list(x[[i]])) {
                for(j in 1:length(x[[i]])) {
                    value[[k]] = x[[i]][[j]]
                    k=k+1
                }
            } else {
                value[[k]] = x[[i]]
                k=k+1
            }
        }
    } else {
        value = x
    }
    vnames <- unlist(vnames[ncols > 0])
    noname <- nchar(vnames) == 0
    if (any(noname))
        vnames[noname] <- paste("Var", 1:length(vnames), sep = ".")[noname]
    if (check.names) 
        vnames <- make.names(vnames, unique = TRUE)
    names(value) <- vnames    
    attr(value, "class") <- "data.table"
    value
}

