"rbind.data.table" <-
function (...) {
    match.names <- function(clabs, nmi) {
        if (all(clabs == nmi)) 
            NULL
        else if (all(nii <- match(nmi, clabs, 0))) 
            nii
        else stop("names don't match previous names:\n\t", paste(nmi[nii == 
            0], collapse = ", "))
    }
    allargs <- list(...)
    allargs <- allargs[sapply(allargs, length) > 0]
    n <- length(allargs)
    if (n == 0)
        return(structure(list(), class = "data.table"))
        
    if (any(sapply(allargs, class) != "data.table")) stop("All arguments must be data.tables")
    if (length(unique(sapply(allargs, ncol))) != 1) stop("All data.tables must have the same number of columns")
    
    l = list()
    nm = names(allargs[[1]])
    if (length(nm) && n>1) {
        for (i in 2:n) if (length(names(allargs[[i]])) && !all(names(allargs[[i]]) == nm)) warning("colnames of argument ",i," don't match colnames of argument 1")
    }
    for (i in 1:length(allargs[[1]])) l[[i]] = unlist(sapply(allargs, "[[", i))
    names(l) = nm
    return(data.table(l))
    # much of the code in rbind.data.frame that follows this point is either to do with row.names, or coercing various types (and silent rep) which is already done by data.table. therefore removed.
}

