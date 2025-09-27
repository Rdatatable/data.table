"dimnames<-.data.table" <-
function (x, value)   # so that can do  colnames(dt)=<..>  as well as names(dt)=<..>
{
    if (!is.list(value) || length(value) != 2) stop("attempting to assign invalid object to dimnames of a data.table")
    if (!is.null(value[[1]])) stop("data.tables do not have rownames")
    if (ncol(x) != length(value[[2]])) stop("can't assign",length(value[[2]]),"colnames to a",ncol(x),"column data.table")
    names(x) <- as.character(value[[2]])
    x
}

