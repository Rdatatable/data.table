"as.data.table" <-
function(x, keep.rownames=FALSE)
{
    if (is.null(x))
        return(as.data.table(list()))
    UseMethod("as.data.table")
}

