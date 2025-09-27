"as.data.table.data.frame" <-
function(x, keep.rownames=FALSE)
{
    if (keep.rownames) return(data.table(rn=rownames(x), x, keep.rownames=FALSE))
    attr(x,"row.names") = NULL
    class(x) = "data.table"
    x
}

