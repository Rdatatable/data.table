"as.data.frame.data.table" <-
function(x, ...)
{
    attr(x,"row.names") = as.character(1:nrow(x))
    # The row names alone may create a data.frame 10 times larger in memory than the data.table.
    # This is why data.tables can be 10 times faster, and can hold 10 times as many rows in the same amount of memory.
    # row names cannot be dropped from the data.frame class itself because the White Book defines them. See Prof Ripley's r-devel response 12 Dec 05.
    class(x) = "data.frame"
    x
}

