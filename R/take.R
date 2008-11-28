take = function(x, n=1)
{
    # returns the head of head, without the last n observations
    # convenient when inlining expressions
    # NROW, for vectors, returns the vector length, but we cater for non-table like lists also here
    # TO DO: allow taking along any dimension e.g. all but last column, rather than all but last row.
    if (is.list(x) && !is.data.frame(x)  && !is.data.table(x)) l = length(x)
    else l = NROW(x)
    if (l < n) stop("Cannot take ",n," from ",l)
    head(x, l-n)
}


# TO DO. Implement take as UseMethod. Specific methods for each type.
