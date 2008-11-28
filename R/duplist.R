duplist = function(...,order)
{
    # Assumes input list is ordered by each list item (or by 'order' if supplied), and that all list elements are the same length
    # Finds the first non-duplicate rows.
    # TO DO: put back reverse argument
    # reverse=FALSE works in the usual duplicated() way,  the first in a sequence of dups, will be FALSE
    # reverse=TRUE has the last in a sequence of dups FALSE (so you can keep the last if thats required)
    l = list(...)
    ans = integer(length(l[[1]]))
    len = integer(1)
    if (missing(order)) {
        .Call("duplist",l,ans,len,as.integer(-1),PACKAGE="data.table")
    } else {
        .Call("duplist",l,ans,len,as.integer(order),PACKAGE="data.table")
    }
    length(ans) = len
    ans
    #m = sapply(l, function(col) {
    #    c(unclass(col)[-length(col)]==unclass(col)[-1])      # The unclass is required for factors, since MDs "[.factor" will now drop the unused level
    #})
    #if (reverse)
    #    c(apply(m,1,all),FALSE)
    #else
    #    c(FALSE,apply(m,1,all))
}
