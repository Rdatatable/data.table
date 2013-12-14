duplist = function(l,order,tolerance=.Machine$double.eps ^ 0.5)
{
    # Assumes input list is ordered by each list item (or by 'order' if supplied), and that all list elements are the same length
    # Finds the non-duplicate rows.
    # TO DO: Possibly reinstate reverse argument :
    #    FALSE works in the usual duplicated() way,  the first in a sequence of dups, will be FALSE
    #    TRUE has the last in a sequence of dups FALSE (so you can keep the last if thats required)
    # l = list(...)
    # TO DO - its wasteful that ans is allocated at long as the list, it just returns the first of each group.
    if (!is.list(l)) stop("l not type list")
    if (!(length(l) || length(l[[1]]))) return(0L)
    ans = integer(length(l[[1]]))
    len = integer(1)
    if (missing(order)) {
        .Call(Cduplist,l,ans,len,as.integer(-1),as.numeric(tolerance))
    } else {
        .Call(Cduplist,l,ans,len,as.integer(order),as.numeric(tolerance))
    }
    length(ans) = len
    ans
}

# Faster duplist - grows vector - doesn't over-allocate result vector and is >2x times faster on numeric types
uniqlist <- function (l, order = -1L, tolerance = .Machine$double.eps^0.5) 
{
    # Assumes input list is ordered by each list item (or by 'order' if supplied), and that all list elements are the same length
    # Finds the non-duplicate rows.
    # TO DO: Possibly reinstate reverse argument :
    #    FALSE works in the usual duplicated() way,  the first in a sequence of dups, will be FALSE
    #    TRUE has the last in a sequence of dups FALSE (so you can keep the last if thats required)
    # l = list(...)
    if (!is.list(l)) 
        stop("l not type list")
    if (!length(l))  return(list(0L))
    ans <- .Call(Cuniqlist, l, as.integer(order), as.numeric(tolerance))
    ans
}

# wrapper for Cuniqlengths (for internal use only)
# implemented for returning the lengths of groups obtained from uniqlist
uniqlengths <- function(x, len) {
    # check for type happens in C, but still converting to integer here to be sure.
    ans <- .Call(Cuniqlengths, as.integer(x), as.integer(len))
    ans
}

