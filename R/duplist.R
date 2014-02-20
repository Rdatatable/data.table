
uniqlist <- function (l, order = -1L) 
{
    # Assumes input list is ordered by each list item (or by 'order' if supplied), and that all list elements are the same length
    # Finds the non-duplicate rows. Was called duplist but now grows vector - doesn't over-allocate result vector and
    # is >2x times faster on numeric types
    # TO DO: Possibly reinstate reverse argument :
    #    FALSE works in the usual duplicated() way,  the first in a sequence of dups, will be FALSE
    #    TRUE has the last in a sequence of dups FALSE (so you can keep the last if thats required)
    # l = list(...)
    if (!is.list(l)) 
        stop("l not type list")
    if (!length(l))  return(list(0L))
    ans <- .Call(Cuniqlist, l, as.integer(order))
    ans
}

# implemented for returning the lengths of groups obtained from uniqlist (for internal use only)
uniqlengths <- function(x, len) {
    # check for type happens in C, but still converting to integer here to be sure.
    ans <- .Call(Cuniqlengths, as.integer(x), as.integer(len))
    ans
}

