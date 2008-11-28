setkey = function(x, ..., loc=parent.frame())
{
    # sorts table by the columns, and sets the key to be those columns
    # example of use:   setkey(tbl,colA,colC,colB)
    # will always change the table passed in by reference
    # thought about requiring caller to pass in as.ref() but figured neater to do it inside here.
    # TO DO: allow secondary index, which stores the sorted key + a column of integer rows in the primary sorted table. Interesting when it comes to table updates to maintain the keys.
    # TO DO: if key is already set, don't reset it.
    require(ref)
    name = deparse(substitute(x))
    if (!exists(name, env=loc)) loc=.GlobalEnv
    x = list(name=name, loc=loc)
    class(x) = "ref"
    if (class(deref(x))!="data.table") stop("first argument must be a data.table")
    if (any(sapply(deref(x),is.ff))) stop("joining to a table with ff columns is not yet implemented")
    cols = getdots()
    if (!length(cols)) {
        cols = colnames(deref(x))   #stop("Must supply one or more columns for the key")
    } else {
        miss = !(cols %in% colnames(deref(x)))
        if (any(miss)) stop("some columns are not in the table: " %+% cols[miss])
    }
    if (!all( sapply(deref(x),storage.mode)[cols] == "integer")) stop("All keyed columns must be storage mode integer")
    o = eval(parse(text=paste("deref(x)[,order(",paste(cols,collapse=","),",na.last=FALSE)]",sep="")))
    # We put NAs first because NA is internally a very large negative number. This is relied on in the C binary search.
    deref(x) = deref(x)[o]
    attr(deref(x),"sorted") = cols
    invisible()
}

J = function(...,SORTFIRST=FALSE) {
    # J because a little like the base function I(). Intended as a wrapper for subscript to DT[]
    JDT = data.table(...)
    for (i in 1:length(JDT)) if (storage.mode(JDT[[i]])=="double") mode(JDT[[i]])="integer"
    if (SORTFIRST) setkey(JDT)
    JDT
}
SJ = function(...) J(...,SORTFIRST=TRUE)
# S for Sorted, sorts the left table first.
# Note it may well be faster to do an unsorted join, rather than sort first involving a memory copy plus the
# sorting overhead. Often its clearer in the code to do an unsorted join anyway. Unsorted join uses less I-chache, at
# the expense of more page fetches. Very much data dependent, but the various methods are implemented so tests for the
# fastest method in each case can be performed.

CJ = function(...)
{
    # Pass in a list of unique values, e.g. ids and dates
    # Cross Join will then produce a join table with the combination of all values (cross product).
    # The last vector is varied the quickest in the table, so dates should be last for roll for example
    l = list(...)
    n = sapply(l,length)
    x=rev(take(cumprod(rev(n))))
    for (i in seq(along=x)) l[[i]] = rep(l[[i]],each=x[i])
    SJ(l)    # This will then expand out the vectors to fit.
}

# TO DO:  reliably mark tables as sorted so we don't need to sort again.  maybe this is simple one line change above to check for $sorted
