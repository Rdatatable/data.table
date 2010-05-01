setkey = function(x, ..., loc=parent.frame())
{
    # sorts table by the columns, and sets the key to be those columns
    # example of use:   setkey(tbl,colA,colC,colB)
    #             or:   setkey("tbl",c("colA","colC","colB"))
    # changes the table passed in by reference
    # TO DO: allow secondary index, which stores the sorted key + a column of integer rows in the primary sorted table. Will need to either drop or maintain keys with updates or inserts.
    if (is.character(x)) {
        if (length(x)>1) stop("x is character vector length > 1")
        name = x
        if (!exists(name, env=loc)) loc=.GlobalEnv
        x = get(x,envir=loc,inherits=FALSE)
        cols=c(...)
    }  else {
        name = deparse(substitute(x))
        cols = getdots()
    }
    if (identical(key(x),cols)) return(invisible()) # table is already key'd by those columns
    if (!is.data.table(x)) stop("first argument must be a data.table")
    if (any(sapply(x,is.ff))) stop("joining to a table with ff columns is not yet implemented")
    if (!length(cols)) {
        cols = colnames(x)   # All columns in the data.table, usually a few when used in this form
    } else {
        miss = !(cols %in% colnames(x))
        if (any(miss)) stop("some columns are not in the data.table: " %+% cols[miss])
    }
    if (!all( sapply(x,storage.mode)[cols] == "integer")) stop("All keyed columns must be storage mode integer")
    for (i in cols) {
        # if key columns don't already have sorted levels, sort them, test 150
        if (is.factor(x[[i]])) {
            l = levels(x[[i]])
            if (is.unsorted(l)) {
                r = rank(l)
                l[r] = l
                x[[i]] = structure(r[as.integer(x[[i]])], levels=l, class="factor")
            }
        }
    }
    o = fastorder(x, cols, na.last=FALSE)
    # We put NAs first because NA is internally a very large negative number. This is relied on in the C binary search.
    ans = x[o]   # TO DO: implement column by column re-order here, so only memory for one column is required, rather than copy of whole table.
    attr(ans,"sorted") = cols
    assign(name,ans,envir=loc)
    invisible()
}

radixorder1 <- function(x, na.last = FALSE, decreasing = FALSE) {
    if(is.object(x)) x = xtfrm(x) # should take care of handling factors, Date's and others, so we don't need unlist
    if(!typeof(x) == "integer") # do want to allow factors here, where we assume the levels are sorted as we always do in data.table
        stop("radixorder1 is only for integer 'x'")
    if(is.na(na.last))
        return(.Internal(radixsort(x, TRUE, decreasing))[seq_len(sum(!is.na(x)))])
        # this is a work around for what we consider a bug in sort.list on vectors with NA (inconsistent with order)
    else
        return(.Internal(radixsort(x, na.last, decreasing)))
}

regularorder1 <- function(x, na.last = FALSE, decreasing = FALSE) {
    if(is.object(x)) x = xtfrm(x) # should take care of handling factors, Date's and others, so we don't need unlist
    if(is.na(na.last))
        return(.Internal(order(TRUE, decreasing, x))[seq_len(sum(!is.na(x)))])
    else
        return(.Internal(order(na.last, decreasing, x)))
}


fastorder <- function(lst, which=seq_along(lst), na.last = FALSE, decreasing = FALSE, verbose=getOption("datatable.verbose",FALSE))
{
    # lst is a list or anything thats stored as a list and can be accessed with [[.
    # 'which' may be integers or names
    # Its easier to pass arguments around this way, and we know for sure that [[ doesn't take a copy but l=list(...) might do.
    printdone=FALSE
    # Run through them back to front to group columns.
    w <- last(which)
    err <- try(silent = TRUE, {
        # Use a radix sort (fast and stable), but it will fail if there are more than 1e5 unique elements (or any negatives)
        o <- radixorder1(lst[[w]], na.last = na.last, decreasing = decreasing)
    })
    if (inherits(err, "try-error"))
        o <- regularorder1(lst[[w]], na.last = na.last, decreasing = decreasing)
    # If there is more than one column, run through them back to front to group columns.
    for (w in rev(take(which))) {
        err <- try(silent = TRUE, {
            o <- o[radixorder1(lst[[w]][o], na.last = na.last, decreasing = decreasing)]
        })
        if (inherits(err, "try-error"))
            o <- o[regularorder1(lst[[w]][o], na.last = na.last, decreasing = decreasing)]
    }
    if (printdone) {cat("done\n");flush.console()}   # TO DO - add time taken
    o
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

key = function(x) attr(x,"sorted")

"key<-" = function(x,value) {
    if (is.null(value)) attr(x,"sorted")=NULL
    else setkey("x",value)
    x
}

haskey = function(x) !is.null(key(x))


