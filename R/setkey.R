setkey = function(x, ..., loc=parent.frame(), verbose=getOption("datatable.verbose",FALSE))
{
    # sorts table by the columns, and sets the key to be those columns
    # example of use:   setkey(tbl,colA,colC,colB)
    #             or:   setkey("tbl",c("colA","colC","colB"))
    # changes the table passed in by reference
    # TO DO: allow secondary index, which stores the sorted key + a column of integer rows in the primary sorted table. Will need to either drop or maintain keys with updates or inserts.
    if (is.character(x)) {
        if (length(x)>1) stop("x is character vector length > 1")
        name = x
        if (!exists(name, envir=loc)) loc=.GlobalEnv
        x = get(x,envir=loc,inherits=FALSE)
        cols=c(...)
    }  else {
        name = deparse(substitute(x))
        cols = getdots()
    }
    if (!is.data.table(x)) stop("first argument must be a data.table")
    if (!length(cols)) {
        cols = colnames(x)   # All columns in the data.table, usually a few when used in this form
    } else {
        miss = !(cols %in% colnames(x))
        if (any(miss)) stop("some columns are not in the data.table: " %+% cols[miss])
    }
    alreadykeyed = identical(key(x),cols)
    copied = FALSE   # in future we hope to be able to setkeys on any type, this goes away, and saves more potential copies
    for (i in cols) {
        if (is.character(x[[i]])) {
            x[[i]] = factor(x[[i]])
            if (verbose) cat("setkey changed the type of column '",i,"' from character to factor.\n",sep="")
            copied=TRUE
            next
        }
        if (typeof(x[[i]]) == "double") {
            toint = as.integer(x[[i]])   # see [.data.table for similar logic, and comments
            if (isTRUE(all.equal(as.vector(x[[i]]),toint))) {
                mode(x[[i]]) = "integer"
                if (verbose) cat("setkey changed the type of column '",i,"' from numeric to integer, no fractional data present.\n",sep="")
                copied=TRUE
                next
            }
            stop("Column '",i,"' cannot be coerced to integer without losing fractional data.")
        }
        if (is.factor(x[[i]])) {
            # check levels are sorted, if not sort them, test 150
            l = levels(x[[i]])
            if (is.unsorted(l)) {
                r = rank(l)
                l[r] = l
                x[[i]] = structure(r[as.integer(x[[i]])], levels=l, class="factor")
                if (verbose) cat("setkey detected the levels of column '",i,"' were not sorted, so sorted them.\n",sep="")
                copied=TRUE
            }
            next
        }
        if (!typeof(x[[i]]) %in% c("integer","logical")) stop("Column '",i,"' is type '",typeof(x[[i]]),"' which is not accepted by setkey.")
    }
    if (!is.character(cols) || length(cols)<1) stop("'cols' should be character at this point in setkey")
    o = fastorder(x, cols, verbose=verbose)
    if (is.unsorted(o)) {
        if (alreadykeyed) warning("Already keyed by this key but had invalid row order, key rebuilt. If you didn't go under the hood please let maintainer('data.table') know so the root cause can be fixed.")
        .Call("reorder",x,o, PACKAGE="data.table")
    }
    if (!alreadykeyed) setattr(x,"sorted",cols)
    if (copied) {
        if (verbose) cat("setkey incurred a copy of the whole table, due to the coercion(s) above.\n")
        if (alreadykeyed) warning("Already keyed by this key but had invalid structure (e.g. unordered factor levels, or incorrect column types), key rebuilt. If you didn't go under the hood please let maintainer('data.table') know so the root cause can be fixed.")
        assign(name,x,envir=loc)
    }
    invisible(x)
}

radixorder1 <- function(x) {
    if(is.object(x)) x = xtfrm(x) # should take care of handling factors, Date's and others, so we don't need unlist
    if(typeof(x) == "logical") return(c(which(is.na(x)),which(!x),which(x))) # logical is a special case of radix sort; just 3 buckets known up front. TO DO - could be faster in C but low priority
    if(typeof(x) != "integer") # this allows factors; we assume the levels are sorted as we always do in data.table
        stop("radixorder1 is only for integer 'x'")
    # Actually, we never set na.last=NA. We rely that NA's are first in the C binary search, lets be safe ...
    # if(is.na(na.last))
    #    return(.Internal(radixsort(x, TRUE, decreasing))[seq_len(sum(!is.na(x)))])
    #    # this is a work around for what we consider a bug in sort.list on vectors with NA (inconsistent with order) reported to r-devel
    #else
    return(.Internal(radixsort(x, na.last=FALSE, decreasing=FALSE)))
}

regularorder1 <- function(x) {
    if(is.object(x)) x = xtfrm(x) # should take care of handling factors, Date's and others, so we don't need unlist
    #if(is.na(na.last))
    #    return(.Internal(order(TRUE, decreasing, x))[seq_len(sum(!is.na(x)))])
    #else
    return(.Internal(order(na.last=FALSE, decreasing=FALSE, x)))
}


fastorder <- function(lst, which=seq_along(lst), verbose=getOption("datatable.verbose",FALSE))
{
    # lst is a list or anything thats stored as a list and can be accessed with [[.
    # 'which' may be integers or names
    # It's easier to pass arguments around this way, and we know for sure that [[ doesn't take a copy but l=list(...) might do.
    # Run through them back to front to group columns.
    # This is a different approach to src/main/sort.c:ordervector(...,listgreater). In data.table, keys are
    # almost always only unique including the last column. There are a great number of repeats in the n-1 columns. So
    # listgreater would be doing a lot of type switching, skipping across columns in RAM, and getting == to previous
    # column value a lot. Here, in fastorder we order the whole column, column by column in reverse, updating
    # the very same 1:n vector that sort.c needs (but passing the previous columns order, not 1:n).
    w <- last(which)
    v = lst[[w]]
    if (is.double(v))
        o = ordernumtol(v)
    else {
        err = try(o <- radixorder1(v), silent=TRUE)
        # Use a radix sort (fast and stable), but it will fail if there are more than 1e5 unique elements (or any negatives)
        if (inherits(err, "try-error")) {
            if (verbose) cat("First column",w,"failed radixorder1, reverting to regularorder1\n")
            o = regularorder1(v)
        }
    }
    # If there is more than one column, run through them back to front to group columns.
    for (w in rev(take(which))) {
        v = lst[[w]]
        if (is.double(v))
            o = ordernumtol(v,o)
        else {
            err = try(o <- o[radixorder1(v[o])], silent=TRUE)
            if (inherits(err, "try-error")) {
                if (verbose) cat("Non-first column",w,"failed radixorder1, reverting to regularorder1\n")
                o = o[regularorder1(v[o])]    # TO DO: avoid the copy and reorder, pass in o to C like ordernumtol
            }
        }
    }
    o
}

ordernumtol = function(x, o=1:length(x), tol=.Machine$double.eps^0.5) {
    .Call("rorder_tol",x,o,tol)
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
    for (i in seq(along=l)) if (storage.mode(l[[i]])=="double") mode(l[[i]])="integer"
    if (length(l)>1) {
        n = sapply(l,length)
        x=rev(take(cumprod(rev(n))))
        m = x[1]*length(l[[1]])
        for (i in seq(along=x)) l[[i]] = rep(l[[i]],each=x[i],length=m)
    }
    setattr(l,"row.names",.set_row_names(length(l[[1]])))
    setattr(l,"class",c("data.table","data.frame"))
    names(l) = paste("V",1:length(l),sep="")
    setkey(l)
    l
}

key = function(x) attr(x,"sorted")

"key<-" = function(x,value) {
    if (is.null(value)) setattr(x,"sorted",NULL)
    else setkey("x",value)
    x
}

haskey = function(x) !is.null(key(x))


