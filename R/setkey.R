setkey = function(x, ..., verbose=getOption("datatable.verbose"))
{
    if (is.character(x)) stop("x may no longer be the character name of the data.table. The possibility was undocumented and has been removed.")
    cols = getdots()
    if (!length(cols)) cols=colnames(x)
    else if (identical(cols,"NULL")) cols=NULL
    setkeyv(x,cols,verbose=verbose)
}

setkeyv = function(x, cols, verbose=getOption("datatable.verbose"))
{
    if (is.null(cols)) {   # this is done on a data.frame when !cedta at top of [.data.table
        setattr(x,"sorted",NULL)
        return(x)
    }
    if (!is.data.table(x)) stop("x is not a data.table")
    if (!is.character(cols)) stop("cols is not a character vector. Please see further information in ?setkey.")
    if (!length(cols)) {
        warning("cols is a character vector of zero length. Removed the key, but use NULL instead, or wrap with suppressWarnings() to avoid this warning.")
        setattr(x,"sorted",NULL)
        return(x)
    }
    if (identical(cols,"")) stop("cols is the empty string. Use NULL to remove the key.")
    if (any(nchar(cols)==0)) stop("cols contains some blanks.")
    if (length(grep(",",cols))) stop("Don't use comma inside quotes. Please see the examples in help('setkey')") 
    if (!length(cols)) {
        cols = colnames(x)   # All columns in the data.table, usually a few when used in this form
    } else {
        miss = !(cols %in% colnames(x))
        if (any(miss)) stop("some columns are not in the data.table: " %+% cols[miss])
    }
    alreadykeyedbythiskey = identical(key(x),cols)
    coerced = FALSE   # in future we hope to be able to setkeys on any type, this goes away, and saves more potential copies
    if (".xi" %in% colnames(x)) stop("x contains a column called '.xi'. Conflicts with internal use by data.table.")
    for (i in cols) {
        .xi = x[[i]]  # TO DO: check that [[ is copy on write, otherwise checking types itself will be copying each column.
        #if (is.character(.xi)) {
        #    if (verbose) cat("setkey changing the type of column '",i,"' from character to factor by reference.\n",sep="")
        #    x[,i:=factor(.xi),with=FALSE]
        #    coerced=TRUE
        #    next
        #}
        #if (typeof(.xi) == "double") {
            # TO DO: use reallyreal to issue warning that int might be more appropriate
            
        #    toint = as.integer(.xi)   # see [.data.table for similar logic, and comments
        #    if (isTRUE(all.equal(as.vector(.xi),toint))) {
        #        if (verbose) cat("setkey changing the type of column '",i,"' from numeric to integer by reference, no fractional data present.\n",sep="")
        #        mode(.xi) = "integer"
        #        x[,i:=.xi,with=FALSE]
        #        coerced=TRUE
        #        next
        #    }
        #    stop("Column '",i,"' cannot be coerced to integer without losing fractional data.")
        #}
        #if (is.factor(.xi)) {
        #    # check levels are sorted, if not sort them, test 150
        #    # TO DO ... do we need sorted levels now that we use chmatch rather than sortedmatch?
        #    #           good to allow unsorted levels for convenience to avoid "1. orange", "2. apple" workaround
        #    l = levels(.xi)
        #    if (is.unsorted(l)) {
        #        if (verbose) cat("setkey detected the levels of column '",i,"' were not sorted, so sorting them, by reference.\n",sep="")
        #        r = rank(l)
        #        l[r] = l
        #        .xi = structure(r[as.integer(.xi)], levels=l, class="factor")
        #        x[,i:=.xi,with=FALSE]
        #        coerced=TRUE
        #    }
        #    next
        #}
        if (!typeof(.xi) %chin% c("integer","logical","character","double")) stop("Column '",i,"' is type '",typeof(.xi),"' which is not (currently) allowed as a key column type.")
    }
    if (!is.character(cols) || length(cols)<1) stop("'cols' should be character at this point in setkey")
    o = fastorder(x, cols, verbose=verbose)
    if (is.unsorted(o)) {
        if (alreadykeyedbythiskey) warning("Already keyed by this key but had invalid row order, key rebuilt. If you didn't go under the hood please let maintainer('data.table') know so the root cause can be fixed.")
        .Call("reorder",x,o, PACKAGE="data.table")
    }
    if (!alreadykeyedbythiskey) setattr(x,"sorted",cols)   # the if() just to be a tiny bit faster
    if (coerced && alreadykeyedbythiskey) {
        # if (verbose) cat("setkey incurred a copy of the whole table, due to the coercion(s) above.\n")
        warning("Already keyed by this key but had invalid structure (e.g. unordered factor levels, or incorrect column types), key rebuilt. If you didn't go under the hood please let maintainer('data.table') know so the root cause can be fixed.")
    }
    invisible(x)
}

key = function(x) attr(x,"sorted")

"key<-" = function(x,value) {
    warning("The key(x)<-value form of setkey copies the whole table. This is due to <- in R itself. Please change to setkeyv(x,value) or setkey(x,...) which do not copy and are faster. See help('setkey'). You can safely ignore this warning if it is inconvenient to change right now. Setting options(warn=2) turns this warning into an error, so you can then use traceback() to find and change your key<- calls.")
    setkeyv(x,value)
    # The returned value here from key<- is then copied by R before assigning to x, it seems. That's
    # why we can't do anything about it without a change in R itself. If we return NULL (or invisible()) from this key<-
    # method, the table gets set to NULL. So, although we call setkeyv(x,cols) here, and that doesn't copy, the
    # returned value (x) then gets copied by R.
    # So, solution is that caller has to call setkey or setkeyv directly themselves, to avoid <- dispatch and its copy.
}

haskey = function(x) !is.null(key(x))


radixorder1 <- function(x) {
    if(is.object(x)) x = xtfrm(x) # should take care of handling factors, Date's and others, so we don't need unlist
    if(typeof(x) == "logical") return(c(which(is.na(x)),which(!x),which(x))) # logical is a special case of radix sort; just 3 buckets known up front. TO DO - could be faster in C but low priority
    if(typeof(x) != "integer") # this allows factors; we assume the levels are sorted as we always do in data.table
        stop("radixorder1 is only for integer 'x'")
    sort.list(x, na.last=FALSE, decreasing=FALSE,method="radix")
    # Always put NAs first, relied on in C binary search by relying on NA_integer_ being -maxint (checked in C).
}

regularorder1 <- function(x) {
    if(is.object(x)) x = xtfrm(x) # should take care of handling factors, Date's and others, so we don't need unlist
    sort.list(x, na.last=FALSE, decreasing=FALSE)
}


fastorder <- function(lst, which=seq_along(lst), verbose=getOption("datatable.verbose"))
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
    o = switch(typeof(v),
        "double" = ordernumtol(v),
        "character" = chorder(v),
        # Use a radix sort (fast and stable for ties), but will fail for range > 1e5 elements (and any negatives)
        tryCatch(radixorder1(v),error=function(e) {
            if (verbose) cat("First column",w,"failed radixorder1, reverting to regularorder1\n")
            regularorder1(v)
        })
    )
    # If there is more than one column, run through them back to front to group columns.
    for (w in rev(take(which))) {
        v = lst[[w]]
        o = switch(typeof(v),
            "double" = ordernumtol(v, o),   # o is changed by reference by ordernumtol, and returned too
            "character" = o[chorder(v[o])],   # TO DO: avoid the copy and reorder, pass in o to C like ordernumtol
            tryCatch(o[radixorder1(v[o])], error=function(e) {
                if (verbose) cat("Non-first column",w,"failed radixorder1, reverting to regularorder1\n")
                o[regularorder1(v[o])]    # TO DO: avoid the copy and reorder, pass in o to C like ordernumtol
            })
        )
    }
    o
}

ordernumtol = function(x, o=seq_along(x), tol=.Machine$double.eps^0.5) {
    .Call("rorder_tol",x,o,tol,PACKAGE="data.table")
    o
}

J = function(...,SORTFIRST=FALSE) {
    # J because a little like the base function I(). Intended as a wrapper for subscript to DT[]
    JDT = data.table(...)   # TO DO: can this be list() for efficiency instead?
    # TO DO: delete... for (i in 1:length(JDT)) if (storage.mode(JDT[[i]])=="double") mode(JDT[[i]])="integer"
    if (SORTFIRST) setkey(JDT)
    JDT
}
SJ = function(...) J(...,SORTFIRST=TRUE)
# S for Sorted, sorts the left table first.
# Note it may well be faster to do an unsorted join, rather than sort first involving a memory copy plus the
# sorting overhead. Often its clearer in the code to do an unsorted join anyway. Unsorted join uses less I-chache, at
# the expense of more page fetches. Very much data dependent, but the various methods are implemented so tests for the
# fastest method in each case can be performed.

# TO DO: Use the CJ list() method for J and SJ too to avoid alloc.col

CJ = function(...)
{
    # Pass in a list of unique values, e.g. ids and dates
    # Cross Join will then produce a join table with the combination of all values (cross product).
    # The last vector is varied the quickest in the table, so dates should be last for roll for example
    l = list(...)
    # for (i in seq(along=l)) if (storage.mode(l[[i]])=="double") mode(l[[i]])="integer"
    if (length(l)>1) {
        n = sapply(l,length)
        nrow = prod(n)
        x=c(rev(take(cumprod(rev(n)))),1L)
        for (i in seq(along=x)) l[[i]] = rep(l[[i]],each=x[i],length=nrow)
    }
    setattr(l,"row.names",.set_row_names(length(l[[1]])))
    setattr(l,"class",c("data.table","data.frame"))
    vnames = names(l)
    if (is.null(vnames)) vnames=rep("",length(l))
    tt = vnames==""
    if (any(tt)) {
        vnames[tt] = paste("V", which(tt), sep="")
        setattr(l,"names",vnames)
    }
    settruelength(l,0L)
    l=alloc.col(l)  # a tiny bit wasteful to over-allocate a fixed join table (column slots only), doing it anyway for consistency, and it's possible a user may wish to use SJ directly outside a join and would expect consistent over-allocation.
    setkey(l)    # TO DO: if inputs are each !is.unsorted, then no need to setkey here, just setattr("sorted") to save sort
    l
}



