setkey = function(x, ..., verbose=getOption("datatable.verbose"))
{
    if (is.character(x)) stop("x may no longer be the character name of the data.table. The possibility was undocumented and has been removed.")
    cols = as.character(substitute(list(...))[-1])
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
    if (!length(cols)) {
        cols = colnames(x)   # All columns in the data.table, usually a few when used in this form
    } else {
        # remove backticks form cols 
        cols <- gsub("`", "", cols)
        miss = !(cols %in% colnames(x))
        if (any(miss)) stop("some columns are not in the data.table: " %+% cols[miss])
    }
    alreadykeyedbythiskey = identical(key(x),cols)
    if (".xi" %in% colnames(x)) stop("x contains a column called '.xi'. Conflicts with internal use by data.table.")
    for (i in cols) {
        .xi = x[[i]]  # [[ is copy on write, otherwise checking type would be copying each column
        if (!typeof(.xi) %chin% c("integer","logical","character","double")) stop("Column '",i,"' is type '",typeof(.xi),"' which is not supported as a key column type, currently.")
    }
    if (!is.character(cols) || length(cols)<1) stop("'cols' should be character at this point in setkey")
    # o = fastorder(x, cols, verbose=verbose)
    o = forder(x, cols, sort=TRUE, retGrp=FALSE)
    if (length(o)) {
        if (alreadykeyedbythiskey) warning("Already keyed by this key but had invalid row order, key rebuilt. If you didn't go under the hood please let datatable-help know so the root cause can be fixed.")
        .Call(Creorder,x,o)
    } # else empty integer() from forder means x is already ordered by those cols, nothing to do.
    if (!alreadykeyedbythiskey) setattr(x,"sorted",cols)   # the if() just to save plonking an identical vector into the attribute
    invisible(x)
}

key = function(x) attr(x,"sorted")

"key<-" = function(x,value) {
    warning("The key(x)<-value form of setkey can copy the whole table. This is due to <- in R itself. Please change to setkeyv(x,value) or setkey(x,...) which do not copy and are faster. See help('setkey'). You can safely ignore this warning if it is inconvenient to change right now. Setting options(warn=2) turns this warning into an error, so you can then use traceback() to find and change your key<- calls.")
    setkeyv(x,value)
    # The returned value here from key<- is then copied by R before assigning to x, it seems. That's
    # why we can't do anything about it without a change in R itself. If we return NULL (or invisible()) from this key<-
    # method, the table gets set to NULL. So, although we call setkeyv(x,cols) here, and that doesn't copy, the
    # returned value (x) then gets copied by R.
    # So, solution is that caller has to call setkey or setkeyv directly themselves, to avoid <- dispatch and its copy.
}

haskey = function(x) !is.null(key(x))

# reverse a vector by reference (no copy)
setrev <- function(x) .Call(Csetrev, x)

# reorder a vector based on 'order' (integer)
# to be used in fastorder instead of x[o], but in general, it's better to replace vector subsetting with this..?
# Basic checks that all items of order are in range 1:n with no NAs are now made inside Creorder.
# FOR INTERNAL USE ONLY
setreordervec <- function(x, order) .Call(Creorder, x, order)

# radixorder1 is used internally, only with fastorder
# so adding a new argument is okay. added 'o' for order vector
radixorder1 <- function(x, o=NULL) {
    if(is.object(x)) x = xtfrm(x) # should take care of handling factors, Date's and others, so we don't need unlist
    if (!is.null(o)) { # fix for http://stackoverflow.com/questions/21437546/data-table-1-8-11-and-aggregation-issues (moved this if-check to before checking logical)
        x = copy(x)
        setreordervec(x, o)
    }
    if(typeof(x) == "logical") return(c(which(is.na(x)),which(!x),which(x))) # logical is a special case of radix sort; just 3 buckets known up front. TO DO - could be faster in C but low priority
    if(typeof(x) != "integer") # this allows factors; we assume the levels are sorted as we always do in data.table
        stop("radixorder1 is only for integer 'x'")
    sort.list(x, na.last=FALSE, decreasing=FALSE,method="radix")
    # Always put NAs first, relied on in C binary search by relying on NA_integer_ being -maxint (checked in C).
}

# FOR INTERNAL use only.
# Note that implementing just "sort" (and not order) takes half of this time. Getting order seems to be more time-consuming
# slightly slower than R's (improperly named radix order) counting sort but: 
# 1) works for any data size - not restricted like R's radix where max-min should be <= 1e5 
# 2) with any values => also works on -ve integers, NA
# 3) directly returns sort value instead of sort order by setting last parameter in C function to FALSE (not accessible via iradixorder)
# 4) removed "decreasing=". Use 'setrev' instead to get the reversed order
iradixorder <- function(x, o=NULL) {
    # copied from radixorder1 and just changed the call to the correct function
    # xtfrm converts date object to numeric. but this will be called only if it's integer, so do a as.integer(.)
    if(is.object(x)) x = as.integer(xtfrm(x))
    if(typeof(x) == "logical") {
        if (!is.null(o)) { # since iradixorder requires a copy this check is better to be inside this if-statement unlike radixorder1
            x = copy(x)
            setreordervec(x, o)
        }
        return(c(which(is.na(x)), which(!x), which(x)))
    }
    if(typeof(x) != "integer") # this allows factors; we assume the levels are sorted as we always do in data.table
        stop("iradixorder is only for integer 'x'. Try dradixorder for numeric 'x'")
    if (length(x) == 0L) return(integer(0))
    # OLD: passing list(x) to C to ensure copy is being made...
    # NOTE: passing list(x) does not make a copy in >3.0.2 (devel version currently), so explicitly copying
    x = copy(x)
    if (!is.null(o)) setreordervec(x, o)
    ans <- .Call(Cfastradixint, x, TRUE) # TRUE returns indices, FALSE returns sorted value directly
    ans
    # NA first as data.table requires
}

# FOR INTERNAL use only.
# at least > 5-30x times faster than ordernumtol and order (depending on the number of groups to find the tolerance on)
# real-life performances must be towards the much faster side though.
dradixorder <- function(x, o=NULL, tol=.Machine$double.eps^0.5) {
    if (!is.atomic(x) || typeof(x) != "double") stop("'dradixorder' is only numeric 'x'")
    if (length(x) == 0) return(integer(0))
    # OLD: passing list(x) to C to ensure copy is being made...
    # NOTE: passing list(x) does not make a copy in >3.0.2 (devel version currently), so explicitly copying
    x = copy(x)
    if (!is.null(o)) setreordervec(x, o)
    ans <- .Call(Cfastradixdouble, x, as.numeric(tol), TRUE) # TRUE returns order, FALSE returns sorted vector.
    ans
    # NA first followed by NaN as data.table requires
}

regularorder1 <- function(x) {
    if(is.object(x)) x = xtfrm(x) # should take care of handling factors, Date's and others, so we don't need unlist
    sort.list(x, na.last=FALSE, decreasing=FALSE)
}

is.sorted = function(x, by=seq_along(x)) { 
    # Return value of TRUE/FALSE is relied on in data.table.R quite a bit. Simpler. Stick with that.
    # TO DO: This could be implemented as !length(forder(x,by)),  or, better, check calling use and avoid is.sorted as that's inside forder
    # TO DO: at least use twiddle in isSortedList.c rather than tolerance
    if (is.list(x)) {
        if (is.character(by)) by = chmatch(by,names(x))
        .Call(CisSortedList, x, as.integer(by), sqrt(.Machine$double.eps))
    } else {
        if (!missing(by) && !is.null(by)) stop("x is a single vector, non-NULL 'by' doesn't make sense.")
        identical(FALSE,is.unsorted(x)) && !(length(x)==1 && is.na(x))
    }
}
# base::is.unsorted returns NA if any NA is found anywhere, hence converting NA to FALSE above.
# The && is now needed to maintain backwards compatibility after r-devel's change of is.unsorted(NA) to FALSE (was NA) [May 2013].
# base::is.unsorted calls any(is.na(x)) at R level, could be avoided.
# TO DO: hook up our own vector is.sorted which checks NAs are just at the start, and then returns TRUE. Since, in data.table
# our rule is NA at the start.
# TO DO: instead of TRUE/FALSE, return -1/0/+1  -1=sorted in reverse, 0=not sorted either way, 1=sorted forwards. Conveniently, if (-1) in R is TRUE, since anything !=0 is TRUE, just like C.

forder = function(x, by=seq_along(x), retGrp=FALSE, sort=TRUE)
{
    if (!(sort || retGrp)) stop("At least one of retGrp or sort must be TRUE")
    # TO DO: export and document forder
    if (is.atomic(x)) {
        if (!missing(by) && !is.null(by)) stop("x is a single vector, non-NULL 'by' doesn't make sense")
        by = NULL
    } else {
        if (is.character(by)) by=chmatch(by, names(x))
        by = as.integer(by)
    }
    .Call(Cforder, x, by, retGrp, sort)  # returns integer() if already sorted, regardless of sort=TRUE|FALSE
}

fastorder <- function(x, by=seq_along(x), verbose=getOption("datatable.verbose"))
{
    # x can be a vector, or anything that's stored as a list (inc data.frame and data.table), thus can be accessed with non-copying base::[[.
    # When x is a list, 'by' may be integers or names
    # This function uses the backwards approach; i.e. first orders the last column, then orders the 2nd to last column ordered by the order of
    # the last column, and so on. This vectorized approach is much faster than base::order(...) [src/main/sort.c:ordervector(...,listgreater)]
    # which is a comparison sort comparing 2 rows using a loop through columns for that row with a switch on each column type.
    
    if (is.atomic(x)) by=NULL
    if (is.sorted(x, by=by)) return(NULL)  # callers need to check for NULL (meaning already sorted in increasing order)
    if (is.atomic(x)) { v = x; w = 1 }  # w = 1 just for the error message below
    else { w = last(by); v = x[[w]] }
    o = switch(typeof(v),
        "double" = dradixorder(v), # ordernumtol(v),
        "character" = chorder(v),
        # Use a radix sort (fast and stable for ties), but will fail for range > 1e5 elements (and any negatives in base)
        tryCatch(radixorder1(v),error=function(e) {
            if (verbose) cat("Column",w,"failed radixorder1, reverting to 'iradixorder'\n")
            iradixorder(v) # regularorder1(v)
        })
    )
    if (is.atomic(x)) return(o)
    # If there is more than one column, run through them backwards
    for (w in rev(take(by))) {
        v = x[[w]] # We could make the 'copy' here followed by 'setreordervec' 
                     # instead of creating 'chorder2'. But 'iradixorder' and 'dradixorder' 
                     # already take a copy internally So it's better to avoid copying twice.
        switch(typeof(v),
            "double" = setreordervec(o, dradixorder(v, o)), # PREV: o[dradixorder(v[o])], PPREV: o[ordernumto(v[o])]
            "character" = setreordervec(o, chorder2(v, o)), # TO DO: avoid the copy and reorder, pass in o to C like ordernumtol (still stands??)
            tryCatch(setreordervec(o, radixorder1(v, o)), error=function(e) {
                if (verbose) cat("Column",w,"failed radixorder1, reverting to 'iradixorder'\n")
                setreordervec(o, iradixorder(v, o))         # PREV: o[regularorder1(v[o])]
                                                            # TO DO: avoid the copy and reorder, pass in o to C like ordernumtol (still holds??)
            })
        )
    }
    o
}

ordernumtol = function(x, tol=.Machine$double.eps^0.5) {
    o=seq_along(x)
    .Call(Crorder_tol,x,o,tol)
    o
}

SJ = function(...) {
    JDT = as.data.table(list(...))
    setkey(JDT)
}
# S for Sorted, sorts the left table first.
# Note it may well be faster to do an unsorted join, rather than sort first involving a memory copy plus the
# sorting overhead. Often its clearer in the code to do an unsorted join anyway. Unsorted join uses less I-chache, at
# the expense of more page fetches. Very much data dependent, but the various methods are implemented so tests for the
# fastest method in each case can be performed.

# TO DO: Use the CJ list() method for SJ (and inside as.data.table.list?, #2109) too to avoid alloc.col

CJ = function(..., sorted = TRUE)
{
    # Pass in a list of unique values, e.g. ids and dates
    # Cross Join will then produce a join table with the combination of all values (cross product).
    # The last vector is varied the quickest in the table, so dates should be last for roll for example
    l = list(...)
    # for (i in seq_along(l)) if (storage.mode(l[[i]])=="double") mode(l[[i]])="integer"

    # using rep.int instead of rep speeds things up considerably (but attributes are dropped).
    j <- lapply(l, class) # changed "vapply" to avoid errors with "ordered" "factor" input
    if (length(l)==1L && sorted && !identical(is.unsorted(l[[1L]]),FALSE)) {
        l[[1L]] <- sort.int(l[[1L]], na.last=FALSE, method="quick")
    } else if (length(l) > 1L) {
        n = vapply(l, length, 0L)
        nrow = prod(n)
        x = c(rev(take(cumprod(rev(n)))), 1L)
        for (i in seq_along(x)) {
            y <- l[[i]]
            if (sorted && !identical(is.unsorted(y),FALSE))  # any NAs will cause a sort, even if they are at the beginning (can live with that)
                y <- sort.int(y, na.last=FALSE, method="quick") # no worries for ties because there are no row.names or attributes to worry about.
            if (i == 1L) 
                l[[i]] <- rep.int(y, times = rep.int(x[i], n[i]))   # i.e. rep(y, each=x[i])
            else if (i == length(n))
                l[[i]] <- rep.int(y, times = nrow/(x[i]*n[i]))
            else
                l[[i]] <- rep.int(rep.int(y, times = rep.int(x[i], 
                               n[i])), times = nrow/(x[i]*n[i]))
           if (any(class(l[[i]]) != j[[i]]))
               setattr(l[[i]], 'class', j[[i]]) # reset "Date" class - rep.int coerces to integer
        }
    }
    setattr(l, "row.names", .set_row_names(length(l[[1L]])))
    setattr(l, "class", c("data.table", "data.frame"))

    if (is.null(vnames <- names(l))) 
        vnames = vector("character", length(l)) 
    if (any(tt <- vnames == "")) {
        vnames[tt] = paste("V", which(tt), sep="")
        setattr(l, "names", vnames)
    }
    settruelength(l, 0L)
    l <- alloc.col(l)  # a tiny bit wasteful to over-allocate a fixed join table (column slots only), doing it anyway for consistency, and it's possible a user may wish to use SJ directly outside a join and would expect consistent over-allocation.
    if (sorted) 
        setattr(l, 'sorted', names(l))
    l
}


bench = function(quick=TRUE, testback=TRUE, baseline=FALSE) {
    if (baseline) testback=FALSE  # when baseline return in fastorder.c is uncommented, baseline must be TRUE
    # fastorder benchmark forwards vs backwards
    if (quick) {Sr = 1:3; Nr = 2:4} else {Sr = 1:5; Nr = 2:8}
    ans = setkey(CJ(Levels=as.integer(10^Sr),Rows=as.integer(10^Nr)))
    
    # TO DO:  add a case   S1 : 1e7 levels    S2 : 1:3   Every level has 3 rows.   1e7 calls to iradix
    #   Sr:  c(1:3, 10^(1:7))    CJ(Sr,Sr) all combinations
    
    ans[, SubGroupN:=format(as.integer(ceiling(Rows/Levels)), big.mark=",")]
    ans[,Rows:=format(Rows,big.mark=",")]
    ans[,Levels:=format(Levels,big.mark=",")]
    for (i in 1:nrow(ans)) {
        ttype = c("user.self","sys.self")  # elapsed can sometimes be >> user.self+sys.self. TO DO: three repeats as well.
        tol = 0.5                          # we don't mind about 0.5s; benefits when almost sorted outweigh
        S = ans[i,as.integer(gsub(",","",Levels))]
        N = ans[i,as.integer(gsub(",","",Rows))]
        DT = setDT(lapply(1:2, function(x){sample(S,N,replace=TRUE)}))
        
        if (testback || baseline) ans[i, rand.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]
        # in baseline mode, Cforder doesn't order, so y is needed to test baseline on ordered DT
        ans[i, rand.forw := sum(system.time(x<<-forder(DT))[ttype])]
        if (testback) ans[i, faster1 := rand.forw<rand.back+tol]
        if (testback) if (!identical(x,y)) browser()
        
        .Call(Creorder,DT, if (baseline) y else x)  # in baseline mode, x is deliberately wrong. And if testback=FALSE, we won't have y
        if (!is.sorted(DT)) stop("Logical error: ordered table is not sorted according to is.sorted!")
        if (baseline) ans[, rand.back:=NULL]
        
        if (testback) ans[i, ord.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]
        ans[i, ord.forw := sum(system.time(x<<-forder(DT))[ttype])]
        if (testback) ans[i, faster2 := ord.forw<ord.back+tol]
        if (testback) if (!identical(x,y)) browser()
        
        if (DT[[1]][1] == DT[[1]][2]) v = 2 else v = 1  # make small change to column 2, unless rows 1 and 2 aren't in the same group by column 1
        old = DT[[v]][1:2]
        DT[1:2, (v):=77:76]   # unsorted near the top to trigger full sort, is.sorted detects quickly.
        if (is.sorted(DT)) stop("Table is sorted. Change to the top didn't work.")
        
        if (testback) ans[i, ordT.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]   # T for Top
        ans[i, ordT.forw := sum(system.time(x<<-forder(DT))[ttype])]
        if (testback) ans[i, faster3 := ordT.forw<ordT.back+tol]
        if (testback) if (!identical(x,y)) browser()

        DT[1:2, (v):=old]          # undo the change at the top to make it sorted again
        if (!is.sorted(DT)) stop("Logical error: reverting the small change at the top didn't make DT ordered again")
        r = c(nrow(DT)-1, nrow(DT))
        if (DT[[1]][r[1]] == DT[[1]][r[2]]) v = 2 else v = 1
        old = DT[[v]][r]
        DT[r, (v):=77:76]    # unsorted near the very end, so is.sorted does full scan.
        if (is.sorted(DT)) stop("Table is sorted. Change to the very bottom didn't work.")
        
        if (testback) ans[i, ordB.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]   # B for Bottom
        ans[i, ordB.forw := sum(system.time(x<<-forder(DT))[ttype])]
        if (testback) ans[i, faster4 := ordB.forw<ordB.back+tol]
        if (testback) if (!identical(x,y)) browser()
        
        DT[r, (v):=old]          # undo the change at the top to make it sorted again
        if (!is.sorted(DT)) stop("Logical error: reverting the small change at the bottom didn't make DT ordered again")
        
        .Call(Creorder,DT,nrow(DT):1)   # Pefect reverse order, some sort algo's worst case e.g. O(n^2)
        if (is.sorted(DT)) stop("Logical error: reverse order of table is sorted according to is.sorted!")
        # Adding this test revealed the complexity that a reverse order vector containing ties, would not be stable if the reverse was applied. isorted fixed so that -1 returned only if strictly decreasing order
        
        if (testback) ans[i, rev.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]   # rev = reverse order
        ans[i, rev.forw := sum(system.time(x<<-forder(DT))[ttype])]
        if (testback) ans[i, faster5 := rev.forw<rev.back+tol]
        if (testback) if (!identical(x,y)) browser()
        
        if (i==nrow(ans) || ans[i+1,Levels]!=ans[i,Levels]) print(ans[Levels==Levels[i]])  # print each block as we go along
    }
    cat("\nFinished.\n\n")
    ans
}



