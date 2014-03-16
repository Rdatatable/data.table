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
    if (verbose) {
        tt = system.time(o <- forderv(x, cols, sort=TRUE, retGrp=FALSE))  # system.time does a gc, so we don't want this always on, until refcnt is on by default in R
        cat("forder took", tt["user.self"]+tt["sys.self"], "sec\n")
    } else {
        o <- forderv(x, cols, sort=TRUE, retGrp=FALSE)
    }
    if (length(o)) {
        if (alreadykeyedbythiskey) warning("Already keyed by this key but had invalid row order, key rebuilt. If you didn't go under the hood please let datatable-help know so the root cause can be fixed.")
        if (verbose) {
            tt = system.time(.Call(Creorder,x,o))
            cat("reorder took", tt["user.self"]+tt["sys.self"], "sec\n")
        } else {
            .Call(Creorder,x,o)
        }
    } else {
        if (verbose) cat("x is already ordered by these columns, no need to call reorder\n")
    } # else empty integer() from forderv means x is already ordered by those cols, nothing to do.
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

# sort = sort.int = sort.list = order = is.unsorted = function(...)
#    stop("Should never be called by data.table internals. Use is.sorted() on vectors, or forder() for lists and vectors.")
# Nice idea, but users might use these in i or j e.g. blocking order caused tests 304 to fail.
# Maybe just a grep through *.R for use of these function internally would be better (TO DO).

# Don't use base::is.unsorted internally, because :
#    1) it returns NA if any(is.na(.)) where NAs are detected at R level, inefficiently
#    2) it uses locale whereas in data.table we control locale sorting independently (C locale currently, but
#       "sorted" attribute will need an extra attribute "locale" so we can check if key's locale is the current locale)
#    3) wrapper needed, used to be :
#       identical(FALSE,is.unsorted(x)) && !(length(x)==1 && is.na(x))
#       where the && was needed to maintain backwards compatibility after r-devel's change of is.unsorted(NA) to FALSE (was NA) [May 2013].
# The others (order, sort.int etc) are turned off to protect ourselves from using them internally, for speed and for
# consistency; e.g., consistent twiddling of numeric/integer64, NA at the beginning of integer, locale ordering of character vectors.

is.sorted = function(x, by=seq_along(x)) {
    if (is.list(x)) {
        warning("Use 'if (length(o<-forderv(DT,by))) ...' for efficiency in one step, so you have o as well if not sorted.")
        # could pass through a flag for forderv to return early on first FALSE. But we don't need that internally
        # since internally we always then need ordering, an it's better in one step. Don't want inefficiency to creep in.
        # This is only here for user/debugging use to check/test valid keys; e.g. data.table:::is.sorted(DT,by)
        0 == length(forderv(x,by,retGrp=FALSE,sort=TRUE))
    } else {
        if (!missing(by)) stop("x is vector but 'by' is supplied")
        .Call(Cfsorted, x)
    }
    # Cfsorted could be named CfIsSorted, but since "sorted" is an adjective not verb, it's clear; e.g., Cfsort would sort it ("sort" is verb).
    # Return value of TRUE/FALSE is relied on in [.data.table quite a bit on vectors. Simple. Stick with that (rather than -1/0/+1)
    # Important to call forder.c::fsorted here, for consistent character ordering and numeric/integer64 twiddling.
}

forderv = function(x, by=seq_along(x), retGrp=FALSE, sort=TRUE, order=1L)
{
    if (!(sort || retGrp)) stop("At least one of retGrp or sort must be TRUE")
    # TO DO: export and document forder
    if (is.atomic(x)) {
        if (!missing(by) && !is.null(by)) stop("x is a single vector, non-NULL 'by' doesn't make sense")
        by = NULL
        if ( !missing(order) && (length(order) != 1L || !(order %in% c(1L, -1L))) )
            stop("x is a single vector, length(order) must be =1 and it's value should be 1 (ascending) or -1 (descending).")
    } else {
        if (!length(x)) return(integer(0)) # to be consistent with base:::order. this'll make sure forderv(NULL) will result in error 
                                           # (as base does) but forderv(data.table(NULL)) and forderv(list()) will return integer(0))
        if (is.character(by)) by=chmatch(by, names(x))
        by = as.integer(by)
        if ( (length(order) != 1L && length(order) != length(by)) || any(!order %in% c(1L, -1L)) )
            stop("x is a list, length(order) must be either =1 or =length(by) and each value should be 1 or -1 for each column in 'by', corresponding to ascending or descending order, respectively. If length(order) == 1, it will be recycled to length(by).")
        if (length(order) == 1L) order = rep(order, length(by))
    }
    order = as.integer(order)
    .Call(Cforder, x, by, retGrp, sort, order)  # returns integer() if already sorted, regardless of sort=TRUE|FALSE
}

forder = function(x, ..., decreasing=FALSE)
{
    if (!is.data.table(x)) stop("x must be a data.table.")
    if (ncol(x) == 0) stop("Attempting to order a 0-column data.table.")
    if (is.na(decreasing) || !is.logical(decreasing)) stop("'decreasing' must be logical TRUE or FALSE")
    cols = substitute(list(...))[-1]
    if (identical(as.character(cols),"NULL") || !length(cols)) return(NULL) # to provide the same output as base:::order
    ans = x
    order = rep(1L, length(cols))
    if (length(cols)) {
        ans = vector("list", length(cols))
        cols = as.list(cols)
        xcols = names(x)
        for (i in seq_along(cols)) {
            v=cols[[i]]
            if (i == 1L && is.call(v) && length(v) == 2L && v[[1L]] == "list") return(1L) # to be consistent with base, see comment below under while loop
            while (is.call(v) && length(v) == 2L && v[[1L]] != "list") { 
                # take care of "--x", "{-x}", "(---+x)" etc., cases and also "list(y)". 'list(y)' is ambiguous though. In base, with(DT, order(x, list(y))) will error 
                # that 'arguments are not of same lengths'. But with(DT, order(list(x), list(y))) will return 1L, which is very strange. On top of that, with(DT, 
                # order(x, as.list(10:1)) would return 'unimplemented type list'. It's all very inconsistent. But we HAVE to be consistent with base HERE.
                if (v[[1L]] == "-") order[i] = -order[i]
                v = v[[-1L]]
            }
            if (is.name(v)) {
                ix <- chmatch(as.character(v), xcols, nomatch=0L)
                if (ix != 0L) ans <- point(ans, i, x, ix) # see 'point' in data.table.R and C-version pointWrapper in assign.c - avoid copies
                else {
                    v = as.call(list(as.name("list"), v))
                    ans <- point(ans, i, eval(v, x, parent.frame()), 1L)
                }
            } else {
                v = as.call(list(as.name("list"), v))
                ans <- point(ans, i, eval(v, x, parent.frame()), 1L) # eval has to make a copy here (not due to list(.), but due to ex: "4-5*y"), unavoidable.
            } # else stop("Column arguments to order by in 'forder' should be of type name/symbol (ex: quote(x)) or call (ex: quote(-x), quote(x+5*y))")
        }
    }
    cols = seq_along(ans)
    for (i in cols) {
        if (!typeof(ans[[i]]) %chin% c("integer","logical","character","double")) 
            stop("Column '",i,"' is type '",typeof(ans[[i]]),"' which is not supported for ordering currently.")
    }
    o = forderv(ans, cols, sort=TRUE, retGrp=FALSE, order= if (decreasing) -order else order)
    if (!length(o)) o = seq_along(ans[[1L]]) else o
    o
}

setorder = function(x, ...)
{
    if (!is.data.table(x)) stop("x must be a data.table.")
    cols = substitute(list(...))[-1]
    if (identical(as.character(cols),"NULL")) return(x)
    if (length(cols)) {
        cols=as.list(cols)
        order=rep(1L, length(cols))
        for (i in seq_along(cols)) {
            v=as.list(cols[[i]])
            if (length(v) > 1 && v[[1L]] == "+") v=v[[-1L]]
            else if (length(v) > 1 && v[[1L]] == "-") {
                v=v[[-1L]]
                order[i] = -1L
            }
            cols[[i]]=as.character(v)
        }
        cols=unlist(cols, use.names=FALSE)
    } else {
        cols=colnames(x)
        order=rep(1L, length(cols))
    }
    setorderv(x, cols, order)
}

setorderv = function(x, cols, order=1L)
{
    if (is.null(cols)) return(x)
    if (!is.data.table(x)) stop("x is not a data.table")
    if (!is.character(cols)) stop("cols is not a character vector. Please see further information in ?setorder.")
    if (!length(cols)) {
        warning("cols is a character vector of zero length. Use NULL instead, or wrap with suppressWarnings() to avoid this warning.")
        return(x)
    }
    if (any(nchar(cols)==0)) stop("cols contains some blanks.")     # TODO: probably I'm checking more than necessary here.. there are checks in 'forderv' as well
    if (!length(cols)) {
        cols = colnames(x)   # All columns in the data.table, usually a few when used in this form
    } else {
        # remove backticks from cols 
        cols <- gsub("`", "", cols)
        miss = !(cols %in% colnames(x))
        if (any(miss)) stop("some columns are not in the data.table: " %+% cols[miss])
    }
    if (".xi" %in% colnames(x)) stop("x contains a column called '.xi'. Conflicts with internal use by data.table.")
    for (i in cols) {
        .xi = x[[i]]  # [[ is copy on write, otherwise checking type would be copying each column
        if (!typeof(.xi) %chin% c("integer","logical","character","double")) stop("Column '",i,"' is type '",typeof(.xi),"' which is not supported for ordering currently.")
    }
    if (!is.character(cols) || length(cols)<1) stop("'cols' should be character at this point in setkey.")

    o = forderv(x, cols, sort=TRUE, retGrp=FALSE, order=order)
    if (length(o)) {
        .Call(Creorder, x, o)
        setattr(x, 'sorted', NULL) # if 'forderv' is not 0-length, it means order has changed. So, set key to NULL, else retain key.
    }
    invisible(x)
}

twiddle = function(x) {
    .Call(Ctwiddlewrapper, x)
}

SJ = function(...) {
    JDT = as.data.table(list(...))
    setkey(JDT)
}
# S for Sorted, usually used in i to sort the i table

# TO DO?: Use the CJ list() replication method for SJ (inside as.data.table.list?, #2109) too to avoid alloc.col

CJ = function(..., sorted = TRUE)
{
    # Pass in a list of unique values, e.g. ids and dates
    # Cross Join will then produce a join table with the combination of all values (cross product).
    # The last vector is varied the quickest in the table, so dates should be last for roll for example
    l = list(...)

    # using rep.int instead of rep speeds things up considerably (but attributes are dropped).
    j = lapply(l, class)  # changed "vapply" to avoid errors with "ordered" "factor" input
    if (length(l)==1L && sorted && length(o <- forderv(l[[1L]])))
        l[[1L]] = l[[1L]][o]
    else if (length(l) > 1L) {
        n = vapply(l, length, 0L)
        nrow = prod(n)
        x = c(rev(take(cumprod(rev(n)))), 1L)
        for (i in seq_along(x)) {
            y = l[[i]]
            if (sorted && length(o <- forderv(y))) y = y[o]
            if (i == 1L) 
                l[[i]] = rep.int(y, times = rep.int(x[i], n[i]))   # i.e. rep(y, each=x[i])
            else if (i == length(n))
                l[[i]] = rep.int(y, times = nrow/(x[i]*n[i]))
            else
                l[[i]] = rep.int(rep.int(y, times = rep.int(x[i], n[i])), times = nrow/(x[i]*n[i]))
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
    l <- alloc.col(l)  # a tiny bit wasteful to over-allocate a fixed join table (column slots only), doing it anyway for consistency, and it's possible a user may wish to use SJ directly outside a join and would expect consistent over-allocation.
    if (sorted) setattr(l, 'sorted', names(l))
    l
}



#########################################################################################
# Deprecated ...
#########################################################################################



bench = function(quick=TRUE, testback=TRUE, baseline=FALSE) {
    if (baseline) testback=FALSE  # when baseline return in fastorder.c is uncommented, baseline must be TRUE
    # fastorder benchmark forwards vs backwards
    
    Levels=Rows=SubGroupN=rand.forw=rand.back=ordT.forw=ordT.back=ordB.forw=ordB.back=rev.forw=rev.back=NULL  
    x=y=faster1=faster2=faster3=faster4=NULL  # to keep R CMD check quiet
    
    if (quick) {Sr = 1:3; Nr = 2:4} else {Sr = 1:5; Nr = 2:8}
    ans = setkey(CJ(Levels=as.integer(10^Sr),Rows=as.integer(10^Nr)))
    
    # TO DO:  add a case   S1 : 1e7 levels    S2 : 1:3   Every level has 3 rows.   1e7 calls to iradix
    #   Sr:  c(1:3, 10^(1:7))    CJ(Sr,Sr) all combinations

    # fastorder (backwards) doesn't call isSortedList anymore (we removed that at C level). It now proceeds as if
    # unsorted always. This is always more favourable to fastorder timings, unless, the data is actually perfectly
    # sorted. Hence we no longer test perfectly ordered here, as that was just testing isSortedList anyway. ordT
    # and ordB tests dominate.

    ans[, SubGroupN:=format(as.integer(ceiling(Rows/Levels)), big.mark=",")]
    ans[,Rows:=format(Rows,big.mark=",")]
    ans[,Levels:=format(Levels,big.mark=",")]
    ident = function(x,y) if (length(x)==0) length(y)==0 || identical(y,seq_along(y)) else identical(x,y)
    IS.SORTED = function(...)suppressWarnings(is.sorted(...))  # just needed here to ensure the test data construction is working
    for (i in 1:nrow(ans)) {
        ttype = c("user.self","sys.self")  # elapsed can sometimes be >> user.self+sys.self. TO DO: three repeats as well.
        tol = 0.5                          # we don't mind about 0.5s; benefits when almost sorted outweigh
        S = ans[i,as.integer(gsub(",","",Levels))]
        N = ans[i,as.integer(gsub(",","",Rows))]
        DT = setDT(lapply(1:2, function(x){sample(S,N,replace=TRUE)}))
        
        if (testback || baseline) ans[i, rand.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]
        # in baseline mode, Cforder doesn't order, so y is needed to test baseline on ordered DT
        ans[i, rand.forw := sum(system.time(x<<-forderv(DT))[ttype])]
        if (testback) ans[i, faster1 := rand.forw<rand.back+tol]
        if (testback) if (!ident(x,y)) browser()
        
        .Call(Creorder,DT, if (baseline) y else x)  # in baseline mode, x is deliberately wrong. And if testback=FALSE, we won't have y
        if (!IS.SORTED(DT)) stop("Logical error: ordered table is not sorted according to IS.SORTED!")
        if (baseline) ans[, rand.back := NULL]
        
        if (FALSE) {
          # don't test perfectly ordered case. See note above.
          if (testback) ans[i, ord.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]
          ans[i, ord.forw := sum(system.time(x<<-forderv(DT))[ttype])]
          if (testback) ans[i, faster2 := ord.forw<ord.back+tol]
          if (testback) if (!ident(x,y)) browser()
        }
        
        if (DT[[1]][1] == DT[[1]][2]) v = 2 else v = 1  # make small change to column 2, unless rows 1 and 2 aren't in the same group by column 1
        old = DT[[v]][1:2]
        DT[1:2, (v):=77:76]   # unsorted near the top to trigger full sort, is.sorted detects quickly.
        if (IS.SORTED(DT)) stop("Table is sorted. Change to the top didn't work.")
        
        if (testback) ans[i, ordT.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]   # T for Top
        ans[i, ordT.forw := sum(system.time(x<<-forderv(DT))[ttype])]
        if (testback) ans[i, faster2 := ordT.forw<ordT.back+tol]
        if (testback) if (!ident(x,y)) browser()

        DT[1:2, (v):=old]          # undo the change at the top to make it sorted again
        if (!IS.SORTED(DT)) stop("Logical error: reverting the small change at the top didn't make DT ordered again")
        r = c(nrow(DT)-1, nrow(DT))
        if (DT[[1]][r[1]] == DT[[1]][r[2]]) v = 2 else v = 1
        old = DT[[v]][r]
        DT[r, (v):=77:76]    # unsorted near the very end, so is.sorted does full scan.
        if (IS.SORTED(DT)) stop("Table is sorted. Change to the very bottom didn't work.")
        
        if (testback) ans[i, ordB.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]   # B for Bottom
        ans[i, ordB.forw := sum(system.time(x<<-forderv(DT))[ttype])]
        if (testback) ans[i, faster3 := ordB.forw<ordB.back+tol]
        if (testback) if (!ident(x,y)) browser()
        
        DT[r, (v):=old]          # undo the change at the top to make it sorted again
        if (!IS.SORTED(DT)) stop("Logical error: reverting the small change at the bottom didn't make DT ordered again")
        
        .Call(Creorder,DT,nrow(DT):1)   # Pefect reverse order, some sort algo's worst case e.g. O(n^2)
        if (IS.SORTED(DT)) stop("Logical error: reverse order of table is sorted according to IS.SORTED!")
        # Adding this test revealed the complexity that a reverse order vector containing ties, would not be stable if the reverse was applied. isorted fixed so that -1 returned only if strictly decreasing order
        
        if (testback) ans[i, rev.back := sum(system.time(y<<-fastorder(DT, 1:2))[ttype])]   # rev = reverse order
        ans[i, rev.forw := sum(system.time(x<<-forderv(DT))[ttype])]
        if (testback) ans[i, faster4 := rev.forw<rev.back+tol]
        if (testback) if (!ident(x,y)) browser()
        
        if (i==nrow(ans) || ans[i+1,Levels]!=ans[i,Levels]) print(ans[Levels==Levels[i]])  # print each block as we go along
    }
    cat("\nFinished.\n\n")
    ans
}

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
    base::sort.list(x, na.last=FALSE, decreasing=FALSE,method="radix")
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
    base::sort.list(x, na.last=FALSE, decreasing=FALSE)
}

ordernumtol = function(x, tol=.Machine$double.eps^0.5) {
    o = forderv(x)
    if (length(o)) o else seq_along(x)
    # was as follows, but we removed Crorder_tol at C level. 
    #   o=seq_along(x)
    #   .Call(Crorder_tol,x,o,tol)
    #   o
    # Retaining this function (ordernumtol) so that fastorder and bench() still works. So we can
    # still test forwards vs backwards through columns, but just using the new forderv to sort the
    # entire numeric column when backwards with fastorder.
}

# chorder2 to be used only with fastorder
# neither are exported
chorder2 = function(x, o=NULL) {
    if (!is.null(o)) {
        x = copy(x)
        setreordervec(x, o)
    }
    forderv(x,sort=TRUE)   #  was .Call(Ccountingcharacter, x, TRUE) but that's now removed
}

fastorder <- function(x, by=seq_along(x), verbose=getOption("datatable.verbose"))
{
    # x can be a vector, or anything that's stored as a list (inc data.frame and data.table), thus can be accessed with non-copying base::[[.
    # When x is a list, 'by' may be integers or names
    # This function uses the backwards approach; i.e. first orders the last column, then orders the 2nd to last column ordered by the order of
    # the last column, and so on. This vectorized approach is much faster than base::order(...) [src/main/sort.c:ordervector(...,listgreater)]
    # which is a comparison sort comparing 2 rows using a loop through columns for that row with a switch on each column type.
    
    # Now here only for dev testing to compare to forderv, e.g. in bench()
    # Always orders without testing for sortedness. This is favourable to fastorder (!), unless, data is perfectly ordered. See message in bench().

    if (is.atomic(x)) { by=NULL; v = x; w = 1 }  # w = 1 just for the error message below
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



