merge.data.table <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all,
                             all.y = all, sort = TRUE, suffixes = c(".x", ".y"), allow.cartesian=getOption("datatable.allow.cartesian"), ...) {
    if (!sort %in% c(TRUE, FALSE))
      stop("Argument 'sort' should be logical TRUE/FALSE")
    if (!is.data.table(y)) {
        y = as.data.table(y)
        if (missing(by) && missing(by.x)) {
            by = key(x)
        }
    }
    if (any(duplicated(names(x)))) stop("x has some duplicated column name(s): ",paste(names(x)[duplicated(names(x))],collapse=","),". Please remove or rename the duplicate(s) and try again.")
    if (any(duplicated(names(y)))) stop("y has some duplicated column name(s): ",paste(names(y)[duplicated(names(y))],collapse=","),". Please remove or rename the duplicate(s) and try again.")
    
    ## set up 'by'/'by.x'/'by.y'
    if ( (!is.null(by.x) || !is.null(by.y)) && length(by.x)!=length(by.y) )
        stop("`by.x` and `by.y` must be of same length.")
    if (!missing(by) && !missing(by.x))
        warning("Supplied both `by` and `by.x/by.y`. `by` argument will be ignored.")
    if (!is.null(by.x)) {
        if ( !is.character(by.x) || !is.character(by.y))
            stop("A non-empty vector of column names are required for `by.x` and `by.y`.")
        if (!all(by.x %in% names(x)))
            stop("Elements listed in `by.x` must be valid column names in x.")
        if (!all(by.y %in% names(y)))
            stop("Elements listed in `by.y` must be valid column names in y.")
        by = by.x
        names(by) = by.y
    } else {
        if (is.null(by))
            by = intersect(key(x), key(y))
        if (is.null(by)) 
            by = key(x)
        if (is.null(by))
            stop("Can not match keys in x and y to automatically determine appropriate `by` parameter. Please set `by` value explicitly.")
        if (length(by) == 0L || !is.character(by))
            stop("A non-empty vector of column names for `by` is required.")
        if (!all(by %in% intersect(colnames(x), colnames(y))))
            stop("Elements listed in `by` must be valid column names in x and y")
        by.x = by.y = by
    }
    # with i. prefix in v1.9.3, this goes away. Left here for now ...
    ## sidestep the auto-increment column number feature-leading-to-bug by
    ## ensuring no names end in ".1", see unit test
    ## "merge and auto-increment columns in y[x]" in test-data.frame.like.R
    start = setdiff(names(x), by.x)
    end = setdiff(names(y), by.y)
    dupnames = intersect(start, end)
    if (length(dupnames)) {
        start[chmatch(dupnames, start, 0L)] = paste(dupnames, suffixes[1L], sep="")
        end[chmatch(dupnames, end, 0L)] = paste(dupnames, suffixes[2L], sep="")
    }

    dt = y[x,nomatch=ifelse(all.x,NA,0),on=by,allow.cartesian=allow.cartesian]   # includes JIS columns (with a i. prefix if conflict with x names)

    if (all.y && nrow(y)) {  # If y does not have any rows, no need to proceed
        # Perhaps not very commonly used, so not a huge deal that the join is redone here.
        missingyidx = y[!x,which=TRUE,on=by,allow.cartesian=allow.cartesian]
        if (length(missingyidx)) {
            yy = y[missingyidx]
            othercolsx = setdiff(names(x), by)
            if (length(othercolsx)) {
                tmp = rep.int(NA_integer_, length(missingyidx))
                # TO DO: use set() here instead..
                yy = cbind(yy, x[tmp, othercolsx, with = FALSE])
            }
            # empty data.tables (nrow =0, ncol>0) doesn't skip names anymore in new rbindlist
            # takes care of #5672 without having to save names. This is how it should be, IMHO.
            dt = rbind(dt, yy, use.names=FALSE)
        }
    }
    # X[Y] sytax puts JIS i columns at the end, merge likes them alongside i.
    newend = setdiff(names(y), by.y)
    # fix for #1290, make sure by.y order is set properly before naming
    setcolorder(dt, c(by.y, setdiff(names(dt), c(by.y, newend)), newend))
    setnames(dt, c(by.x, start, end))
    if (nrow(dt) > 0L) {
      setkeyv(dt, if (sort) by.x else NULL)
    }
    dt
}
