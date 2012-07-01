merge.data.table <- function(x, y, by = NULL, all = FALSE, all.x = all,
                             all.y = all, suffixes = c(".x", ".y"), ...) {
    if (!inherits(y, 'data.table')) {
        y <- as.data.table(y)
        if (missing(by)) {
            by <- key(x)
        }
    }

    ## Try to infer proper value for `by`
    if (is.null(by)) {
        by <- intersect(key(x), key(y))
    }
    if (is.null(by)) {
        by <- key(x)
    }
    if (is.null(by)) {
        stop("Can not match keys in x and y to automatically determine ",
             "appropriate `by` parameter. Please set `by` value explicitly.")
    }
    if (length(by) == 0L || !is.character(by)) {
        stop("A non-empty vector of column names for `by` is required.")
    }
    if (!all(by %in% intersect(colnames(x), colnames(y)))) {
        stop("Elements listed in `by` must be valid column names in x and y")
    }

    ## Checks to see that keys on dt are set and are in correct order
    .reset.keys <- function(dt, by) {
        dt.key <- key(dt)
        length(dt.key) < length(by) || !all(dt.key[1:length(by)] == by)
    }

    if (.reset.keys(y, by)) {
        y=setkeyv(copy(y),by)
        # TO DO Add a secondary key here, when implemented which would be cached in the object
    }

    xkey = if (identical(key(x),by)) x else setkeyv(copy(x),by)
    # TO DO: new [.data.table argument joincols or better name would allow leaving x as is if by was a head subset
    # of key(x). Also NAMED on each column would allow subset references. Also, a secondary key may be
    # much simpler but just need an argument to tell [.data.table to use the 2key of i.


    ## sidestep the auto-increment column number feature-leading-to-bug by
    ## ensuring no names end in ".1", see unit test
    ## "merge and auto-increment columns in y[x]" in test-data.frame.like.R
    dupnames <- setdiff(intersect(names(xkey), names(y)), by)
    if (length(dupnames)) {
        xkey = setnames(shallow(xkey), dupnames, sprintf("%s.", dupnames))
        y = setnames(shallow(y), dupnames, sprintf("%s.", dupnames))
    }

    dt = y[xkey,nomatch=ifelse(all.x,NA,0)]   # includes JIS columns (with a .1 suffix if conflict with x names)

    end = setdiff(names(y),by)     # X[Y] sytax puts JIS i columns at the end, merge likes them alongside i.
    setcolorder(dt,c(setdiff(names(dt),end),end))
    
    if (all.y) {
        # Perhaps not very commonly used, so not a huge deal that the join is redone here.
        missingyidx = seq.int(nrow(y))
        whichy = y[xkey,which=TRUE,nomatch=0]  # TO DO:  Use not join (i=-xkey) here when implemented
        whichy = whichy[whichy>0]
        if (length(whichy)) missingyidx = missingyidx[-whichy]
        if (length(missingyidx)) {
            yy <- y[missingyidx]
            othercolsx <- setdiff(names(xkey), by)
            if (length(othercolsx)) {
                tmp = rep(NA_integer_, length(missingyidx))
                yy <- cbind(yy, xkey[tmp, othercolsx, with = FALSE])
                setnames(yy, make.names(names(yy),unique=TRUE))
            }
            setcolorder(yy, names(dt))
            dt = rbind(dt, yy)
        }
    }

    if (nrow(dt) > 0) setkeyv(dt,by)

    if (length(dupnames)) {
        setnames(dt, sprintf("%s.", dupnames), paste(dupnames, suffixes[2], sep=""))
        setnames(dt, sprintf("%s..1", dupnames), paste(dupnames, suffixes[1], sep=""))
    }
    
    dt
}


