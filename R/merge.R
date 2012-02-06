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
    
    dt = y[xkey,nomatch=ifelse(all.x,NA,0)]   # includes JIS columns (with a .1 suffix if conflict with x names)
    
    end = setdiff(names(y),by)     # X[Y] sytax puts JIS i columns at the end, merge likes them alongside i.
    setcolorder(dt,c(setdiff(names(dt),end),end))
    
    if (all.y) {
        # Perhaps not very commonly used, so not a huge deal that the join is redone here.
        missingyidx <- (1:nrow(y))[-y[xkey,which=TRUE,nomatch=0]]
        if (length(missingyidx)) {
            yy <- y[missingyidx]
            othercolsx <- setdiff(names(x), by)
            if (length(othercolsx) > 0) {
                tmp = rep(NA_integer_, length(missingyidx))
                yy <- cbind(yy, x[tmp, othercolsx, with = FALSE])
            }
            setcolorder(yy, names(dt))
            dt = rbind(dt, yy)
        }
    }

    if (nrow(dt) > 0) setkeyv(dt,by)

    ## We expect that the result of dt.1[dt.2] adds a ".1" suffix to
    ## duplicate columns for dt.1 that are added by result of the implicit
    ## merge. The (duplicate) names in dt.1 are not changed.
    ##
    ## If the suffixes are c(".1", "") our work is already done as that's the
    ## behaviour of X[Y].
    ##
    ## TO DO: revisit this logic and simplify, too.
    
    if (!identical(suffixes,c(".1", ""))) {
        col.names <- colnames(dt)
        rename.y <- grep("\\.1$", col.names)

        if (length(rename.y) > 0L) {
            ## It's possible that some columns in `rename.y` are from `x`, if
            ## the colnames from `x` end in *.1$ as well, so let's remove those
            ## if they're there.
            in.x <- which(col.names[rename.y] %in% colnames(x))
            if (length(in.x) > 0) {
                rename.y <- rename.y[-in.x]
            }
        }
        if (length(rename.y) > 0L) {
            base.names <- gsub("\\.1$", "", col.names[rename.y])
            
            ## suffix y columns
            col.names[rename.y] <- paste(base.names, suffixes[1], sep="")
            
            ## suffix x columns
            xref <- match(base.names, col.names)
            if (any(is.na(xref))) {
                ## expected to suffix columns from `x`, something went wrong
                warning("There was a problem suffixing the merged ",
                        "data.table. The merge was successful, but the ",
                        "default column naming has been used [ie. suffixes ",
                        "were set to c('', '.1')].\nPlease consider ",
                        "contacting `maintainer('data.table')` so we ",
                        "can fix this.")
            } else {
                col.names[xref] <- paste(base.names, suffixes[2], sep="")
                setnames(dt, col.names)
            }
        }
    }
    dt
}


