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

    if (.reset.keys(x, by)) {
        key(x) <- by
    }
    if (.reset.keys(y, by)) {
        key(y) <- by
    }
    key <- by

    ykey <- y[,key,with=FALSE]
    xidx <- x[ykey, nomatch = 0, mult = 'all', which = TRUE]
    xkey <- x[,key,with=FALSE]
    yidx <- y[xkey, nomatch = 0, mult = 'all', which = TRUE]

    if (any(xidx)) {
        tmp = sort(xidx)
        xx <- x[tmp]
        yy <- y[yidx, setdiff(names(y), key), with = FALSE]
        dt <- xx
        if (ncol(yy) > 0) {
            dt <- cbind(xx, yy)
        }
    } else {
        # nothing in common between the two data tables, return NULL data.table
        dt <- data.table()
    }

    if (all.x) {
        missingxidx <- setdiff(1:nrow(x), xidx)
        if (length(missingxidx) > 0) {
            othercols <- setdiff(names(y), key)
            if (length(othercols) > 0) {
                tmp = rep(1, length(missingxidx))
                xx <- cbind(x[missingxidx], y[NA, othercols, with = FALSE][tmp])
            } else
                xx <- x[missingxidx]
            dt <- rbind(xx, dt[,names(xx),with=FALSE])
        }
    }
    if (all.y) {
        missingyidx <- setdiff(1:nrow(y), yidx)
        if (length(missingyidx) > 0) {
            yy <- y[missingyidx, key, with = FALSE]
            othercolsx <- setdiff(names(x), key)
            if (length(othercolsx) > 0) {
                tmp = rep(1, length(missingyidx))
                yy <- cbind(yy, x[NA, othercolsx, with = FALSE][tmp])
            }
            othercolsy <- setdiff(names(y), key)
            if (length(othercolsy) > 0)
                yy <- cbind(yy, y[missingyidx, othercolsy, with = FALSE])
            dt <- rbind(dt, yy[,names(dt),with=FALSE])
        }
    }
    if (nrow(dt) > 0) key(dt) = key

    ###########################################################################
    ## Handle merging suffix behavior.
    ## Enable ability to use "old" suffix behavior for now.
    if (missing(suffixes)) {
        pre.suffix <- options('datatable.pre.suffixes')[[1L]]
        pre.suffix <- !is.null(pre.suffix) && as.logical(pre.suffix)[1L]
        suffixes <- if (pre.suffix) c("", ".1") else c(".x", ".y")
    }

    ## We expect that the result of dt.1[dt.2] adds a ".1" suffix to
    ## duplicate columns for dt.2 that are added by result of the implicit
    ## merge. The (duplicate) names in dt.1 are not changed.
    ##
    ## If the suffixes are c("", ".1") our work is already done.
    if (!all(suffixes == c("", ".1"))) {
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
            col.names[rename.y] <- paste(base.names, suffixes[2], sep="")
            
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
                col.names[xref] <- paste(base.names, suffixes[1], sep="")
                setattr(dt, "names", col.names)
            }
        }
    }

    dt
}

