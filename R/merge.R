 
merge.data.table <- function(x, y, all = FALSE, all.x = all, all.y = all, ...) {
    # Relatively quick merge by common keys
    # Keys must be in the same order

    keylen <- min(length(key(x)), length(key(y)))
    if (keylen == 0 || !all(key(x)[1:keylen] == key(y)[1:keylen])) stop("data table keys don't match")
    key <- key(x)[1:keylen]
    ykey <- y[,key,with=FALSE]
    xidx <- x[ykey, nomatch = 0, mult = 'all', which = TRUE]
    xkey <- x[,key,with=FALSE]
    yidx <- y[xkey, nomatch = 0, mult = 'all', which = TRUE]

    if (any(xidx)) {
        tmp = sort(xidx)
        xx <- x[tmp]
        yy <- y[yidx, setdiff(names(y), key), with = FALSE]
        dt <- xx
        if (ncol(yy) > 0)  dt <- cbind(xx, yy)
    } else # nothing in common between the two data tables
        dt <- data.table() # NULL data table

    if (all.x) {
        missingxidx <- setdiff(1:nrow(x), xidx)
        if (length(missingxidx) > 0) {
            othercols <- setdiff(names(y), key)
            if (length(othercols) > 0) {
                tmp = rep(1, length(missingxidx))
                xx <- cbind(x[missingxidx], y[NA, othercols, with = FALSE][tmp])
            } else 
                xx <- x[missingxidx]
            dt <- rbind(xx, dt)
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
            dt <- rbind(dt, yy)
        }
    }
    if (nrow(dt) > 0) key(dt) = key
    dt
}

