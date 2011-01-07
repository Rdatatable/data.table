 
merge.data.table <- function(x, y, all = FALSE, all.x = all, all.y = all, ...) {
    # Relatively quick merge by common keys
    # Keys must be in the same order

    key1 <- attr(x, "sorted")
    key2 <- attr(y, "sorted")
    keylen <- min(length(key1), length(key2))
    if (keylen == 0 || !all(key1[1:keylen] == key2[1:keylen])) stop("data table keys don't match")
    key <- key1[1:keylen]
    xidx <- x[J(y[,key,with=FALSE]), nomatch = 0, mult = 'all', which = TRUE]
    yidx <- y[J(x[,key,with=FALSE]), nomatch = 0, mult = 'all', which = TRUE]

    if (any(xidx)) {
        xx <- x[sort(xidx)]
        yy <- y[yidx, setdiff(names(y), key), with = FALSE]
        dt <- xx
        if (ncol(yy) > 0)  dt <- cbind(xx, yy)
    } else # nothing in common between the two data tables
        dt <- data.table() # NULL data table

    if (all.x) {
        missingxidx <- setdiff(1:nrow(x), xidx)
        if (length(missingxidx) > 0) {
            othercols <- setdiff(names(y), key)
            if (length(othercols) > 0)
                xx <- cbind(x[missingxidx], y[NA, othercols, with = FALSE][rep(1, length(missingxidx))])
            else 
                xx <- x[missingxidx]
            dt <- rbind(xx, dt)
        }
    }
    if (all.y) {
        missingyidx <- setdiff(1:nrow(y), yidx)
        if (length(missingyidx) > 0) {
            yy <- y[missingyidx, key, with = FALSE]
            othercolsx <- setdiff(names(x), key)
            if (length(othercolsx) > 0)
                yy <- cbind(yy, x[NA, othercolsx, with = FALSE][rep(1, length(missingyidx))])
            othercolsy <- setdiff(names(y), key)
            if (length(othercolsy) > 0) 
                yy <- cbind(yy, y[missingyidx, othercolsy, with = FALSE])
            dt <- rbind(dt, yy)
        }
    }
    if (nrow(dt) > 0)
        eval(parse(text=paste("setkey(dt,", paste(key, collapse=","), ")",sep="")))
    dt
}
