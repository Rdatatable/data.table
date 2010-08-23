
merge.data.table <- function(x, y, all = FALSE, all.x = all, all.y = all, ...) {
    # Relatively quick merge by common keys
    # Keys must be in the same order

    key1 <- attr(x, "sorted")
    key2 <- attr(y, "sorted")
    keylen <- min(length(key1), length(key2))
    if (!all(key1[1:keylen] == key2[1:keylen])) stop("data table keys don't match")
    key <- key1[1:keylen]
    xidx <- x[J(y[,key,with=FALSE]), nomatch = 0, mult = 'all', which = TRUE]
    yidx <- y[J(x[,key,with=FALSE]), nomatch = 0, mult = 'all', which = TRUE]

    if (any(xidx))
        dt <- cbind(x[sort(xidx)],
                    y[yidx, setdiff(names(y), key), with = FALSE])
    else # nothing in common between the two data tables
        dt <- data.table() # NULL data table

    if (all.x) {
        missingxidx <- setdiff(1:nrow(x), xidx)
        if (length(missingxidx) > 0) {
            othercols <- setdiff(names(y), key)
            if (length(othercols) < 1) othercols = data.table()
            dt <- rbind(cbind(x[missingxidx], y[NA, othercols, with = FALSE][rep(1, length(missingxidx))]),
                        dt)
        }
    }
    if (all.y) {
        missingyidx <- setdiff(1:nrow(y), yidx)
        if (length(missingyidx) > 0) {
            othercolsx <- setdiff(names(x), key)
            if (length(othercolsx) < 1) othercolsx <- data.table()
            othercolsy <- setdiff(names(y), key)
            if (length(othercolsy) < 1) othercolsy <- data.table()
            dt <- rbind(dt,
                        cbind(y[missingyidx, key, with = FALSE],
                              x[NA, othercolsx, with = FALSE][rep(1, length(missingyidx))],
                              y[missingyidx, othercolsy, with = FALSE]))
        }
    }
    if (nrow(dt) > 0 && (all.x || all.y))
        eval(parse(text=paste("setkey(dt,", paste(key, collapse=","), ")",sep="")))
    dt
}
