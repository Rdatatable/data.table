 
merge.data.table <- function(x, y, all = FALSE, all.x = all, all.y = all,
                             suffixes = c("", ".1"), ...) {
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
    
    ## Act like merge.data.frame does with respect to column names/suffixes.
    ## We expect that the result of dt.1[dt.2] adds a ".1" suffix to
    ## duplicate columns for dt.2 that are added by result of the implicit
    ## merge.
    ## 
    ## If the suffixes are c("", ".1") we don't have anything to do
    if (!all(suffixes == c("", ".1"))) {
      col.names <- colnames(dt)
      rename.y <- grep("\\.1$", col.names)
      if (length(rename.y) > 0L) {
          do.rename <- TRUE
          renames <- col.names[rename.y]
          base.names <- gsub("\\.1$", "", renames)

          ## y
          y.names <- paste(base.names, suffixes[2], sep="")
          col.names[rename.y] <- y.names

          ## x
          xref <- match(base.names, col.names)
          if (any(is.na(xref))) {
            ## Houston we have a problem
            do.rename <- FALSE
          } else {
            col.names[xref] <- paste(base.names, suffixes[1], sep="")
          }

          if (do.rename) {
            colnames(dt) <- col.names
          } else {
            warning("There was a problem re-suffixing the merged data.table. ",
                    "The merge was succesful, but the default column naming ",
                    "been kept (suffixes ignored)")
          }
      }
    }
    
    dt
}

