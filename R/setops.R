# For internal use only (input symbol requirement is not checked)
#   cols [symbol] - columns provided to function argument
#   dt   [symbol] - a data.table
# Iff all of 'cols' is present in 'x' return col indices
# is.data.table(dt) check should be performed in the calling function
validate <- function(cols, dt) {
    argcols = deparse(substitute(cols))
    argdt = deparse(substitute(dt))
    origcols = cols
    if (is.character(cols)) cols = chmatch(cols, names(dt))
    cols = as.integer(cols)
    isna = which(!cols %in% seq_along(dt))
    if (length(isna))
        stop(argcols, " value(s) [", paste(origcols[isna], collapse=", "), "] not present (or out of range) in ", argdt)
    cols
}

# setdiff for data.tables, internal at the moment #547, used in not-join
setdiff_ <- function(x, y, by.x=seq_along(x), by.y=seq_along(y), use.names=FALSE) {
    if (!is.data.table(x) || !is.data.table(y)) stop("x and y must both be data.tables")
    if (is.null(x) || !length(x)) return(x)
    by.x = validate(by.x, x)
    if (is.null(y) || !length(y)) return(unique(x, by=by.x))
    by.y = validate(by.y, y)    
    if (length(by.x) != length(by.y)) stop("length(by.x) != length(by.y)") 
    # factor in x should've factor/character in y, and viceversa
    for (a in seq_along(by.x)) {
        lc = by.y[a]
        rc = by.x[a]
        icnam = names(y)[lc]
        xcnam = names(x)[rc]
        if ( is.character(x[[rc]]) && !(is.character(y[[lc]]) || is.factor(y[[lc]])) ) {
            stop("When x's column ('",xcnam,"') is character, the corresponding column in y ('",icnam,"') should be factor or character, but found incompatible type '",typeof(y[[lc]]),"'.")
        } else if ( is.factor(x[[rc]]) && !(is.character(y[[lc]]) || is.factor(y[[lc]])) ) {
            stop("When x's column ('",xcnam,"') is factor, the corresponding column in y ('",icnam,"') should be character or factor, but found incompatible type '",typeof(y[[lc]]),"'.")
        } else if ( (is.integer(x[[rc]]) || is.double(x[[rc]])) && (is.logical(y[[lc]]) || is.character(y[[lc]])) ) {
            stop("When x's column ('",xcnam,"') is integer or numeric, the corresponding column in y ('",icnam,"') can not be character or logical types, but found incompatible type '",typeof(y[[lc]]),"'.")
        }
    }
    ux = unique(shallow(x, by.x))
    uy = unique(shallow(y, by.y))
    ix = duplicated(rbind(uy, ux, use.names=use.names, fill=FALSE))[-seq_len(nrow(uy))]
    .Call("CsubsetDT", ux, which_(ix, FALSE), seq_along(ux)) # more memory efficient version of which(!ix)
}

# set operators ---

funique <- function(x) {
    stopifnot(is.data.table(x))
    dup = duplicated(x)
    if(any(dup)) .Call("CsubsetDT", x, which_(dup, FALSE), seq_along(x)) else x
}

fintersect <- function(x, y, all=FALSE) {
    if (!is.logical(all) || length(all) != 1L) stop("argument 'all' should be logical of length one")
    if (!is.data.table(x) || !is.data.table(y)) stop("x and y must be both data.tables")
    if (!identical(sort(names(x)), sort(names(y)))) stop("x and y must have same column names")
    if (!identical(names(x), names(y))) stop("x and y must have same column order")
    bad.type = setNames(c("raw","complex","list") %chin% c(vapply(x, typeof, FUN.VALUE = ""), vapply(y, typeof, FUN.VALUE = "")), c("raw","complex","list"))
    if (any(bad.type)) stop(sprintf("x and y must not have unsupported column types: %s", paste(names(bad.type)[bad.type], collapse=", ")))
    if (!identical(lapply(x, class), lapply(y, class))) stop("x and y must have same column classes")
    if (".seqn" %in% names(x)) stop("None of the datasets to intersect should contain a column named '.seqn'")
    if (!nrow(x) || !nrow(y)) return(x[0L])
    if (all) {
        x = shallow(x)[, ".seqn" := rowidv(x)]
        y = shallow(y)[, ".seqn" := rowidv(y)]
        x[y, .SD, .SDcols=setdiff(names(x),".seqn"), nomatch=0L, on=names(x)]
    } else {
        x[funique(y), nomatch=0L, on=names(x), mult="first"]
    }
}

fsetdiff <- function(x, y, all=FALSE) {
    if (!is.logical(all) || length(all) != 1L) stop("argument 'all' should be logical of length one")
    if (!is.data.table(x) || !is.data.table(y)) stop("x and y must be both data.tables")
    if (!identical(sort(names(x)), sort(names(y)))) stop("x and y must have same column names")
    if (!identical(names(x), names(y))) stop("x and y must have same column order")
    bad.type = setNames(c("raw","complex","list") %chin% c(vapply(x, typeof, FUN.VALUE = ""), vapply(y, typeof, FUN.VALUE = "")), c("raw","complex","list"))
    if (any(bad.type)) stop(sprintf("x and y must not have unsupported column types: %s", paste(names(bad.type)[bad.type], collapse=", ")))
    if (!identical(lapply(x, class), lapply(y, class))) stop("x and y must have same column classes")
    if (".seqn" %in% names(x)) stop("None of the datasets to setdiff should contain a column named '.seqn'")
    if (!nrow(x)) return(x)
    if (!nrow(y)) return(if(!all) funique(x) else x)
    if (all) {
        x = shallow(x)[, ".seqn" := rowidv(x)]
        y = shallow(y)[, ".seqn" := rowidv(y)]
        x[!y, .SD, .SDcols=setdiff(names(x),".seqn"), on=names(x)]
    } else {
        funique(x[!y, on=names(x)])
    }
}

funion <- function(x, y, all=FALSE) {
    if (!is.logical(all) || length(all) != 1L) stop("argument 'all' should be logical of length one")
    if (!is.data.table(x) || !is.data.table(y)) stop("x and y must be both data.tables")
    if (!identical(sort(names(x)), sort(names(y)))) stop("x and y must have same column names")
    if (!identical(names(x), names(y))) stop("x and y must have same column order")
    bad.type = setNames(c("raw","complex", if(!all) "list") %chin% c(vapply(x, typeof, FUN.VALUE = ""), vapply(y, typeof, FUN.VALUE = "")), c("raw","complex", if(!all) "list"))
    if (any(bad.type)) stop(sprintf("x and y must not have unsupported column types: %s", paste(names(bad.type)[bad.type], collapse=", ")))
    if (!identical(lapply(x, class), lapply(y, class))) stop("x and y must have same column classes")
    ans = rbindlist(list(x, y))
    if (!all) ans = funique(ans)
    ans
}

fsetequal <- function(x, y) {
    if (!is.data.table(x) || !is.data.table(y)) stop("x and y must be both data.tables")
    if (!identical(sort(names(x)), sort(names(y)))) stop("x and y must have same column names")
    if (!identical(names(x), names(y))) stop("x and y must have same column order")
    bad.type = setNames(c("raw","complex","list") %chin% c(vapply(x, typeof, FUN.VALUE = ""), vapply(y, typeof, FUN.VALUE = "")), c("raw","complex","list"))
    if (any(bad.type)) stop(sprintf("x and y must not have unsupported column types: %s", paste(names(bad.type)[bad.type], collapse=", ")))
    if (!identical(lapply(x, class), lapply(y, class))) stop("x and y must have same column classes")
    isTRUE(all.equal.data.table(x, y, check.attributes = FALSE, ignore.row.order = TRUE))
}

# all.equal ----

all.equal.data.table <- function(target, current, trim.levels=TRUE, check.attributes=TRUE, ignore.col.order=FALSE, ignore.row.order=FALSE, ...) {
    stopifnot(is.logical(trim.levels), is.logical(check.attributes), is.logical(ignore.col.order), is.logical(ignore.row.order))
    if (!is.data.table(target) || !is.data.table(current)) stop("'target' and 'current' must be both data.tables")
    
    msg = character(0)
    # init checks that detect high level all.equal
    if(nrow(current) != nrow(target)) msg = "Different number of rows"
    if(ncol(current) != ncol(target)) msg = c(msg, "Different number of columns")
    diff.colnames = !identical(sort(names(target)), sort(names(current)))
    diff.colorder = !identical(names(target), names(current))
    if(diff.colnames) msg = c(msg, "Different column names")
    if(!diff.colnames && !ignore.col.order && diff.colorder) msg = c(msg, "Different column order")
    
    if(length(msg)) return(msg) # skip check.attributes and further heavy processing
    
    # ignore.col.order
    if (ignore.col.order && diff.colorder) current = setcolorder(shallow(current), names(target))
    
    # check column classes match
    if (!identical(lapply(target, class), lapply(current, class))) return("Datasets has different column classes")
    
    # check attributes
    if (check.attributes) {
        # check key
        k1 = key(target)
        k2 = key(current)
        if (!identical(k1, k2)) {
            return(sprintf("Datasets has different keys. 'target'%s. 'current'%s.",
                           if(length(k1)) paste0(": ", paste(k1, collapse=", ")) else " has no key",
                           if(length(k2)) paste0(": ", paste(k2, collapse=", ")) else " has no key"))
        }
        # check index
        i1 = key2(target)
        i2 = key2(current)
        if (!identical(i1, i2)) {
            return(sprintf("Datasets has different indexes. 'target'%s. 'current'%s.",
                           if(length(i1)) paste0(": ", paste(i1, collapse=", ")) else " has no index",
                           if(length(i2)) paste0(": ", paste(i2, collapse=", ")) else " has no index"))
        }
        
        # Trim any extra row.names attributes that came from some inheritence
        # Trim ".internal.selfref" as long as there is no `all.equal.externalptr` method
        exclude.attrs = function(x, attrs = c("row.names",".internal.selfref")) x[!names(x) %in% attrs]
        a1 = exclude.attrs(attributes(target))
        a2 = exclude.attrs(attributes(current))
        if (length(a1) != length(a2)) return(sprintf("Datasets has different number of (non-excluded) attributes: target %s, current %s", length(a1), length(a2)))
        if (!identical(nm1 <- sort(names(a1)), nm2 <- sort(names(a2)))) return(sprintf("Datasets has attributes with different names: %s", paste(setdiff(union(names(a1), names(a2)), intersect(names(a1), names(a2))), collapse=", ")))
        attrs.r = all.equal(a1[nm1], a2[nm2], ..., check.attributes = check.attributes)
        if (is.character(attrs.r)) return(paste("Attributes: <", attrs.r, ">")) # skip further heavy processing
    }
    
    # ignore.row.order
    if (ignore.row.order) {
        if (".seqn" %in% names(target)) stop("None of the datasets to compare should contain a column named '.seqn'")
        bad.type = setNames(c("raw","complex","list") %chin% c(vapply(current, typeof, FUN.VALUE = ""), vapply(target, typeof, FUN.VALUE = "")), c("raw","complex","list"))
        if (any(bad.type)) stop(sprintf("Datasets to compare with 'ignore.row.order' must not have unsupported column types: %s", paste(names(bad.type)[bad.type], collapse=", ")))
        target_dup = as.logical(anyDuplicated(target))
        current_dup = as.logical(anyDuplicated(current))
        if (target_dup && !current_dup)
            return("Dataset 'target' has duplicate rows while 'current' don't have any duplicate rows")
        if (!target_dup && current_dup)
            return("Dataset 'current' has duplicate rows while 'target' don't have any duplicate rows")
        if (target_dup && current_dup) {
            target = shallow(target)[, ".seqn" := rowidv(target)]
            current = shallow(current)[, ".seqn" := rowidv(current)]
            jn.on = c(".seqn",setdiff(names(target),".seqn"))
        } else {
            jn.on = names(target)
        }
        ans = target[current, nomatch=NA, which=TRUE, on=jn.on]
        if (anyNA(ans)) return("Dataset 'current' has duplicated rows present in different quantity than in 'target'")
        ans = current[target, nomatch=NA, which=TRUE, on=jn.on]
        if (anyNA(ans)) return("Dataset 'target' has duplicated rows present in different quantity than in 'current'")
    } else {
        for (i in seq_along(target)) {
            # trim.levels moved here
            if.trim = function(x) if (check.attributes && trim.levels && is.factor(x)) factor(x) else x
            cols.r = all.equal(if.trim(target[[i]]), if.trim(current[[i]]), ..., check.attributes = check.attributes)
            if (is.character(cols.r)) return(paste0("Column '", names(target)[i], "': ", cols.r))
        }
    }
    TRUE
}
