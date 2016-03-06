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
