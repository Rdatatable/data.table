
duplicated.data.table <- function(x, incomparables=FALSE, by=key(x), ...) {
    if (!cedta()) return(NextMethod("duplicated"))
    if (!identical(incomparables, FALSE)) {
        .NotYetUsed("incomparables != FALSE")
    }

    query <- .duplicated.helper(x, by)
    # fix for bug #5405 - unique on null data table returns error (because of 'forder')
    # however, in this case we can bypass having to go to forder at all.
    # TODO: Still 'forder' will need a fix to not return an error in case of null data.tables (that's for later)
    if (!length(query$by)) return(logical(0))
    res <- rep.int(TRUE, nrow(x))
    
    if (query$use.keyprefix) {
        f = uniqlist(x[, query$by, with=FALSE])
    } else {
        o = forder(x, by=query$by, sort=FALSE, retGrp=TRUE)
        f = attr(o,"starts")
        if (length(o)) f=o[f]
    }
    res[f] = FALSE
    res
}

unique.data.table <- function(x, incomparables=FALSE, by=key(x), ...) {
    if (!cedta()) return(NextMethod("unique"))
    dups <- duplicated.data.table(x, incomparables, by, ...)
    x[!dups]
}

## Specify the column names to be used in the uniqueness query, and if this
## query can take advantage of the keys of `x` (if present).
## returns a list
##
## This was dropped into a helper because initial implementation of
## unique.data.table and duplicated.data.table both needed this. However,
## unique.data.table has bene refactored to simply call duplicated.data.table
## making the refactor unnecessary, but let's leave it here just in case
.duplicated.helper <- function(x, by) {
    use.sub.cols <- !is.null(by) && !isTRUE(by)

    if (use.sub.cols) {
        ## Did the user specify (integer) indexes for the columns?
        if (is.numeric(by)) {
            if (any(as.integer(by) != by) || any(by<1) || any(by>ncol(x))) {
                stop("Integer values between 1 and ncol are required for 'by' when ",
                     "column indices. It's often better to use column names.")
            }
            by <- names(x)[by]
        }
        if (!is.character(by)) {
            stop("Only column indices or names are allowed in by")
        }
        bad.cols <- setdiff(by, names(x))
        if (length(bad.cols)) {
            stop("by specifies column names that do not exist. First 5: ",paste(head(bad.cols,5),collapse=","))
        }

        use.keyprefix = haskey(x) &&
            length(by) <= length(key(x)) &&
            all(head(key(x), length(by)) == by)
    } else {
        ## by is not was explicitly set to
        use.keyprefix = FALSE
        by = names(x)
    }

    list(use.keyprefix=use.keyprefix, by=by)
}


