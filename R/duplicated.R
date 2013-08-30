
duplicated.data.table <- function(x, incomparables=FALSE,
                                  tolerance=.Machine$double.eps ^ 0.5,
                                  by=key(x), ...) {
    if (!cedta()) return(NextMethod("duplicated"))
    if (!identical(incomparables, FALSE)) {
        .NotYetUsed("incomparables != FALSE")
    }

    query <- .duplicated.helper(x, by)
    res <- rep.int(TRUE, nrow(x))

    if (query$use.keyprefix) {
        res[duplist(x[, query$by, with=FALSE], tolerance=tolerance)] = FALSE
    } else {
        xx <- x[, query$by, with=FALSE]
        o = fastorder(xx)
        f = o[duplist(xx, o, tolerance=tolerance)]
        f = f[sort.list(f, na.last=FALSE, decreasing=FALSE)]
        # TO DO: remove sort.list call by replacing fastorder with fastgroup
        res[f] = FALSE
    }

    res
}

unique.data.table <- function(x, incomparables=FALSE,
                              tolerance=.Machine$double.eps ^ 0.5,
                              by=key(x), ...) {
    if (!cedta()) return(NextMethod("unique"))
    dups <- duplicated.data.table(x, incomparables, tolerance, by, ...)
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
            if (as.integer(by) != by) {
                stop("Integer values required for by if specifying ",
                     "column indices")
            }
            by <- names(x)[by]
        }
        if (!is.character(by)) {
            stop("Only column indices or names are allowed in by")
        }
        bad.cols <- setdiff(by, names(x))
        if (length(bad.cols)) {
            stop("by specifies column names that do no exist in `x`")
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
