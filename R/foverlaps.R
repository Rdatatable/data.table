foverlaps <- function(x, y, by.x = if (!is.null(key(x))) key(x) else key(y), by.y = key(y), maxgap=0L, minoverlap=1L, type=c("any", "within", "start", "end", "equal"), mult=c("all", "first", "last"), nomatch=getOption("datatable.nomatch"), which = FALSE,  verbose=getOption("datatable.verbose")) {

    if (!is.data.table(y) || !is.data.table(x)) stop("y and x must both be data.tables. Use `setDT()` to convert list/data.frames to data.tables by reference or as.data.table() to convert to data.tables by copying.")
    maxgap = as.integer(maxgap); minoverlap = as.integer(minoverlap); 
    which = as.logical(which); nomatch = as.integer(nomatch); 
    if (!length(maxgap) || length(maxgap) != 1L || is.na(maxgap) || maxgap < 0L)
        stop("maxgap must be a non-negative integer value of length 1")    
    if (!length(minoverlap) || length(minoverlap) != 1L || is.na(minoverlap) || minoverlap < 1L)
        stop("minoverlap must be a positive integer value of length 1")
    if (!length(which) || length(which) != 1L || is.na(which))
        stop("which must be a logical vector of length 1. Either TRUE/FALSE")
    if (!length(nomatch) || length(nomatch) != 1L || (!is.na(nomatch) && nomatch!=0L))
        stop("nomatch must either be NA or 0, or (ideally) NA_integer_ or 0L")
    type = match.arg(type)
    mult = match.arg(mult)
    if (type == "equal")
        stop("type = 'equal' is not implemented yet. But note that this is just the same as a normal data.table join y[x, ...], unless you are also interested in setting 'minoverlap / maxgap' arguments. But those arguments are not implemented yet as well.")
    if (maxgap > 0L || minoverlap > 1L)
        stop("maxgap and minoverlap arguments are not yet implemented.")
    if (is.null(by.y))
        stop("'y' must be keyed (i.e., sorted, and, marked as sorted). Call setkey(y, ...) first, see ?setkey. Also check the examples in ?foverlaps.")
    if (length(by.x) < 2L || length(by.y) < 2L)
        stop("'by.x' and 'by.y' should contain at least two column names (or numbers) each - corresponding to 'start' and 'end' points of intervals. Please see ?foverlaps and examples for more info.")
    if (is.numeric(by.x)) {
        if (any(by.x < 0L) || any(by.x > length(x)))
            stop("Invalid numeric value for 'by.x'; it should be a vector with values 1 <= by.x <= length(x)")
        by.x = names(x)[by.x]
    }
    if (is.numeric(by.y)) {
        if (any(by.y < 0L) || any(by.y > length(y)))
            stop("Invalid numeric value for 'by.x'; it should be a vector with values 1 <= by.y <= length(y)")
        by.y = names(y)[by.y]
    }
    if (!length(by.x) || !is.character(by.x))
        stop("A non-empty vector of column names is required for by.x")
    if (!length(by.y) || !is.character(by.y))
        stop("A non-empty vector of column names is required for by.y")
    if (!identical(by.y, key(y)[seq_along(by.y)]))
        stop("The first ", length(by.y), " columns of y's key is not identical to the columns specified in by.y.")
    if (any(is.na(chmatch(by.x, names(x)))))
        stop("Elements listed in 'by.x' must be valid names in data.table 'x'")
    if (any(is.na(chmatch(by.y, names(y)))))
        stop("Elements listed in 'by.y' must be valid names in data.table 'y'")
    if (anyDuplicated(by.x) || anyDuplicated(by.y))
        stop("Duplicate columns are not allowed in overlap joins. This may change in the future.")
    if (length(by.x) != length(by.y))
        stop("length(by.x) != length(by.y). Columns specified in by.x should correspond to columns specified in by.y and should be of same lengths.")
    
    xnames = by.x; xintervals = tail(xnames, 2L);
    ynames = by.y; yintervals = tail(ynames, 2L);
    if (!storage.mode(x[[xintervals[1L]]]) %chin% c("double", "integer") || !storage.mode(x[[xintervals[2L]]]) %chin% c("double", "integer"))
        stop("The last two columns in by.x should correspond to the 'start' and 'end' intervals in data.table 'x' and must be integer/numeric type.") 
    if ( any(x[[xintervals[2L]]] - x[[xintervals[1L]]] < 0L) )
        stop("All entries in column ", xintervals[1L], " should be <= corresponding entries in column ", xintervals[2L], " in data.table 'x'")
    if (!storage.mode(y[[yintervals[1L]]]) %chin% c("double", "integer") || !storage.mode(y[[yintervals[2L]]]) %chin% c("double", "integer"))
        stop("The last two columns in by.y should correspond to the 'start' and 'end' intervals in data.table 'y' and must be integer/numeric type.") 
    if ( any(y[[yintervals[2L]]] - y[[yintervals[1L]]] < 0L) )
        stop("All entries in column ", yintervals[1L], " should be <= corresponding entries in column ", yintervals[2L], " in data.table 'y'")

    ## hopefully all checks are over. Now onto the actual task at hand.
    shallow <- function(x, cols) {
        xx = as.list(x)[cols]
        setDT(xx)
    }
    origx = x; x = shallow(x, by.x)
    origy = y; y = shallow(y, by.y)
    if (identical(by.x, key(origx)[seq_along(by.x)]))
        setattr(x, 'sorted', by.x)
    setattr(y, 'sorted', by.y) ## is definitely sorted on by.y
    roll = switch(type, start=, end=, equal= FALSE, 
                    any=, within= TRUE)
    make_call = function(names, fun=NULL) {
        if (is.character(names))
            names = lapply(names, as.name)
        call = c(substitute(fun, list(fun=fun)), names)
        if (!is.null(fun)) as.call(call) else call
    }
    construct = function(icols, mcols, type=type) {
        icall = make_call(icols)
        setattr(icall, 'names', icols)
        mcall = make_call(mcols, quote(c))
        if (type %chin% c("within", "any")) {
            mcall[[3L]] = substitute(val+1L, 
                list(val = mcall[[3L]]))
        }
        make_call(c(icall, pos=mcall), quote(list))
    }
    uycols = switch(type, start = yintervals[1L], 
                end = yintervals[2L], any =, 
                within =, equal = yintervals)
    call = construct(head(ynames, -2L), uycols, type)
    if (verbose) {last.started.at=proc.time()[3];cat("unique() + setkey() operations done in ...");flush.console()}
    uy = unique(y[, eval(call)])
    setkey(uy)[, `:=`(lookup = list(list(integer(0))), type_lookup = list(list(integer(0))), count=0L, type_count=0L)]
    if (verbose) {cat(round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}
    matches <- function(ii, xx, del, ...) {
        cols = setdiff(names(xx), del)
        xx = shallow(xx, cols)
        ii[xx, which=TRUE, ...]
    }
    indices = function(x, y, intervals, ...) {
        if (type == "start") {
            sidx = eidx = matches(x, y, intervals[2L], ...) ## TODO: eidx can be set to integer(0)
        } else if (type == "end") {
            eidx = sidx = matches(x, y, intervals[1L], ...) ## TODO: sidx can be set to integer(0)
        } else {
            rollends = ifelse(type == "any", TRUE, FALSE)
            sidx = matches(x, y, intervals[2L], rollends=rollends, ...)
            eidx = matches(x, y, intervals[1L], ...)
        }
        list(sidx, eidx)
    }
    .Call(Clookup, uy, nrow(y), indices(uy, y, yintervals, roll=roll), maxgap, minoverlap, mult, type, verbose)
    if (maxgap == 0L && minoverlap == 1L) {
        iintervals = tail(names(x), 2L)
        if (verbose) {last.started.at=proc.time()[3];cat("binary search(es) done in ...");flush.console()}
        xmatches = indices(uy, x, xintervals, nomatch=0L, roll=roll)
        if (verbose) {cat(round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}
        olaps = .Call(Coverlaps, uy, xmatches, mult, type, nomatch, verbose)
    } else if (maxgap == 0L && minoverlap > 1L) {
        stop("Not yet implemented")
    } else if (maxgap > 0L && minoverlap == 1L) {
        stop("Not yet implemented")
    } else if (maxgap > 0L && minoverlap > 1L) {
        if (maxgap > minoverlap)
            warning("maxgap > minoverlap. maxgap will have no effect here.")
        stop("Not yet implemented")
    }
    setDT(olaps)
    setnames(olaps, c("xid", "yid"))
    yid = NULL  # for 'no visible binding for global variable' from R CMD check on i clauses below
    # if (type == "any") setorder(olaps) # at times the combine operation may not result in sorted order
    if (which) {
        if (mult %chin% c("first", "last"))
            return (olaps$yid)
        else if (!is.na(nomatch))
            return (olaps[yid > 0L])
        else return (olaps)
    } else {
        if (!is.na(nomatch))
            olaps = olaps[yid > 0L]
        ycols = setdiff(names(origy), head(by.y, -2L))
        idx = chmatch(ycols, names(origx), nomatch=0L)
        ans = origx[olaps$xid]
        if (any(idx>0L))
            setnames(ans, names(ans)[idx], paste("i.", names(ans)[idx], sep=""))
        xcols1 = head(by.x, -2L)
        xcols2 = setdiff(names(ans), xcols1)
        ans[, (ycols) := origy[olaps$yid, ycols, with=FALSE]]
        setcolorder(ans, c(xcols1, ycols, xcols2))
        return (ans)
    }
}
