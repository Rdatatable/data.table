guess <- function(x) {
    if ("value" %chin% names(x))
        return("value")
    if ("(all)" %chin% names(x)) 
        return("(all)")
    var <- names(x)[ncol(x)]
    message("Using '", var, "' as value column. Use 'value.var' to override")
    return(var)
}

dcast.data.table <- function(data, formula, fun.aggregate = NULL, ..., margins = NULL, 
    subset = NULL, fill = NULL, drop = TRUE, value.var = guess(data), verbose = getOption("datatable.verbose")) {
    if (!is.data.table(data)) stop("'data' must be a data.table.")
    if (anyDuplicated(names(data))) stop('data.table to cast must have unique column names')
    is.formula <- function(x) class(x) == "formula"
    strip <- function(x) gsub("[[:space:]]*", "", x)
    if (is.formula(formula)) formula <- deparse(formula, 500)
    if (is.character(formula)) {
        ff <- strsplit(strip(formula), "~", fixed=TRUE)[[1]]
        if (length(ff) > 2)
            stop("Cast formula of length > 2 detected. Data.table has at most two output dimensions.")
        ff <- strsplit(ff, "+", fixed=TRUE)
        setattr(ff, 'names', c("ll", "rr"))
        ff <- lapply(ff, function(x) x[x != "."])
        ff_ <- unlist(ff, use.names=FALSE)
        ff <- lapply(ff, function(x) if (any(x == "...")) c(x[x != "..."], setdiff(names(data), c(value.var, ff_))) else x)
    } else stop("Invalid formula.")
    ff_ <- unlist(ff, use.names=FALSE)
    if (length(is_wrong <- which(is.na(chmatch(ff_, names(data))))) > 0) stop("Column '", ff_[is_wrong[1]], "' not found.")
    if (length(ff$ll) == 0) stop("LHS of formula evaluates to 'character(0)', invalid formula.")
    if (length(value.var) != 1 || !is.character(value.var)) stop("'value.var' must be a character vector of length 1.")
    if (is.na(chmatch(value.var, names(data)))) stop("'value.var' column '", value.var, "' not found.")
    if (any(unlist(lapply(as.list(data)[ff_], class), use.names=FALSE) == "list")) 
        stop("Only 'value.var' column maybe of type 'list'. This may change in the future.")
    drop <- as.logical(drop[1])
    if (is.na(drop)) stop("'drop' must be TRUE/FALSE")

    # deal with 'subset' first
    m <- as.list(match.call()[-1])
    subset <- m$subset[[2]]
    if (!is.null(subset)) data = data[eval(subset), unique(c(ff_, value.var)), with=FALSE] # TODO: revisit. Maybe too costly on large data

    # if original or subset'd data.table has 0 rows or cols, error.
    if (nrow(data) == 0L || ncol(data) == 0L) stop("Can't 'cast' on an empty data.table")

    # next, check and set 'fun.aggregate = length' it's null but at least one group size is > 1.
    oo = 0L
    if (is.null(fun.aggregate)) {
        oo = forderv(data, by=ff_, retGrp=TRUE) # to check if the maximum group size is > 1 and is TRUE set fun.aggregate to length if it's NULL
        if (attr(oo, 'maxgrpn') > 1L) {
            message("Aggregate function missing, defaulting to 'length'")
            fun.aggregate <- length
            m[["fun.aggregate"]] = quote(length)
        }
    }
    # TO DO: better way... not sure how else to get an expression from function (in fun.aggregate)
    fill.default <- NULL
    if (!is.null(fun.aggregate)) {
        fill.default = fun.aggregate(data[[value.var]][0], ...)
        args <- c("data", "formula", "margins", "subset", "fill", "value.var", "verbose", "drop")
        m <- m[setdiff(names(m), args)]
        fun.aggregate <- as.call(c(m[1], as.name(value.var), m[-1]))
        fun.aggregate <- as.call(c(as.name("list"), setattr(list(fun.aggregate), 'names', value.var)))
        # checking for #5191 (until fixed, this is a workaround
        if (length(intersect(value.var, ff_))) fun.aggregate = as.call(list(as.name("{"), as.name(".SD"), fun.aggregate))
    }
    if (length(ff$rr) == 0) {
        if (is.null(fun.aggregate)) 
            ans = data[, c(ff$ll, value.var), with=FALSE]
        else {
            # checking for #5191 (until fixed, this is a workaround
            if (length(intersect(value.var, ff_))) ans = data[, eval(fun.aggregate), by=c(ff$ll), .SDcols=value.var]
            else ans = data[, eval(fun.aggregate), by=c(ff$ll)]
        }
        if (any(duplicated(names(ans)))) {
            message("Duplicate column names found in cast data.table. Setting unique names using 'make.names'")   
            setnames(ans, make.unique(names(ans)))
        }
        if (!identical(key(ans), ff$ll)) setkeyv(ans, names(ans)[seq_along(ff$ll)])
        return(ans)
    }
    # if fun.aggregate exists, then aggregate in R-side (now that 'adhoc-by' is extremely fast!)
    if (!is.null(fun.aggregate)) {
        if (length(intersect(value.var, ff_))) {
            data = data[, eval(fun.aggregate), by=c(ff_), .SDcols=value.var]
            value.var = tail(make.unique(names(data)), 1L)
            setnames(data, ncol(data), value.var)
        }
        else data = data[, eval(fun.aggregate), by=c(ff_)]
        setkeyv(data, ff_) # can't use 'oo' here, but should be faster as it's uncommon to have huge number of groups.
    }
    if (is.null(subset) && is.null(fun.aggregate)) {
        # 'data' has not been modified yet, so setkey and go to C.
        data = data[, unique(c(ff_, value.var)), with=FALSE] # we need the copy. using subsetting instead of copy(.) ensures copying of only required columns
        if (length(oo) && oo == 0L) setkeyv(data, ff_)
        else { # we can avoid 'forderv' as it's already done
            if (length(oo)).Call(Creorder, data, oo)
            setattr(data, 'sorted', ff_)
        }
    }
    .CASTenv = new.env(parent=parent.frame())
    assign("forder", forderv, .CASTenv)
    ans <- .Call("Cfcast", data, ff$ll, ff$rr, value.var, fill, fill.default, is.null(fun.aggregate), .CASTenv, drop)
    setDT(ans)
    if (any(duplicated(names(ans)))) {
        message("Duplicate column names found in cast data.table. Setting unique names using 'make.names'")   
        setnames(ans, make.unique(names(ans)))
    }
    setattr(ans, 'sorted', names(ans)[seq_along(ff$ll)])
    ans
}
