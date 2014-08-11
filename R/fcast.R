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
            stop("Cast formula length is > 2, must be = 2.")
        ff <- strsplit(ff, "+", fixed=TRUE)
        setattr(ff, 'names', c("ll", "rr"))
        ff <- lapply(ff, function(x) x[x != "."])
        ff_ <- unlist(ff, use.names=FALSE)
        replace_dots <- function(x) {
            if (!is.list(x)) x = as.list(x)
            for (i in seq_along(x)) {
                if (x[[i]] == "...") 
                    x[[i]] = setdiff(names(data), c(value.var, ff_))
            }
            unlist(x)
        }
        ff <- lapply(ff, replace_dots)
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

    # subset
    m <- as.list(match.call()[-1])
    subset <- m$subset[[2]]
    if (!is.null(subset)) {
        if (is.name(subset)) subset = as.call(list(quote(`(`), subset))
        data = data[eval(subset, data, parent.frame()), unique(c(ff_, value.var)), with=FALSE]
    }
    if (nrow(data) == 0L || ncol(data) == 0L) stop("Can't 'cast' on an empty data.table")

    # set 'fun.aggregate = length' if max group size > 1
    fun.null=FALSE
    if (is.null(fun.aggregate)) {
        fun.null=TRUE
        oo = forderv(data, by=ff_, retGrp=TRUE)
        if (attr(oo, 'maxgrpn') > 1L) {
            message("Aggregate function missing, defaulting to 'length'")
            fun.aggregate <- length
            m[["fun.aggregate"]] = quote(length)
        }
    }
    fill.default <- NULL
    if (!is.null(fun.aggregate)) { # construct the 'call'
        fill.default = fun.aggregate(data[[value.var]][0], ...)
        if (!length(fill.default) && (is.null(fill) || !length(fill)))
            stop("Aggregating function provided to argument 'fun.aggregate' should always return a length 1 vector, but returns 0-length value for fun.aggregate(", typeof(data[[value.var]]), "(0)).", " This value will have to be used to fill missing combinations, if any, and therefore can not be of length 0. Either override by setting the 'fill' argument explicitly or modify your function to handle this case appropriately.")
        args <- c("data", "formula", "margins", "subset", "fill", "value.var", "verbose", "drop")
        m <- m[setdiff(names(m), args)]
        .CASTcall = as.call(c(m[1], as.name(value.var), m[-1])) # issues/713
        .CASTcall = as.call(c(as.name("list"), setattr(list(.CASTcall), 'names', value.var)))
        # workaround until #5191 (issues/497) is fixed
        if (length(intersect(value.var, ff_))) 
            .CASTcall = as.call(list(as.name("{"), as.name(".SD"), .CASTcall))
    }
    # special case
    if (length(ff$rr) == 0) {
        if (is.null(fun.aggregate))
            ans = data[, c(ff$ll, value.var), with=FALSE]
        else {
            # workaround until #5191 (issues/497) is fixed
            if (length(intersect(value.var, ff_))) ans = data[, eval(.CASTcall), by=c(ff$ll), .SDcols=value.var]
            else ans = data[, eval(.CASTcall), by=c(ff$ll)]
        }
        if (anyDuplicated(names(ans))) {
            message("Duplicate column names found in cast data.table. Setting unique names using 'make.unique'")   
            setnames(ans, make.unique(names(ans)))
        }
        if (!identical(key(ans), ff$ll)) setkeyv(ans, names(ans)[seq_along(ff$ll)])
        return(ans)
    }
    # aggregation moved to R now that 'adhoc-by' is crazy fast!
    if (!is.null(fun.aggregate)) {
        if (length(intersect(value.var, ff_))) {
            data = data[, eval(.CASTcall), by=c(ff_), .SDcols=value.var]
            value.var = tail(make.unique(names(data)), 1L)
            setnames(data, ncol(data), value.var)
        }
        else data = data[, eval(.CASTcall), by=c(ff_)]
        setkeyv(data, ff_)
        # issues/693
        fun_agg_chk <- function(x) {
            # sorted now, 'forderv' should be as fast as uniqlist+uniqlengths
            oo = forderv(data, by=key(data), retGrp=TRUE)
            attr(oo, 'maxgrpn') > 1L
        }
        if (!fun.null && fun_agg_chk(data))
            stop("Aggregating function provided to argument 'fun.aggregate' should always return a length 1 vector for each group, but returns length != 1 for atleast one group. Please have a look at the DETAILS section of ?dcast.data.table ")
    } else {
        if (is.null(subset))
            data = data[, unique(c(ff_, value.var)), with=FALSE] # data is untouched so far. subset only required columns
        if (length(oo)) .Call(Creorder, data, oo)
        setattr(data, 'sorted', ff_)
    }
    .CASTenv = new.env(parent=parent.frame())
    assign("forder", forderv, .CASTenv)
    assign("CJ", CJ, .CASTenv)
    ans <- .Call("Cfcast", data, ff$ll, ff$rr, value.var, fill, fill.default, is.null(fun.aggregate), .CASTenv, drop)
    setDT(ans)
    if (anyDuplicated(names(ans))) {
        message("Duplicate column names found in cast data.table. Setting unique names using 'make.unique'")
        setnames(ans, make.unique(names(ans)))
    }
    setattr(ans, 'sorted', names(ans)[seq_along(ff$ll)])
    ans
}
