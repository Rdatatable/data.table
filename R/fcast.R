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
    subset = NULL, fill = NULL, drop = TRUE, value.var = guess(data), tolerance = .Machine$double.eps^0.5, 
           verbose = getOption("datatable.verbose")) {
    if (!is.data.table(data)) stop("'data' must be a data.table.")
    is.formula <- function(x) class(x) == "formula"
    strip <- function(x) gsub("[[:space:]]*", "", x)
    if (is.formula(formula)) {
        formula <- deparse(formula, 500)
    }
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
    is_sorted <- FALSE # replaced is.sorted with is_sorted (as it clashes with the new function)
    # added || is.sorted(DT, ff_) after Matthew added is.sorted - now we can check for "sortedness" even if key isn't set
    if ((!is.null(key(data)) && length(ff_) <= length(key(data)) && all(key(data) == ff_[1:length(key(data))])) || is.sorted(data, ff_)) 
        is_sorted = TRUE
    # TO DO: better way... not sure how else to get an expression from function (in fun.aggregate)
    m <- as.list(match.call()[-1])
    subset <- m$subset[[2]]
    if (!is.null(subset))
        vars <- intersect(names(data), all.vars(subset))
    else vars <- NULL
    fill.default <- NULL
    if (!is.null(fun.aggregate)) {
        fill.default = fun.aggregate(data[[value.var]][0], ...)
        args <- c("data", "formula", "margins", "subset", "fill", "value.var", "tolerance", "verbose", "drop")
        m <- m[setdiff(names(m), args)]
        if (getOption("datatable.optimize") > 0L && m[[1]] == "mean") {
            fun.aggregate <- as.call(c(as.name(".External"), as.name("Cfastmean"), as.name(value.var), 
                                  if(!is.null(m[["na.rm"]])) list(m[["na.rm"]]) else list(FALSE)))
        } else {
            fun.aggregate <- as.call(c(m[1], as.name(value.var), m[-1]))
        }
        # make sure list columns on aggregation gives back a list column - have to do this because grouping returns a list only with list(list(.))
        if (is.list(data[[value.var]]) || is.list(fill.default)) fun.aggregate <- as.call(c(as.name("list"), list(fun.aggregate)))
    }
    if (length(ff$rr) == 0) {
        # probably simple formula - should be okay to deal in R
        agg = data[, .N, keyby=c(ff$ll)] # if any N > 1, then default to length, else return data
        if (all(agg$N == 1L)) {
            ans = data[, c(ff$ll, value.var), with=FALSE]
            if (!identical(key(ans), ff$ll)) setkeyv(ans, ff$ll)
            return(ans)
        }
        if (is.null(fun.aggregate)) {
            message("Aggregate function missing, defaulting to 'length'")
            return(agg)
        } else return(data[, eval(fun.aggregate), keyby=c(ff$ll)])
    }
    .CASTenv = new.env(parent=parent.frame())
    assign("forder", forder, .CASTenv)
    assign("print", function(x,...){base::print(x,...);NULL}, .CASTenv)
    assign("Cfastmean", Cfastmean, .CASTenv)
    assign("mean", base::mean.default, .CASTenv)
    if (!is.null(vars)) for (i in vars) assign(i, data[[i]], .CASTenv) # assign subset vars directly in env
    ans <- .Call("Cfcast", data, ff$ll, ff$rr, value.var, fill, tolerance, .CASTenv, is_sorted, fun.aggregate, fill.default, drop, subset)
    setDT(ans)
    if (any(duplicated(names(ans)))) {
        message("Duplicate column names found in cast data.table. Setting unique names using 'make.names'")   
        setnames(ans, make.unique(names(ans)))
    }
    setattr(ans, 'sorted', names(ans)[seq_along(ff$ll)])
    ans
}
