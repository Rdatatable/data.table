guess <- function(x) {
    if ("value" %chin% names(x))
        return("value")
    if ("(all)" %chin% names(x)) 
        return("(all)")
    var <- names(x)[ncol(x)]
    message("Using '", var, "' as value column. Use 'value.var' to override")
    return(var)
}

dcast <- function(data, formula, fun.aggregate = NULL, ..., margins = NULL, 
            subset = NULL, fill = NULL, value.var = guess(data)) {
    if (is.data.table(data)) 
        UseMethod("dcast", data)
    else 
        reshape2::dcast(data, formula, fun.aggregate = fun.aggregate, ..., margins = margins, 
            subset = subset, fill = fill, value.var = value.var)
}

check_formula <- function(formula, varnames, valnames) {
    if (is.character(formula)) formula = as.formula(formula)
    if (class(formula) != "formula" || length(formula) != 3L)
        stop("Invalid formula. Cast formula should be of the form LHS ~ RHS, for e.g., a + b ~ c.")
    vars = all.vars(formula)
    vars = vars[!vars %chin% c(".", "...")]
    allvars = c(vars, valnames)
    if (any(allvars %in% varnames[duplicated(varnames)])) 
      stop('data.table to cast must have unique column names')
    ans = deparse_formula(as.list(formula)[-1L], varnames, allvars)
}

deparse_formula <- function(expr, varnames, allvars) {
    lvars = lapply(expr, function(this) {
        if (is.call(this)) {
            if (this[[1L]] == quote(`+`))
                unlist(deparse_formula(as.list(this)[-1L], varnames, allvars))
            else this
        } else if (is.name(this)) {
            if (this == quote(`...`)) {
                subvars = setdiff(varnames, allvars)
                lapply(subvars, as.name)
            } else this
        }
    })
    lvars = lapply(lvars, function(x) if (length(x) && !is.list(x)) list(x) else x)
}

value_vars <- function(value.var, varnames) {
    if (is.character(value.var))
        value.var = list(value.var)
    value.var = lapply(value.var, unique)
    valnames = unique(unlist(value.var))
    iswrong = which(!valnames %in% varnames)
    if (length(iswrong))
        stop("value.var values [", paste(value.var[iswrong], collapse=", "), "] are not found in 'data'.")
    value.var    
}

aggregate_funs <- function(funs, vals, sep="_", ...) {
    if (is.call(funs) && funs[[1L]] == "eval") 
        funs = eval(funs[[2L]], parent.frame(2L), parent.frame(2L))
    if (is.call(funs) && as.character(funs[[1L]]) %in% c("c", "list")) 
        funs = lapply(as.list(funs)[-1L], function(x) {
            if (is.call(x) && as.character(x[[1L]]) %in% c("c", "list")) as.list(x)[-1L] else x
        })
    else funs = list(funs)
    if (length(funs) != length(vals)) {
        if (length(vals) == 1L)
            vals = replicate(length(funs), vals)
        else stop("When 'fun.aggregate' and 'value.var' are both lists, 'value.var' must be either of length =1 or =length(fun.aggregate).")
    }
    only_one_fun = length(unlist(funs)) == 1L
    dots = list(...)
    construct_funs <- function(fun, val) {
        if (!is.list(fun)) fun = list(fun)
        ans = vector("list", length(fun)*length(val))
        nms = vector("character", length(ans))
        k = 1L
        for (i in fun) {
            for (j in val) {
                expr = list(i, as.name(j))
                if (length(dots))
                    expr = c(expr, dots)
                ans[[k]] = as.call(expr)
                # changed order of arguments here, #1153
                nms[k] = if (only_one_fun) j else 
                            paste(j, all.names(i, max.names=1L, functions=TRUE), sep=sep)
                k = k+1L;
            }
        }
        setattr(ans, 'names', nms)
    }
    ans = mapply(construct_funs, funs, vals, SIMPLIFY=FALSE)
    as.call(c(quote(list), unlist(ans)))
}

dcast.data.table <- function(data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = guess(data), verbose = getOption("datatable.verbose")) {
    if (!is.data.table(data)) stop("'data' must be a data.table.")
    drop = as.logical(rep(drop, length.out=2L))
    if (any(is.na(drop))) stop("'drop' must be logical TRUE/FALSE")
    lvals = value_vars(value.var, names(data))
    valnames = unique(unlist(lvals))
    lvars = check_formula(formula, names(data), valnames)
    lvars = lapply(lvars, function(x) if (!length(x)) quote(`.`) else x)
    # tired of lapply and the way it handles environments!
    allcols = c(unlist(lvars), lapply(valnames, as.name))
    dat = vector("list", length(allcols))
    for (i in seq_along(allcols)) {
        x = allcols[[i]]
        dat[[i]] = if (identical(x, quote(`.`))) rep(".", nrow(data)) 
                      else eval(x, data, parent.frame())
        if (is.function(dat[[i]]))
            stop("Column [", deparse(x), "] not found or of unknown type.")
    }
    setattr(lvars, 'names', c("lhs", "rhs"))
    # Have to take care of duplicate names, and provide names for expression columns properly.
    varnames = make.unique(vapply_1c(unlist(lvars), all.vars, max.names=1L), sep=sep)
    dupidx = which(valnames %in% varnames)
    if (length(dupidx)) {
        dups = valnames[dupidx]
        valnames = tail(make.unique(c(varnames, valnames)), -length(varnames))
        lvals = lapply(lvals, function(x) { x[x %in% dups] = valnames[dupidx]; x })
    }
    lhsnames = head(varnames, length(lvars$lhs))
    rhsnames = tail(varnames, -length(lvars$lhs))
    setattr(dat, 'names', c(varnames, valnames))
    setDT(dat)
    if (any(vapply_1b(as.list(dat)[varnames], is.list))) {
        stop("Columns specified in formula can not be of type list")
    }
    m <- as.list(match.call()[-1L])
    subset <- m[["subset"]][[2L]]
    if (!is.null(subset)) {
        if (is.name(subset)) subset = as.call(list(quote(`(`), subset))
        idx = which(eval(subset, data, parent.frame())) # any advantage thro' secondary keys?
        dat = .Call(CsubsetDT, dat, idx, seq_along(dat))
    }
    if (!nrow(dat) || !ncol(dat)) stop("Can not cast an empty data.table")
    fun.call = m[["fun.aggregate"]]
    fill.default = NULL
    if (is.null(fun.call)) {
        oo = forderv(dat, by=varnames, retGrp=TRUE)
        if (attr(oo, 'maxgrpn') > 1L) {
            message("Aggregate function missing, defaulting to 'length'")
            fun.call = quote(length)
        }
    }
    if (!is.null(fun.call)) {
        fun.call = aggregate_funs(fun.call, lvals, sep, ...)
        errmsg = "Aggregating function(s) should take vector inputs and return a single value (length=1). However, function(s) returns length!=1. This value will have to be used to fill any missing combinations, and therefore must be length=1. Either override by setting the 'fill' argument explicitly or modify your function to handle this case appropriately."
        if (is.null(fill)) {
            fill.default <- suppressWarnings(dat[0][, eval(fun.call)])
            # tryCatch(fill.default <- dat[0][, eval(fun.call)], error = function(x) stop(errmsg, call.=FALSE))
            if (nrow(fill.default) != 1L) stop(errmsg, call.=FALSE)
        }
        if (!any(valnames %chin% varnames)) {
            dat = dat[, eval(fun.call), by=c(varnames)]
        } else {
            dat = dat[, { .SD; eval(fun.call) }, by=c(varnames), .SDcols = valnames]
        }
    }
    order_ <- function(x) {
        o = forderv(x, retGrp=TRUE, sort=TRUE)
        idx = attr(o, 'starts')
        if (!length(o)) o = seq_along(x)
        o[idx] # subsetVector retains attributes, using R's subset for now
    }
    cj_uniq <- function(DT) {
        do.call("CJ", lapply(DT, function(x) 
            if (is.factor(x)) {
                xint = seq_along(levels(x))
                setattr(xint, 'levels', levels(x))
                setattr(xint, 'class', class(x))
            } else .Call(CsubsetVector, x, order_(x))
    ))}
    valnames = setdiff(names(dat), varnames)
    # 'dat' != 'data'? then setkey to speed things up (slightly), else ad-hoc (for now). Still very fast!
    if (!is.null(fun.call) || !is.null(subset)) 
        setkeyv(dat, varnames)
    if (length(rhsnames)) {
        lhs = shallow(dat, lhsnames); rhs = shallow(dat, rhsnames); val = shallow(dat, valnames)
        # handle drop=TRUE/FALSE - Update: Logic moved to R, AND faster than previous version. Take that... old me :-).
        if (all(drop)) {
            map = setDT(lapply(list(lhsnames, rhsnames), function(cols) frankv(dat, cols=cols, ties.method="dense")))
            maporder = lapply(map, order_)
            mapunique = lapply(seq_along(map), function(i) .Call(CsubsetVector, map[[i]], maporder[[i]]))
            lhs = .Call(CsubsetDT, lhs, maporder[[1L]], seq_along(lhs))
            rhs = .Call(CsubsetDT, rhs, maporder[[2L]], seq_along(rhs))
        } else {
            lhs_ = if (!drop[1L]) cj_uniq(lhs) else setkey(unique(lhs, by=names(lhs)))
            rhs_ = if (!drop[2L]) cj_uniq(rhs) else setkey(unique(rhs, by=names(rhs)))
            map = vector("list", 2L)
            .Call(Csetlistelt, map, 1L, lhs_[lhs, which=TRUE])
            .Call(Csetlistelt, map, 2L, rhs_[rhs, which=TRUE])
            setDT(map)
            mapunique = vector("list", 2L)
            .Call(Csetlistelt, mapunique, 1L, seq_len(nrow(lhs_)))
            .Call(Csetlistelt, mapunique, 2L, seq_len(nrow(rhs_)))
            lhs = lhs_; rhs = rhs_
        }
        maplen = vapply_1i(mapunique, length)
        idx = do.call("CJ", mapunique)[map, I := .I][["I"]] # TO DO: move this to C and avoid materialising the Cross Join.
        ans = .Call(Cfcast, lhs, val, maplen[[1L]], maplen[[2L]], idx, fill, fill.default, is.null(fun.call))
        allcols = do.call("paste", c(rhs, sep=sep))
        if (length(valnames) > 1L)
            allcols = do.call("paste", if (identical(".", allcols)) list(valnames, sep=sep) 
                        else c(CJ(valnames, allcols, sorted=FALSE), sep=sep))
            # removed 'setcolorder()' here, #1153
        setattr(ans, 'names', c(lhsnames, allcols))
        setDT(ans); setattr(ans, 'sorted', lhsnames)
    } else {
        # formula is of the form x + y ~ . (rare case)
        if (drop) {
            if (is.null(subset) && is.null(fun.call)) {
                dat = copy(dat) # can't be avoided
                setkeyv(dat, lhsnames)
            }
            ans = dat
        } else {
            lhs = shallow(dat, lhsnames)
            val = shallow(dat, valnames)
            lhs_ = cj_uniq(lhs)
            idx = lhs_[lhs, I := .I][["I"]]
            lhs_[, I := NULL]
            ans = .Call(Cfcast, lhs_, val, nrow(lhs_), 1L, idx, fill, fill.default, is.null(fun.call))
            setDT(ans); setattr(ans, 'sorted', lhsnames)
            setnames(ans, c(lhsnames, valnames))
        }
        if (length(valnames) == 1L)
            setnames(ans, valnames, value.var)
    }
    return (ans)
}
