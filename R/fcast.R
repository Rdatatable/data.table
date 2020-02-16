guess = function(x) {
  if ("value" %chin% names(x))
    return("value")
  if ("(all)" %chin% names(x))
    return("(all)")
  var = names(x)[ncol(x)]
  message("Using '", var, "' as value column. Use 'value.var' to override")
  return(var)
}

dcast <- function(
  data, formula, fun.aggregate = NULL, ..., margins = NULL,
  subset = NULL, fill = NULL, value.var = guess(data)
) {
  if (is.data.table(data)) UseMethod("dcast", data)
  # nocov start
  else {
    data_name = deparse(substitute(data))
    ns = tryCatch(getNamespace("reshape2"), error=function(e)
      stop("The dcast generic in data.table has been passed a ",class(data)[1L],", but data.table::dcast currently only has a method for data.tables. Please confirm your input is a data.table, with setDT(", data_name, ") or as.data.table(", data_name, "). If you intend to use a reshape2::dcast, try installing that package first, but do note that reshape2 is deprecated and you should be migrating your code away from using it."))
    warning("The dcast generic in data.table has been passed a ", class(data)[1L], " and will attempt to redirect to the reshape2::dcast; please note that reshape2 is deprecated, and this redirection is now deprecated as well. Please do this redirection yourself like reshape2::dcast(", data_name, "). In the next version, this warning will become an error.")
    ns$dcast(data, formula, fun.aggregate = fun.aggregate, ..., margins = margins,
             subset = subset, fill = fill, value.var = value.var)
  }
  # nocov end
}

check_formula = function(formula, varnames, valnames) {
  if (is.character(formula)) formula = as.formula(formula)
  if (!inherits(formula, "formula") || length(formula) != 3L)
    stop("Invalid formula. Cast formula should be of the form LHS ~ RHS, for e.g., a + b ~ c.")  # nocov; couldn't find a way to construct a test formula with length!=3L
  vars = all.vars(formula)
  vars = vars[!vars %chin% c(".", "...")]
  allvars = c(vars, valnames)
  if (any(allvars %chin% varnames[duplicated(varnames)]))
    stop('data.table to cast must have unique column names')
  deparse_formula(as.list(formula)[-1L], varnames, allvars)
}

deparse_formula = function(expr, varnames, allvars) {
  lvars = lapply(expr, function(this) {
    if (this %iscall% '+') {
      unlist(deparse_formula(as.list(this)[-1L], varnames, allvars))
    } else if (is.name(this) && this==quote(`...`)) {
      subvars = setdiff(varnames, allvars)
      lapply(subvars, as.name)
    } else this
  })
  lvars = lapply(lvars, function(x) if (length(x) && !is.list(x)) list(x) else x)
}

value_vars = function(value.var, varnames) {
  if (is.character(value.var))
    value.var = list(value.var)
  value.var = lapply(value.var, unique)
  valnames = unique(unlist(value.var))
  iswrong = which(!valnames %chin% varnames)
  if (length(iswrong))
    stop("value.var values [", paste(value.var[iswrong], collapse=", "), "] are not found in 'data'.")
  value.var
}

aggregate_funs = function(funs, vals, sep="_", ...) {
  if (funs %iscall% 'eval')
    funs = eval(funs[[2L]], parent.frame(2L), parent.frame(2L))
  if (funs %iscall% c('c', 'list')) {
    funs = lapply(as.list(funs)[-1L], function(x) {
      if (x %iscall% c('c', 'list')) as.list(x)[-1L] else x
    })
  } else funs = eval(funs, parent.frame(2L), parent.frame(2L))
  if(is.function(funs)) funs = list(funs) # needed for cases as shown in test#1700.1
  if (length(funs) != length(vals)) {
    if (length(vals) == 1L)
      vals = replicate(length(funs), vals)
    else stop("When 'fun.aggregate' and 'value.var' are both lists, 'value.var' must be either of length =1 or =length(fun.aggregate).")
  }
  only_one_fun = length(unlist(funs)) == 1L
  dots = list(...)
  construct_funs = function(fun, nm, val) {
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
        nms[k] = if (only_one_fun) j else paste(j, nm, sep=sep)
        k = k+1L;
      }
    }
    setattr(ans, 'names', nms)
  }
  ans = lapply(seq_along(funs), function(i) {
    nm = names(funs[i])
    if (is.null(nm) || !nzchar(nm)) {
      nm = all.names(funs[[i]], max.names=1L, functions=TRUE)
    }
    if (!length(nm)) nm <- paste0("fun", i)
    construct_funs(funs[i], nm, vals[[i]])
  })
  as.call(c(quote(list), unlist(ans)))
}

dcast.data.table = function(data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = guess(data), verbose = getOption("datatable.verbose")) {
  if (!is.data.table(data)) stop("'data' must be a data.table.")
  drop = as.logical(rep(drop, length.out=2L))
  if (anyNA(drop)) stop("'drop' must be logical TRUE/FALSE")
  # #2980 if explicitly providing fun.aggregate=length but not a value.var,
  #   just use the last column (as guess(data) would do) because length will be
  #   the same on all columns
  if (missing(value.var) && !missing(fun.aggregate) && identical(fun.aggregate, length))
    value.var = names(data)[ncol(data)]
  lvals = value_vars(value.var, names(data))
  valnames = unique(unlist(lvals))
  lvars = check_formula(formula, names(data), valnames)
  lvars = lapply(lvars, function(x) if (length(x)) x else quote(`.`))
  # tired of lapply and the way it handles environments!
  allcols = c(unlist(lvars), lapply(valnames, as.name))
  dat = vector("list", length(allcols))
  for (i in seq_along(allcols)) {
    x = allcols[[i]]
    dat[[i]] = if (identical(x, quote(`.`))) rep(".", nrow(data)) else eval(x, data, parent.frame())
    if (is.function(dat[[i]]))
      stop("Column [", deparse(x), "] not found or of unknown type.")
  }
  setattr(lvars, 'names', c("lhs", "rhs"))
  # Have to take care of duplicate names, and provide names for expression columns properly.
  varnames = make.unique(vapply_1c(unlist(lvars), all.vars, max.names=1L), sep=sep)
  dupidx = which(valnames %chin% varnames)
  if (length(dupidx)) {
    dups = valnames[dupidx]
    valnames = tail(make.unique(c(varnames, valnames)), -length(varnames))
    lvals = lapply(lvals, function(x) { x[x %chin% dups] = valnames[dupidx]; x })
  }
  lhsnames = head(varnames, length(lvars$lhs))
  rhsnames = tail(varnames, -length(lvars$lhs))
  setattr(dat, 'names', c(varnames, valnames))
  if (any(vapply_1b(dat[varnames], is.list))) {
    stop("Columns specified in formula can not be of type list")
  }
  setDT(dat)

  m = as.list(match.call()[-1L])
  subset = m[["subset"]][[2L]]
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
    if (attr(oo, 'maxgrpn', exact=TRUE) > 1L) {
      message("Aggregate function missing, defaulting to 'length'")
      fun.call = quote(length)
    }
  }
  if (!is.null(fun.call)) {
    fun.call = aggregate_funs(fun.call, lvals, sep, ...)
    errmsg = "Aggregating function(s) should take vector inputs and return a single value (length=1). However, function(s) returns length!=1. This value will have to be used to fill any missing combinations, and therefore must be length=1. Either override by setting the 'fill' argument explicitly or modify your function to handle this case appropriately."
    if (is.null(fill)) {
      fill.default = suppressWarnings(dat[0L][, eval(fun.call)])
      # tryCatch(fill.default <- dat[0L][, eval(fun.call)], error = function(x) stop(errmsg, call.=FALSE))
      if (nrow(fill.default) != 1L) stop(errmsg, call.=FALSE)
    }
    dat = dat[, eval(fun.call), by=c(varnames)]
  }
  order_ = function(x) {
    o = forderv(x, retGrp=TRUE, sort=TRUE)
    idx = attr(o, 'starts', exact=TRUE)
    if (!length(o)) o = seq_along(x)
    o[idx] # subsetVector retains attributes, using R's subset for now
  }
  cj_uniq = function(DT) {
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
      map = setDT(lapply(list(lhsnames, rhsnames), function(cols) frankv(dat, cols=cols, ties.method="dense", na.last=FALSE))) # #2202 fix
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
    idx = do.call("CJ", mapunique)[map, 'I' := .I][["I"]] # TO DO: move this to C and avoid materialising the Cross Join.
    ans = .Call(Cfcast, lhs, val, maplen[[1L]], maplen[[2L]], idx, fill, fill.default, is.null(fun.call))
    allcols = do.call("paste", c(rhs, sep=sep))
    if (length(valnames) > 1L)
      allcols = do.call("paste", if (identical(".", allcols)) list(valnames, sep=sep)
            else c(CJ(valnames, allcols, sorted=FALSE), sep=sep))
      # removed 'setcolorder()' here, #1153
    setattr(ans, 'names', c(lhsnames, allcols))
    setDT(ans); setattr(ans, 'sorted', lhsnames)
  } else stop("Internal error -- empty rhsnames in dcast; please report") # nocov
  return (ans)
}
