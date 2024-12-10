guess = function(x) {
  if ("value" %chin% names(x))
    return("value")
  if ("(all)" %chin% names(x))
    return("(all)")
  var = names(x)[ncol(x)]
  messagef("Using '%s' as value column. Use 'value.var' to override", var)
  var
}

dcast <- function(
  data, formula, fun.aggregate = NULL, ..., margins = NULL,
  subset = NULL, fill = NULL, value.var = guess(data)
) {
  # TODO(>=1.19.0): Remove this, just let dispatch to 'default' method fail.
  if (!is.data.table(data))
    stopf("The %1$s generic in data.table has been passed a %2$s, but data.table::%1$s currently only has a method for data.tables. Please confirm your input is a data.table, with setDT(%3$s) or as.data.table(%3$s). If you intend to use a method from reshape2, try installing that package first, but do note that reshape2 is superseded and is no longer actively developed.", "dcast", class1(data), deparse(substitute(data))) # nocov
  UseMethod("dcast", data)
}

check_formula = function(formula, varnames, valnames, value.var.in.LHSdots, value.var.in.RHSdots) {
  if (is.character(formula)) formula = as.formula(formula)
  if (!inherits(formula, "formula") || length(formula) != 3L)
    stopf("Invalid formula. Cast formula should be of the form LHS ~ RHS, for e.g., a + b ~ c.")  # nocov; couldn't find a way to construct a test formula with length!=3L
  vars = all.vars(formula)
  vars = vars[!vars %chin% c(".", "...")]
  allvars = c(vars, valnames)
  vars = setdiff(vars, valnames)
  allvarsBL = list(vars, allvars)
  if (any(allvars %chin% varnames[duplicated(varnames)]))
    stopf('data.table to cast must have unique column names')
  if (value.var.in.LHSdots == value.var.in.RHSdots && isFALSE(value.var.in.LHSdots)) {
    deparse_formula(as.list(formula)[-1L], varnames, allvars)
  }
  else {
    split_deparsing(as.list(formula)[-1L], varnames, allvarsBL[[2L - value.var.in.LHSdots]], allvarsBL[[2L - value.var.in.RHSdots]])
  }
}

split_deparsing = function(expr, varnames, LHSallvars, RHSallvars) {
  expr = list(list(expr[[1L]], LHSallvars), list(expr[[2L]], c(RHSallvars))) # assume expr[[1L]] is LHS and expr[[2L]] is RHS
  lvars = lapply(expr, function(thisList) {
    this = thisList[[1L]]
    allvars = thisList[[2L]]
    if (!is.language(this)) return(NULL)
    if (this %iscall% '+') return(unlist(deparse_formula(this[-1L], varnames, allvars)))
    if (is.name(this) && this == quote(`...`)) {
      subvars = setdiff(varnames, allvars)
      return(lapply(subvars, as.name))
    }
    this
  })
  lvars = lapply(lvars, function(x) if (length(x) && !is.list(x)) list(x) else tryCatch(unique(x), error = function(e) x))
}

deparse_formula = function(expr, varnames, allvars) {
  lvars = lapply(expr, function(this) {
    if (!is.language(this)) return(NULL)
    if (this %iscall% '+') return(unlist(deparse_formula(this[-1L], varnames, allvars)))
    if (is.name(this) && this == quote(`...`)) {
      subvars = setdiff(varnames, allvars)
      return(lapply(subvars, as.name))
    }
    this
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
    stopf("value.var values %s are not found in 'data'.", brackify(value.var[iswrong]))
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
    else stopf("When 'fun.aggregate' and 'value.var' are both lists, 'value.var' must be either of length =1 or =length(fun.aggregate).")
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

dcast.data.table = function(data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = guess(data), verbose = getOption("datatable.verbose"), value.var.in.dots = FALSE, value.var.in.LHSdots = value.var.in.dots, value.var.in.RHSdots = value.var.in.dots) {
  if (!is.data.table(data)) stopf("'data' must be a data.table.")
  drop = as.logical(rep_len(drop, 2L))
  if (anyNA(drop)) stopf("'drop' must be logical TRUE/FALSE")
  if (!isTRUEorFALSE(value.var.in.dots))
    stopf("Argument 'value.var.in.dots' should be logical TRUE/FALSE")
  if (!isTRUEorFALSE(value.var.in.LHSdots) || !isTRUEorFALSE(value.var.in.RHSdots))
    stopf("Arguments 'value.var.in.LHSdots', 'value.var.in.RHSdots' should be logical TRUE/FALSE")
  # #2980 if explicitly providing fun.aggregate=length but not a value.var,
  #   just use the last column (as guess(data) would do) because length will be
  #   the same on all columns
  if (missing(value.var) && !missing(fun.aggregate) && identical(fun.aggregate, length))
    value.var = names(data)[ncol(data)]
  lvals = value_vars(value.var, names(data))
  valnames = unique(unlist(lvals))
  lvars = check_formula(formula, names(data), valnames, value.var.in.LHSdots, value.var.in.RHSdots)
  lvars = lapply(lvars, function(x) if (length(x)) x else quote(`.`))
  # tired of lapply and the way it handles environments!
  allcols = c(unlist(lvars), lapply(valnames, as.name))
  dat = vector("list", length(allcols))
  for (i in seq_along(allcols)) {
    x = allcols[[i]]
    dat[[i]] = if (identical(x, quote(`.`))) rep(".", nrow(data)) else eval(x, data, parent.frame())
    if (is.function(dat[[i]]))
      stopf("Column [%s] not found or of unknown type.", deparse(x))
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
    stopf("Columns specified in formula can not be of type list")
  }
  setDT(dat)

  m = as.list(match.call()[-1L])
  subset = m[["subset"]][[2L]]
  if (!is.null(subset)) {
    if (is.name(subset)) subset = as.call(list(quote(`(`), subset))
    idx = which(eval(subset, data, parent.frame())) # any advantage thro' secondary keys?
    dat = .Call(CsubsetDT, dat, idx, seq_along(dat))
  }
  fun.call = m[["fun.aggregate"]]
  if (is.null(fun.call)) {
    oo = forderv(dat, by=varnames, retGrp=TRUE)
    if (attr(oo, 'maxgrpn', exact=TRUE) > 1L) {
      warningf("'fun.aggregate' is NULL, but found duplicate row/column combinations, so defaulting to length(). That is, the variables %s used in 'formula' do not uniquely identify rows in the input 'data'. In such cases, 'fun.aggregate' is used to derive a single representative value for each combination in the output data.table, for example by summing or averaging (fun.aggregate=sum or fun.aggregate=mean, respectively). Check the resulting table for values larger than 1 to see which combinations were not unique. See ?dcast.data.table for more details.", brackify(varnames), class= "dt_missing_fun_aggregate_warning")
      fun.call = quote(length)
    }
  }
  dat_for_default_fill = dat
  run_agg_funs = !is.null(fun.call)
  if (run_agg_funs) {
    fun.call = aggregate_funs(fun.call, lvals, sep, ...)
    maybe_err = function(list.of.columns) {
      if (!all(lengths(list.of.columns) == 1L)) {
        msg <- gettext("Aggregating functions should take a vector as input and return a single value (length=1), but they do not, so the result is undefined. Please fix by modifying your function so that a single value is always returned.")
        if (is.null(fill)) { # TODO change to always stopf #6329
          stop(msg, domain=NA, call. = FALSE)
        } else {
          warning(msg, domain=NA, call. = FALSE)
        }
      }
      list.of.columns
    }
    dat = dat[, maybe_err(eval(fun.call)), by=c(varnames)]
  }
  order_ = function(x) {
    o = forderv(x, retGrp=TRUE, sort=TRUE)
    idx = attr(o, 'starts', exact=TRUE)
    if (!length(o)) o = seq_along(x)
    o[idx] # subsetVector retains attributes, using R's subset for now
  }
  cj_uniq = function(DT) {
    do.call(CJ, lapply(DT, function(x)
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
    maplen = lengths(mapunique)
    idx = do.call(CJ, mapunique)[map, 'I' := .I][["I"]] # TO DO: move this to C and avoid materialising the Cross Join.
    some_fill = anyNA(idx)
    fill.default = if (run_agg_funs && is.null(fill) && some_fill) dat_for_default_fill[, maybe_err(eval(fun.call))]
    if (run_agg_funs && is.null(fill) && some_fill) {
      fill.default = dat_for_default_fill[0L][, maybe_err(eval(fun.call))]
    }
    ans = .Call(Cfcast, lhs, val, maplen[[1L]], maplen[[2L]], idx, fill, fill.default, is.null(fun.call), some_fill)
    allcols = do.call(paste, c(rhs, sep=sep))
    if (length(valnames) > 1L)
      allcols = do.call(paste, if (identical(".", allcols)) list(valnames, sep=sep)
            else c(CJ(valnames, allcols, sorted=FALSE), sep=sep))
      # removed 'setcolorder()' here, #1153
    setattr(ans, 'names', c(lhsnames, allcols))
    setDT(ans); setattr(ans, 'sorted', lhsnames)
  } else internal_error("empty rhsnames") # nocov
  return(ans)
}
