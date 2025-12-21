# reshape2 dependency was originally abandoned because (1) we wanted to be in control
#   of the R version dependency and (2) reshape2::dcast is not generic.
#   reshape2 package is deprecated since December 2017, so we've deprecated our
#   redirection as well

melt = function(data, ..., na.rm = FALSE, value.name = "value") {
  UseMethod("melt", data)
}

# TODO(>=1.19.0): Remove this, just let dispatch to 'default' method fail.
melt.default = function(data, ..., na.rm = FALSE, value.name = "value") {
  stopf("The %1$s generic in data.table has been passed a %2$s and will attempt to redirect to the relevant reshape2 method; please note that reshape2 is superseded and is no longer actively developed, and this redirection is now deprecated. To continue using melt methods from reshape2 while both packages are attached, e.g. melt.list, you can prepend the namespace, i.e. reshape2::%1$s(%3$s). In the next version, this warning will become an error.", "melt", class1(data), deparse(substitute(data))) # nocov
}

patterns = function(..., cols=character(0L), ignore.case=FALSE, perl=FALSE, fixed=FALSE, useBytes=FALSE) {
  # if ... has no names, names(list(...)) will be "";
  #   this assures they'll be NULL instead
  if (!is.character(cols) || anyNA(cols)) {
    stopf("cols must be a character vector of column names")
  }
  L = list(...)
  p = unlist(L, use.names = any(nzchar(names(L))))
  if (!is.character(p))
    stopf("Input patterns must be of type character.")
  matched = lapply(p, grep, cols, ignore.case=ignore.case, perl=perl, fixed=fixed, useBytes=useBytes, value=TRUE)
  if (length(idx <- which(lengths(matched) == 0L)))
    stopf(ngettext(length(idx), 'Pattern not found: [%s]', 'Patterns not found: [%s]'), brackify(p[idx]), domain=NA)
  if (length(matched) == 1L) return(matched[[1L]])
  matched
}

measure = function(..., sep="_", pattern, cols, multiple.keyword="value.name") {
  mcall = match.call()
  L = as.list(mcall)[-1L]
  formal.names = names(formals())
  formal.i.vec = which(names(L) %in% formal.names)
  fun.list = L[-formal.i.vec]
  user.named = nzchar(names(fun.list))
  is.symb = sapply(fun.list, is.symbol)
  bad.i = which((!user.named) & (!is.symb))
  if (length(bad.i)) {
    stopf("each ... argument to measure must be either a symbol without argument name, or a function with argument name, problems: %s", brackify(bad.i))
  }
  names(fun.list)[!user.named] = sapply(fun.list[!user.named], paste)
  fun.list[!user.named] = list(NULL)
  # group names error checking.
  group.is.formal = names(fun.list) %in% formal.names
  if (any(group.is.formal)) {
    bad.names = names(fun.list)[group.is.formal]
    stopf("group names specified in ... conflict with measure argument names; please fix by changing group names: %s", brackify(bad.names))
  }
  # evaluate each value in ... and stop if not function.
  for (fun.i in which(user.named)) {
    fun = eval(fun.list[[fun.i]], parent.frame(1L))
    if (!is.function(fun) || length(formals(args(fun)))==0L) {
      stopf("each ... argument to measure must be a function with at least one argument, problem: %s", names(fun.list)[[fun.i]])
    }
    fun.list[[fun.i]] = fun
  }
  measurev.args = c(list(fun.list), L[formal.i.vec])
  do.call(measurev, measurev.args)
}

measurev = function(fun.list, sep="_", pattern, cols, multiple.keyword="value.name"){
  # 1. basic error checking.
  if (!missing(sep) && !missing(pattern)) {
    stopf("both sep and pattern arguments used; must use either sep or pattern (not both)")
  }
  if (!(is.character(multiple.keyword) && length(multiple.keyword)==1L && !is.na(multiple.keyword) && nzchar(multiple.keyword))) {
    stopf("multiple.keyword must be a character string with nchar>0")
  }
  if (!is.character(cols)) {
    stopf("cols must be a character vector of column names")
  }
  prob.i = if (is.null(names(fun.list))) {
    seq_along(fun.list)
  } else {
    which(!nzchar(names(fun.list)))
  }
  if (length(prob.i)) {
    stopf("in measurev, elements of fun.list must be named, problems: %s", brackify(prob.i))
  }
  if (length(dup.funs <- duplicated_values(names(fun.list)))) {
    stopf("elements of fun.list should be uniquely named, problems: %s", brackify(dup.funs))
  }
  # 2. compute initial group data table, used as variable_table attribute.
  group.mat = if (!missing(pattern)) {
    if (!is.character(pattern)) {
      stopf("pattern must be character string")
    }
    match.vec = regexpr(pattern, cols, perl=TRUE)
    measure.vec.i = which(match.vec > 0L)
    if (length(measure.vec.i) == 0L) {
      stopf("pattern did not match any cols, so nothing would be melted; fix by changing pattern")
    }
    start = attr(match.vec, "capture.start")[measure.vec.i, , drop=FALSE]
    if (is.null(start)) {
      stopf("pattern must contain at least one capture group (parenthesized sub-pattern)")
    }
    if (ncol(start) != length(fun.list)) {
      stopf("number of elements of fun.list (%d) must be the same as the number of capture groups in pattern (%d)", length(fun.list), ncol(start))
    }
    end = attr(match.vec, "capture.length")[measure.vec.i,]+start-1L
    measure.vec = cols[measure.vec.i]
    names.mat = matrix(measure.vec, nrow(start), ncol(start))
    substr(names.mat, start, end)
  } else { #pattern not specified, so split using sep.
    if (!is.character(sep)) {
      stopf("sep must be character string")
    }
    list.of.vectors = strsplit(cols, sep, fixed=TRUE)
    vector.lengths = lengths(list.of.vectors)
    n.groups = max(vector.lengths)
    if (n.groups == 1L) {
      stopf("each column name results in only one item after splitting using sep, which means that all columns would be melted; to fix please either specify melt on all columns directly without using measure, or use a different sep/pattern specification")
    }
    if (n.groups != length(fun.list)) {
      stopf("number of elements of fun.list (%d) must be the same as the max number of items after splitting column names (%d)", length(fun.list), n.groups)
    }
    measure.vec.i = which(vector.lengths==n.groups)
    measure.vec = cols[measure.vec.i]
    do.call(rbind, list.of.vectors[measure.vec.i])
  }
  if (length(dup.measures <- duplicated_values(measure.vec))) {
    stopf("measured columns should be uniquely named, problems: %s", brackify(dup.measures))
  }
  uniq.mat = unique(group.mat)
  if (nrow(uniq.mat) < nrow(group.mat)) {
    stopf("number of unique column IDs =%d is less than number of melted columns =%d; fix by changing pattern/sep", nrow(uniq.mat), nrow(group.mat))
  }
  colnames(group.mat) = names(fun.list)
  group.dt = data.table(group.mat)
  # 3. apply conversion functions to group data table.
  fun.i.vec = which(!sapply(fun.list, is.null))
  for (group.i in fun.i.vec) {
    group.name = names(fun.list)[[group.i]]
    fun = fun.list[[group.i]]
    if (!is.function(fun) || length(formals(args(fun))) == 0L) {
      stopf("in the measurev fun.list, each non-NULL element must be a function with at least one argument, problem: %s", group.name)
    }
    group.val = fun(group.dt[[group.name]])
    if (!(is.atomic(group.val) && length(group.val)==nrow(group.dt))) {
      stopf("each conversion function must return an atomic vector with same length as its first argument, problem: %s", group.name)
    }
    if (all(is.na(group.val))) {
      stopf("%s conversion function returned vector of all NA", group.name)
    }
    set(group.dt, j=group.name, value=group.val)
  }
  group.uniq = unique(group.dt)
  if (nrow(group.uniq) < nrow(group.dt)) {
    stopf("number of unique groups after applying type conversion functions less than number of groups, change type conversion")
  }
  # 4. compute measure.vars list or vector.
  if (multiple.keyword %in% names(fun.list)) {# multiple output columns.
    if (!is.character(group.dt[[multiple.keyword]])) {
      stopf("%s column class=%s after applying conversion function, but must be character", multiple.keyword, class1(group.dt[[multiple.keyword]]))
    }
    is.other = names(group.dt) != multiple.keyword
    if (!any(is.other)) {
      stopf("%s is the only group; fix by creating at least one more group", multiple.keyword)
    }
    other.values = lapply(group.dt[, is.other, with=FALSE], unique)
    other.values$stringsAsFactors = FALSE
    other.dt = data.table(do.call(expand.grid, other.values))
    measure.list = structure(list(), variable_table=other.dt)
    column.values = unique(group.dt[[multiple.keyword]])
    for (column.val in column.values) {
      select.dt = data.table(other.dt)
      set(select.dt, j=multiple.keyword, value=column.val)
      measure.list[[column.val]] = data.table(
        measure.vec, group.dt
      )[select.dt, measure.vec, on=names(select.dt)]
    }
    measure.list
  } else {# single output column.
    structure(measure.vec, variable_table=group.dt)
  }
}

melt.data.table = function(data, id.vars, measure.vars, variable.name = "variable",
       value.name = "value", ..., na.rm = FALSE, variable.factor = TRUE, value.factor = FALSE,
       verbose = getOption("datatable.verbose")) {
  if (!is.data.table(data)) stopf("'data' must be a data.table")
  for(type.vars in c("id.vars","measure.vars")){
    sub.lang <- substitute({
      if (missing(VAR)) VAR=NULL
      substitute(VAR)
    }, list(VAR=as.symbol(type.vars)))
    sub.result = eval(sub.lang)
    if (is.call(sub.result)) {
      eval.result = eval_with_cols(sub.result, names(data))
      if (!is.null(eval.result)) {
        assign(type.vars, eval.result)
      }
    }
  }
  if (is.list(measure.vars)) {
    meas.nm = names(measure.vars)
    if (is.null(meas.nm)) {
      # user-provided or default stub
      if (length(value.name) == 1L && length(measure.vars) > 1L) {
        value.name = paste0(value.name, seq_along(measure.vars))
      }
    } else {
      if (length(value.name) > 1L) {
        warningf("'value.name' provided in both 'measure.vars' and 'value.name argument'; value provided in 'measure.vars' is given precedence.")
      }
      if (anyNA(meas.nm) || !all(nzchar(meas.nm))) {
        stopf("Please provide a name to each element of 'measure.vars'.")
      }
      value.name = meas.nm
    }
  }
  ans = .Call(Cfmelt, data, id.vars, measure.vars,
      as.logical(variable.factor), as.logical(value.factor),
      variable.name, value.name, as.logical(na.rm),
      as.logical(verbose))
  setDT(ans)
  if (anyDuplicated(names(ans))) {
    catf("Duplicate column names found in molten data.table. Setting unique names using 'make.names'\n")
    setnames(ans, make.unique(names(ans)))
  }
  setattr(ans, 'sorted', NULL)
  ans
}
