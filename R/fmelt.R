# reshape2 dependency was originally abandoned because (1) we wanted to be in control
#   of the R version dependency and (2) reshape2::dcast is not generic.
#   reshape2 package is deprecated since December 2017, so we'll deprecate our
#   redirection as well

melt = function(data, ..., na.rm = FALSE, value.name = "value") {
  if (is.data.table(data)) {
    UseMethod("melt", data)
    # if data is not data.table and reshape2 is installed, this won't dispatch to reshape2's method;
    # CRAN package edarf and others fail without the else branch
  # nocov start
  } else {
    data_name = deparse(substitute(data))
    ns = tryCatch(getNamespace("reshape2"), error=function(e)
      stop("The melt generic in data.table has been passed a ",class(data)[1L],", but data.table::melt currently only has a method for data.tables. Please confirm your input is a data.table, with setDT(", data_name, ") or as.data.table(", data_name, "). If you intend to use a method from reshape2, try installing that package first, but do note that reshape2 is superseded and is no longer actively developed."))
    warning("The melt generic in data.table has been passed a ", class(data)[1L], " and will attempt to redirect to the relevant reshape2 method; please note that reshape2 is superseded and is no longer actively developed, and this redirection is now deprecated. To continue using melt methods from reshape2 while both libraries are attached, e.g. melt.list, you can prepend the namespace like reshape2::melt(", data_name, "). In the next version, this warning will become an error.")
    ns$melt(data, ..., na.rm=na.rm, value.name=value.name)
  }
  # nocov end
}

patterns = function(..., cols=character(0L)) {
  # if ... has no names, names(list(...)) will be "";
  #   this assures they'll be NULL instead
  L = list(...)
  p = unlist(L, use.names = any(nzchar(names(L))))
  if (!is.character(p))
    stop("Input patterns must be of type character.")
  matched = lapply(p, grep, cols)
  # replace with lengths when R 3.2.0 dependency arrives
  if (length(idx <- which(sapply(matched, length) == 0L)))
    stop('Pattern', if (length(idx) > 1L) 's', ' not found: [',
         paste(p[idx], collapse = ', '), ']')
  matched
}

measure = function(..., sep="_", pattern, cols, multiple.keyword="value.name") {
  mcall = match.call()
  L = as.list(mcall)[-1]
  formal.names = names(formals())
  formal.i.vec = which(names(L) %in% formal.names)
  fun.list = L[-formal.i.vec]
  user.named = names(fun.list) != ""
  is.symb = sapply(fun.list, is.symbol)
  bad.i = which((!user.named) & (!is.symb))
  if (length(bad.i)) {
    stop("each ... argument to measure must be either a symbol without argument name, or a function with argument name, problems: ", paste(bad.i, collapse=","))
  }
  names(fun.list)[!user.named] = sapply(fun.list[!user.named], paste)
  fun.list[!user.named] = list(NULL)
  # group names error checking.
  group.is.formal = names(fun.list) %in% formal.names
  if (any(group.is.formal)) {
    bad.names = names(fun.list)[group.is.formal]
    stop("group names specified in ... conflict with measure argument names; please fix by changing group names: ", paste(bad.names, collapse=","))
  }
  # evaluate each value in ... and stop if not function.
  for (fun.i in which(user.named)) {
    fun = eval(fun.list[[fun.i]], parent.frame(1L))
    if (!is.function(fun) || length(formals(args(fun)))==0) {
      stop("each ... argument to measure must be a function with at least one argument, problem: ", names(fun.list)[[fun.i]])
    }
    fun.list[[fun.i]] = fun
  }  
  measurev.args = c(
    list(fun.list),
    L[formal.i.vec],
    list(group.desc="... arguments to measure"))
  do.call(measurev, measurev.args)
}

measurev = function(fun.list, sep="_", pattern, cols, multiple.keyword="value.name", group.desc="elements of fun.list"){
  # 1. basic error checking.
  if (!missing(sep) && !missing(pattern)) {
    stop("both sep and pattern arguments used; must use either sep or pattern (not both)")
  }
  if (!(is.character(multiple.keyword) && length(multiple.keyword)==1 && !is.na(multiple.keyword) && nchar(multiple.keyword)>0)) {
    stop("multiple.keyword must be a character string with nchar>0")
  }
  if (!is.character(cols)) {
    stop("cols must be a character vector of column names")
  }
  prob.i <- if (is.null(names(fun.list))) {
    seq_along(fun.list)
  } else {
    which(names(fun.list) == "")
  }
  if (length(prob.i)) {
    stop("in measurev, ", group.desc, " must be named, problems: ", paste(prob.i, collapse=","))
  }
  err.names.unique = function(err.what, name.vec) {
    name.tab = table(name.vec)
    bad.counts = name.tab[1 < name.tab]
    if (length(bad.counts)) {
      stop(err.what, " should be uniquely named, problems: ", paste(names(bad.counts), collapse=","))
    }
  }
  err.args.groups = function(type, N){
    if (N != length(fun.list)) {
      stop("number of ", group.desc, " =", length(fun.list), " must be same as ", type, " =", N)
    }
  }
  err.names.unique(group.desc, names(fun.list))
  # 2. compute initial group data table, used as variable_table attribute.
  group.mat = if (!missing(pattern)) {
    if (!is.character(pattern)) {
      stop("pattern must be character string")
    }
    match.vec = regexpr(pattern, cols, perl=TRUE)
    measure.vec = which(0 < match.vec)
    if (length(measure.vec) == 0L) {
      stop("pattern did not match any cols, so nothing would be melted; fix by changing pattern")
    }
    start = attr(match.vec, "capture.start")[measure.vec, , drop=FALSE]
    if (is.null(start)) {
      stop("pattern must contain at least one capture group (parenthesized sub-pattern)")
    }
    err.args.groups("number of capture groups in pattern", ncol(start))
    end = attr(match.vec, "capture.length")[measure.vec,]+start-1L
    names.mat = matrix(cols[measure.vec], nrow(start), ncol(start))
    substr(names.mat, start, end)
  } else { #pattern not specified, so split using sep.
    if (!is.character(sep)) {
      stop("sep must be character string")
    }
    list.of.vectors = strsplit(cols, sep, fixed=TRUE)
    vector.lengths = sapply(list.of.vectors, length)
    n.groups = max(vector.lengths)
    if (n.groups == 1) {
      stop("each column name results in only one item after splitting using sep, which means that all columns would be melted; to fix please either specify melt on all columns directly without using measure, or use a different sep/pattern specification")
    }
    err.args.groups("max number of items after splitting column names", n.groups)
    measure.vec = which(vector.lengths==n.groups)
    do.call(rbind, list.of.vectors[measure.vec])
  }
  err.names.unique("measured columns", cols[measure.vec])
  uniq.mat = unique(group.mat)
  if (nrow(uniq.mat) < nrow(group.mat)) {
    stop("number of unique column IDs =", nrow(uniq.mat), " is less than number of melted columns =", nrow(group.mat), "; fix by changing pattern/sep")
  }
  colnames(group.mat) = names(fun.list)
  group.dt = data.table(group.mat)
  # 3. apply conversion functions to group data table.
  fun.i.vec = which(!sapply(fun.list, is.null))
  for (group.i in fun.i.vec) {
    group.name = names(fun.list)[[group.i]]
    fun = fun.list[[group.i]]
    if (!is.function(fun) || length(formals(args(fun)))==0) {
      stop("in the measurev fun.list, each non-NULL element must be a function with at least one argument, problem: ", group.name)
    }
    group.val = fun(group.dt[[group.name]])
    if (!(is.atomic(group.val) && length(group.val)==nrow(group.dt))) {
      stop("each conversion function must return an atomic vector with same length as its first argument, problem: ", group.name)
    }
    if (all(is.na(group.val))) {
      stop(group.name, " conversion function returned vector of all NA")
    }
    set(group.dt, j=group.name, value=group.val)
  }
  group.uniq = unique(group.dt)
  if (nrow(group.uniq) < nrow(group.dt)) {
    stop("number of unique groups after applying type conversion functions less than number of groups, change type conversion")
  }
  # 4. compute measure.vars list or vector.
  if (multiple.keyword %in% names(fun.list)) {# multiple output columns.
    if (!is.character(group.dt[[multiple.keyword]])) {
      stop(multiple.keyword, " column class=", class(group.dt[[multiple.keyword]])[[1L]], " after applying conversion function, but must be character")
    }
    is.other = names(group.dt) != multiple.keyword
    if (!any(is.other)) {
      stop(multiple.keyword, " is the only group; fix by creating at least one more group")
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
  if (!is.data.table(data)) stop("'data' must be a data.table")
  if (missing(id.vars)) id.vars=NULL
  if (missing(measure.vars)) measure.vars = NULL
  measure.sub = substitute(measure.vars)
  if (is.call(measure.sub)) {
    eval.result = eval_with_cols(measure.sub, names(data))
    if (!is.null(eval.result)) {
      measure.vars = eval.result
    }
  }
  if (is.list(measure.vars) && length(measure.vars) > 1L) {
    meas.nm = names(measure.vars)
    if (is.null(meas.nm)) {
      # user-provided or default stub
      if (length(value.name) == 1L) {
        value.name = paste0(value.name, seq_along(measure.vars))
      }
    } else {
      if (length(value.name) > 1L) {
        warning("'value.name' provided in both 'measure.vars'",
                "and 'value.name argument'; value provided in",
                "'measure.vars' is given precedence.")
      }
      if (anyNA(meas.nm) || !all(nzchar(meas.nm))) {
        stop("Please provide a name to each element of 'measure.vars'.")
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
