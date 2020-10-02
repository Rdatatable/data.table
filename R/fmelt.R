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
      stop("The melt generic in data.table has been passed a ",class(data)[1L],", but data.table::melt currently only has a method for data.tables. Please confirm your input is a data.table, with setDT(", data_name, ") or as.data.table(", data_name, "). If you intend to use a method from reshape2, try installing that package first, but do note that reshape2 is deprecated and you should be migrating your code away from using it."))
    warning("The melt generic in data.table has been passed a ", class(data)[1L], " and will attempt to redirect to the relevant reshape2 method; please note that reshape2 is deprecated, and this redirection is now deprecated as well. To continue using melt methods from reshape2 while both libraries are attached, e.g. melt.list, you can prepend the namespace like reshape2::melt(", data_name, "). In the next version, this warning will become an error.")
    ns$melt(data, ..., na.rm=na.rm, value.name=value.name)
  }
  # nocov end
}

patterns = function(..., cols=character(0L)) {
  # if ... has no names, names(list(...)) will be "";
  #   this assures they'll be NULL instead
  L <- list(...)
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

pattern_match_info = function(pat, fun.list, cols){
  match.vec = regexpr(pat, cols, perl=TRUE)
  capture.names = attr(match.vec, "capture.names")
  if(any("" == capture.names)){
    stop("each capture group needs a name (?<name>pattern)")
  }
  measure.vars = which(0 < match.vec)
  start = attr(match.vec, "capture.start")[measure.vars,]
  end = attr(match.vec, "capture.length")[measure.vars,]+start-1L
  matched.names = cols[measure.vars]
  names.mat = matrix(
    matched.names,
    nrow(start), ncol(start),
    dimnames=list(
      column=matched.names,
      group=capture.names))
  group.mat = substr(names.mat, start, end)
  group.dt = data.table(group.mat)
  for(group.name in names(fun.list)){
    fun = fun.list[[group.name]]
    set(group.dt, j=group.name, value=fun(group.dt[[group.name]]))
  }
  list(measure.vars=measure.vars, group.dt=group.dt)
}

info_to_list = function(measure.vars, group.dt){
  if(! "column" %in% names(group.dt)){
    stop("need capture group named column")
  }
  is.other = names(group.dt)!="column"
  other.values = lapply(group.dt[, ..is.other], unique)
  other.values$stringsAsFactors = FALSE
  other.dt = data.table(do.call(expand.grid, other.values))
  measure.list = structure(list(), variable.name=other.dt)
  column.values = unique(group.dt[["column"]])
  for(column.val in column.values){
    select.dt = data.table(column=column.val, other.dt)
    measure.list[[column.val]] = data.table(
      measure.vars, group.dt
    )[select.dt, measure.vars, on=names(select.dt)]
  }
  measure.list
}

pattern_list = function(pat, fun.list=list(), cols=character(0L)){
  m.list = pattern_match_info(pat, fun.list, cols)
  do.call(info_to_list, m.list)
}

info_to_vec = function(measure.vars, group.dt){
  structure(measure.vars, variable.name=group.dt)
}  

pattern_vec = function(pat, fun.list=list(), cols=character(0L)){
  m.list <- pattern_match_info(pat, fun.list, cols)
  do.call(info_to_vec, m.list)
}

sep_call_info = function(sep, cols){
  parent = sys.parent()
  mcall = match.call(definition=sys.function(parent), call=sys.call(parent))
  L = as.list(mcall)[-1]
  fun.list = L[-which(names(L)%in%names(formals()))]
  no.fun = names(fun.list)==""
  names(fun.list)[no.fun] = sapply(fun.list[no.fun], paste)
  list.of.vectors = strsplit(cols, sep, fixed=TRUE)
  vector.lengths = sapply(list.of.vectors, length)
  measure.vars = which(vector.lengths==max(vector.lengths))
  mat = do.call(rbind, list.of.vectors[measure.vars])
  colnames(mat) = names(fun.list)
  group.dt = data.table(mat)
  for(group.i in which(!no.fun)){
    fun = eval(fun.list[[group.i]])
    set(group.dt, j=group.i, value=fun(group.dt[[group.i]]))
  }
  list(measure.vars=measure.vars, group.dt=group.dt)
}  

sep_list = function(..., sep="_", cols){
  sep.list = sep_call_info(sep, cols)
  do.call(info_to_list, sep.list)
}

sep_vec = function(..., sep="_", cols){
  sep.list = sep_call_info(sep, cols)
  do.call(info_to_vec, sep.list)
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
  if (any(duplicated(names(ans)))) {
    cat("Duplicate column names found in molten data.table. Setting unique names using 'make.names'\n")
    setnames(ans, make.unique(names(ans)))
  }
  setattr(ans, 'sorted', NULL)
  ans
}
