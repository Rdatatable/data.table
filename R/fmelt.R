# reshape2 dependency was originally abandoned because (1) we wanted to be in control
#   of the R version dependency and (2) reshape2::dcast is not generic.
#   reshape2 package is deprecated since December 2017, so we'll deprecate our
#   redirection as well

melt <- function(data, ..., na.rm = FALSE, value.name = "value") {
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
  p = unlist(list(...), use.names = any(nzchar(names(...))))
  if (!is.character(p))
    stop("Input patterns must be of type character.")
  lapply(p, grep, cols)
}

melt.data.table = function(data, id.vars, measure.vars, variable.name = "variable",
       value.name = "value", ..., na.rm = FALSE, variable.factor = TRUE, value.factor = FALSE,
       verbose = getOption("datatable.verbose")) {
  if (!is.data.table(data)) stop("'data' must be a data.table")
  if (missing(id.vars)) id.vars=NULL
  if (missing(measure.vars)) measure.vars = NULL
  measure.sub = substitute(measure.vars)
  if (measure.sub %iscall% "patterns") {
    measure.vars = do_patterns(measure.sub, names(data))
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
