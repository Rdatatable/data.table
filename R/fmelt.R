
# melt is generic in reshape2 which is good (unlike dcast) but we still don't import reshape2 because reshape2's
# dependency on R 3.1 could change in a future release of reshape2. Say it started to depend on R 3.3. Users of data.table
# couldn't then install data.table in R 3.1 even if they only needed melt.data.table. The other reason is that
# reshape2::dcast is not generic (see that method in fcast.R).
melt <- function(data, ..., na.rm = FALSE, value.name = "value") {
  if (is.data.table(data)) {
    UseMethod("melt", data)
    # if data is not data.table and reshape2 is installed, this won't dispatch to reshape2's method;
    # CRAN package edarf and others fail without the else branch
  } else {
    # nocov start
    ns = tryCatch(getNamespace("reshape2"), error=function(e)
         stop("The melt generic in data.table has been passed a ",class(data)[1L]," (not a data.table) but the reshape2 package is not installed to process this type. Please either install reshape2 and try again, or pass a data.table to melt instead."))
    ns$melt(data, ..., na.rm=na.rm, value.name=value.name)
    # nocov end
  }
}

patterns <- function(..., cols=character(0L)) {
  # if ... has no names, names(list(...)) will be "";
  #   this assures they'll be NULL instead
  p = unlist(list(...), use.names = any(nzchar(names(...))))
  if (!is.character(p))
    stop("Input patterns must be of type character.")
  lapply(p, grep, cols)
}

melt.data.table <- function(data, id.vars, measure.vars, variable.name = "variable",
       value.name = "value", ..., na.rm = FALSE, variable.factor = TRUE, value.factor = FALSE,
       verbose = getOption("datatable.verbose")) {
  if (!is.data.table(data)) stop("'data' must be a data.table")
  if (missing(id.vars)) id.vars=NULL
  if (missing(measure.vars)) measure.vars = NULL
  measure.sub = substitute(measure.vars)
  if (is.call(measure.sub) && measure.sub[[1L]] == "patterns") {
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
  ans <- .Call(Cfmelt, data, id.vars, measure.vars,
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
