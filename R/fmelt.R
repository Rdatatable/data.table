# Add melt generic, don't import reshape2 as it requires R >= 3.0.0.
melt <- function(data, ..., na.rm = FALSE, value.name = "value") {
  if (is.data.table(data)) 
      UseMethod("melt", data)
  else
      reshape2::melt(data, ..., na.rm=na.rm, value.name=value.name)
}

melt.data.table <- function(data, id.vars, measure.vars, variable.name = "variable", 
           value.name = "value", ..., na.rm = FALSE, variable.factor = TRUE, value.factor = FALSE, 
           verbose = getOption("datatable.verbose")) {
    drop.levels <- FALSE # maybe a future FR
    if (!is.data.table(data)) stop("'data' must be a data.table")
    if (missing(id.vars)) id.vars=NULL
    if (missing(measure.vars)) measure.vars = NULL
    ans <- .Call("Cfmelt", data, id.vars, measure.vars, 
            as.logical(variable.factor), as.logical(value.factor), 
            variable.name, value.name, 
            as.logical(na.rm), as.logical(drop.levels), 
            as.logical(verbose))
    setDT(ans)
    if (any(duplicated(names(ans)))) {
        message("Duplicate column names found in molten data.table. Setting unique names using 'make.names'")   
        setnames(ans, make.unique(names(ans)))
    }
    ans
}


