# Add melt generic, don't import reshape2 as it requires R >= 3.0.0.
melt <- function(data, ..., na.rm = FALSE, value.name = "value") {
  if (is.data.table(data)) 
      UseMethod("melt", data)
  else
      reshape2::melt(data, ..., na.rm=na.rm, value.name=value.name)
}

patterns <- function(...) {
    p = unlist(list(...), use.names=FALSE)
}

melt.data.table <- function(data, id.vars, measure.vars, variable.name = "variable", 
           value.name = "value", ..., na.rm = FALSE, variable.factor = TRUE, value.factor = FALSE, 
           verbose = getOption("datatable.verbose")) {
    if (!is.data.table(data)) stop("'data' must be a data.table")
    if (missing(id.vars)) id.vars=NULL
    if (missing(measure.vars)) measure.vars = NULL
    measure.sub = substitute(measure.vars)
    if (is.call(measure.sub) && measure.sub[[1L]] == "patterns") {
        measure.vars = lapply(eval(measure.sub), grep, names(data))    
    }
    if (is.list(measure.vars)) {
        if (length(value.name) == 1L)  
          value.name = paste(value.name, seq_along(measure.vars), sep="")
    }
    ans <- .Call("Cfmelt", data, id.vars, measure.vars, 
            as.logical(variable.factor), as.logical(value.factor), 
            variable.name, value.name, as.logical(na.rm), 
            as.logical(verbose))
    setDT(ans)
    if (any(duplicated(names(ans)))) {
        cat("Duplicate column names found in molten data.table. Setting unique names using 'make.names'")   
        setnames(ans, make.unique(names(ans)))
    }
    setattr(ans, 'sorted', NULL)
    ans
}

