melt.data.table <- function(data, id.vars = NULL, measure.vars = NULL, variable.name = "variable", 
           value.name = "value", ..., na.rm = FALSE, variable.factor = TRUE, value.factor = FALSE, 
           verbose = getOption("datatable.verbose")) {
    drop.levels <- FALSE # maybe a future FR
    if (!inherits(data, "data.table")) stop("'data' must be a data.table")
        ans <- .Call("Cfmelt", data, id.vars, measure.vars, 
                as.logical(variable.factor), as.logical(value.factor), 
                variable.name, value.name, 
                as.logical(na.rm), as.logical(drop.levels), 
                as.logical(verbose));
    setattr(ans, "row.names", .set_row_names(length(ans[[1L]])))
    setattr(ans, "class", c("data.table", "data.frame"))
    alloc.col(ans)
    if (any(duplicated(names(ans)))) {
        message("Duplicate column names found in molten data.table. Setting unique names using 'make.names'")   
        setnames(ans, make.unique(names(ans)))
    }
    ans
}

