# here we have helpers functions that DT exports, on contrary to utils.R file

# convert char to factor retaining order #4837
fctr = function(x, levels=unique(x), ..., sort=FALSE, rev=FALSE) {
  if (!isTRUEorFALSE(sort))
    stopf("'%s' must be TRUE or FALSE", "sort")
  if (!isTRUEorFALSE(rev))
    stopf("'%s' must be TRUE or FALSE", "rev")
  if (sort) levels = sort(levels)
  if (rev) levels = frev(levels)
  factor(x, levels=levels, ...)
}

# add a function for validating data.tables that might need setDT #7329
.selfref.ok = function(x) {
  if (!is.data.table(x))
    stopf(".selfref.ok expects data.table class object.")
  selfrefok(x, verbose=FALSE) > 0L
}
