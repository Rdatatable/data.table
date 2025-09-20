# here we have helpers functions that DT exports, on contrary to utils.R file

# convert char to factor retaining order #4837
fctr = function(x, levels=unique(x), ..., sort=FALSE, rev=FALSE) {
  if (!isTRUEorFALSE(sort))
    stopf("argument 'sort' must be TRUE or FALSE")
  if (!isTRUEorFALSE(rev))
    stopf("argument 'rev' must be TRUE or FALSE")
  if (sort) levels = sort(levels)
  if (rev) levels = frev(levels)
  factor(x, levels=levels, ...)
}
