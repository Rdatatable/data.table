# Ensure no calls to omp_set_nested() as
#   i) it's hard to fully honor OMP_THREAD_LIMIT as required by CRAN, and
#   ii) a simpler non-nested approach is always preferable if possible, as has been the case so far
omp_set_nested_linter = function(c_obj) {
  idx = grep("omp_set_nested", c_obj$lines, fixed=TRUE)
  if (!length(idx)) return()
  stop(sprintf(
    "In %s, found omp_set_nested() usage, please reconsider:\n%s",
    c_obj$path, paste0("  ", format(idx), ":", c_obj$lines[idx], collapse = "\n")
  ))
}
