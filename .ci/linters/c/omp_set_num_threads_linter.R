# Ensure no calls to omp_set_num_threads() [to avoid affecting other packages and base R]
# Only comments referring to it should be in openmp-utils.c
omp_set_num_threads_linter = function(c_obj) {
  # strip comments, we only care if the function appears in actual code.
  idx = grep("omp_set_num_threads", c_obj$preprocessed, fixed = TRUE)
  if (!length(idx)) return()
  stop(sprintf(
    "In %s, found omp_set_num_threads() usage, which could affect other packages and base R:\n%s",
    c_obj$path, paste0("  ", format(idx), ":", c_obj$preprocessed[idx], collapse = "\n")
  ))
}
