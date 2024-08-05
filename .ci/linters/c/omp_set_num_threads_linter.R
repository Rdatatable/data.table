
# Ensure no calls to omp_set_num_threads() [to avoid affecting other packages and base R]
# Only comments referring to it should be in openmp-utils.c
omp_set_num_threads_linter = function(c_file) {
  # strip comments, we only care if the function appears in actual code.
  processed_lines = system2("gcc", c("-fpreprocessed", "-E", c_file), stdout=TRUE, stderr=FALSE)
  idx = grep("omp_set_num_threads", processed_lines, fixed = TRUE)
  if (!length(idx)) return()
  stop(sprintf(
    "In %s, found omp_set_num_threads() usage, which could affect other packages and base R:\n%s",
    c_file, paste0("  ", format(idx), ":", processed_lines[idx], collapse = "\n")
  ))
}
