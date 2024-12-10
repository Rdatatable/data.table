args <- commandArgs(TRUE)
stopifnot(`Usage: Rscript check-openmp-flags.R CFLAGS LIBS` = length(args) == 2)
# We'll need to create Makevars (and object and DLL files too)
setwd(tempdir())
cat(sprintf(
  "Testing if OpenMP works with CFLAGS='%s' and LIBS='%s':\n",
  args[[1]], args[[2]]
))

# It must be a 'Makevars' for constructs like $(SHLIB_OPENMP_CFLAGS) to work:
writeLines(c(
  paste("PKG_CFLAGS =", args[[1]]),
  paste("PKG_LIBS =", args[[2]])
), "Makevars")

# Will succeed to compile even without OpenMP but return 0
writeLines("
#ifdef _OPENMP
 #include <omp.h>
#endif
void test_openmp(int * result) {
 int sum = 0;
#ifdef _OPENMP
 // Have to test an actual OpenMP operation: simpler tests may succeed for a broken configuration, #6642
 #pragma omp parallel for reduction(+:sum) num_threads(2)
 for (int i = 1; i <= 2; ++i) sum += i;
#endif
 *result = sum;
}
", "test.c")

# May fail to compile anyway
stopifnot(tools::Rcmd("SHLIB --preclean test.c") == 0)

dyn.load(paste0("test", .Platform$dynlib.ext))
ans <- .C("test_openmp", ans = integer(1))$ans
desired <- 3L
cat(sprintf("Test result: %d (must be %d; should be 0 for disabled OpenMP)\n", ans, desired))
stopifnot(`Test failed` = identical(ans, desired))
cat("Success\n")
