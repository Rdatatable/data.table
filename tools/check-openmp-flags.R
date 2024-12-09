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
void test_openmp(int * numprocs) {
 *numprocs =
#ifdef _OPENMP
  omp_get_max_threads()
#else
  0
#endif
 ;
}
", "test.c")

# May fail to compile anyway
stopifnot(tools::Rcmd("SHLIB --preclean test.c") == 0)

dyn.load(paste0("test", .Platform$dynlib.ext))
success <- .C("test_openmp", ans = integer(1))$ans > 0

cat(sprintf("Result: %s\n", if (success) "yes" else "no"))
q("no", status = !success)
