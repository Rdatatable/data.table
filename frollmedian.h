#include <stdbool.h>
#include <stdint.h>
#include <omp.h> // not used yet
#include <inttypes.h> // print uint64_t
#include <R.h>
#include <Rdefines.h>

void frollmedianFast(double *x, uint64_t nx, /*to be ans_t*/double *ans, int k, double fill, bool narm, int hasnf, bool verbose);
SEXP frollmedianR(SEXP x, SEXP k);
