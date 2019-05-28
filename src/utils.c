#include "data.table.h"
#include <Rdefines.h>

void any_na_integer(int *x, uint64_t nx, bool *ans, uint64_t *first, bool parallel) {
  bool found=false;
  #pragma omp parallel for if (parallel) num_threads(getDTthreads())
  for (uint64_t i=0; i<nx; i++) {
    if (found) continue;
    if (x[i]==NA_INTEGER) {
      found = true;
      first[0] = i;
    }
  }
  ans[0] = found;
}

SEXP any_na_integerR(SEXP x) {
  int protecti=0;
  int *ix = INTEGER(x);
  R_xlen_t nx = xlength(x);
  SEXP ans = PROTECT(allocVector(LGLSXP, 1)); protecti++;
  uint64_t first = NA_INTEGER; // unused here
  bool bans;
  any_na_integer(ix, nx, &bans, &first, true);
  LOGICAL(ans)[0] = (int) bans;
  UNPROTECT(protecti);
  return ans;
}

SEXP list_any_na_integerR(SEXP x) {
  int protecti=0;
  int nx = length(x);
  SEXP ans;
  ans = PROTECT(allocVector(LGLSXP, nx)); protecti++;
  int *ians = LOGICAL(ans);
  bool *bans = malloc(sizeof(bool)*nx);
  if (!bans) error("%s: Unable to allocate memory answer", __func__); // # nocov
  int* ix[nx];
  uint64_t inx[nx];
  for (R_len_t i=0; i<nx; i++) {
    inx[i] = xlength(VECTOR_ELT(x, i));
    ix[i] = INTEGER(VECTOR_ELT(x, i));
  }

  uint64_t first[nx]; // unused here
  #pragma omp parallel for num_threads(getDTthreads())
  for (R_len_t i=0; i<nx; i++) {
    any_na_integer(ix[i], inx[i], &bans[i], &first[i], false);
    ians[i] = ((int)(bans[i])); // map bool to R logical
  }

  UNPROTECT(protecti);
  return ans;
}
