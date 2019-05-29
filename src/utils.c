#include "data.table.h"
#include <Rdefines.h>

void any_na_integer(int *x, uint64_t nx, bool *ans, bool parallel) {
  bool found=false;
  #pragma omp parallel for if (parallel) num_threads(getDTthreads())
  for (uint64_t i=0; i<nx; i++) {
    if (found) continue;
    if (x[i]==NA_INTEGER) found = true;
  }
  ans[0] = found;
}
/* catching first NA index option proposed by Pasha https://github.com/Rdatatable/data.table/pull/3609/files#r288277417
found_index = INTMAX;
#omp for dynamic(1) reduce(min:found_index)
for( i = 1:NBATCHES ) {
  if (found) continue;
  for (row = i*BATCH_SIZE:(i+1)*BATCH_SIZE) {
    if (x[row] is na) {
      found = true;
      found_index = row; 
      break;
    }
  }
}
*/

SEXP any_na_integerR(SEXP x) {
  int protecti=0;
  int *ix = INTEGER(x);
  R_xlen_t nx = xlength(x);
  SEXP ans = PROTECT(allocVector(LGLSXP, 1)); protecti++;
  bool bans;
  any_na_integer(ix, nx, &bans, true);
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
  #pragma omp parallel for num_threads(getDTthreads())
  for (R_len_t i=0; i<nx; i++) {
    any_na_integer(ix[i], inx[i], &bans[i], false);
    ians[i] = (int) bans[i];
  }
  UNPROTECT(protecti);
  return ans;
}
