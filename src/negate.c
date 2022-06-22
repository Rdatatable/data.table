#include "data.table.h"

void negateByRef(SEXP x) {
  if(TYPEOF(x) != LGLSXP) {
    error("not logical or integer vector");
  }
  int n = length(x);
  int *ansd = INTEGER(x);
  for(int i=0;i<n;i++) {
    if (ansd[i] != NA_LOGICAL) ansd[i] ^= 1;
  }
}


SEXP notchin(SEXP x, SEXP table) {
  SEXP result = chin(x, table);
  negateByRef(result);
  return result;
}

