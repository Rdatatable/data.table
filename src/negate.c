#include "data.table.h"

SEXP negate(SEXP x) {
  if(TYPEOF(x) != LGLSXP) {
    error("not logical or integer vector");
  }
  int n = length(x);
  int *ansd = INTEGER(x);
  for(int i=0;i<n;i++) {
    ansd[i] = LOGICAL(x)[i]?0:1;
  }
  return x;
}

