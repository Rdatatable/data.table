#include "data.table.h"

void negateByRef(SEXP x) {
  if(TYPEOF(x) != LGLSXP) {
    error("not logical or integer vector");
  }
  const int n = length(x);
  int *ansd = LOGICAL(x);
  for(int i=0;i<n;i++) {
    ansd[i] ^= (ansd[i] != NA_LOGICAL); 
  }
}


SEXP notchin(SEXP x, SEXP table) {
  SEXP result = PROTECT(chin(x, table));
  negateByRef(result);
  return result;
}

