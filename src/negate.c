#include "data.table.h"

void negateByRef(SEXP x) {
  if(TYPEOF(x) != LGLSXP) {
    error(_("not logical or integer vector"));  // # nocov
  }
  const int n = length(x);
  int *ansd = LOGICAL(x);
  for(int i=0; i<n; ++i) {
    ansd[i] ^= (ansd[i] != NA_LOGICAL);  // invert true/false but leave NA alone
  }
}


SEXP notchin(SEXP x, SEXP table) {
  // see discussion in PR#4931
  SEXP result = PROTECT(chin(x, table));
  negateByRef(result); // save memory
  UNPROTECT(1);
  return result;
}

