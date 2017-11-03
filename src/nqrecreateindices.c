#include "data.table.h"

// TODO: Add oxygen style comments and cleanup var names.
// See other TODOs inside the function.
SEXP nqRecreateIndices(SEXP xo, SEXP len, SEXP indices, SEXP nArg) {

  R_len_t i=0, j=0, tmp=0, n=INTEGER(nArg)[0];
  SEXP ans, newstarts, newlen;
  ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, (newstarts = allocVector(INTSXP, n)));
  SET_VECTOR_ELT(ans, 1, (newlen = allocVector(INTSXP, n)));

  for (i=0; i<n; i++) INTEGER(newlen)[i] = 0;
  for (i=0; i<length(indices); i++) {
    if (INTEGER(xo)[i] != NA_INTEGER)
      INTEGER(newlen)[INTEGER(indices)[i]-1] += INTEGER(len)[i];
    else INTEGER(newlen)[INTEGER(indices)[i]-1] = INTEGER(len)[i] != 0;
  }
  // fix for #2360, rewriting the for-loop from before
  // TODO: revisit ... can this be simplified? Not much time ATM.
  i = 0; // reset i
  while (j < length(xo)) {
    if (INTEGER(xo)[j] <= 0) { // NA_integer_ = INT_MIN is checked in init.c IIRC
      INTEGER(newstarts)[i] = INTEGER(xo)[j];
      j++; // newlen will be 1 for xo=NA and 0 for xo=0 .. but we need to increment by 1 for both
    } else {
      INTEGER(newstarts)[i] = tmp+1;
      tmp += INTEGER(newlen)[i];
      j += INTEGER(newlen)[i];
    }
    i++;
  }
  UNPROTECT(1);
  return (ans);
}
