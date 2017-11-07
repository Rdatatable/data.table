#include "data.table.h"

// TODO: Add oxygen style comments and cleanup var names.
// See other TODOs inside the function.
SEXP nqRecreateIndices(SEXP xo, SEXP len, SEXP indices, SEXP nArg) {

  R_len_t n=INTEGER(nArg)[0];
  SEXP ans, newstarts, newlen;
  ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, (newstarts = allocVector(INTSXP, n)));
  SET_VECTOR_ELT(ans, 1, (newlen = allocVector(INTSXP, n)));

  for (int i=0; i<n; i++) INTEGER(newlen)[i] = 0;
  // simplifying this logic to solve #2275
  for (int i=0; i<length(indices); i++) {
    INTEGER(newlen)[INTEGER(indices)[i]-1] += INTEGER(len)[i];
  }
  // fix for #2360, rewriting the for-loop from before
  // TODO: revisit to see if this be simplified further when I've some time.
  R_len_t j=0, tmp=0;
  for (int i=0; i<n; i++) {
    if (INTEGER(xo)[j] <= 0) { // NA_integer_ = INT_MIN is checked in init.c
      INTEGER(newstarts)[i] = INTEGER(xo)[j];
      j++; // newlen will be 1 for xo=NA and 0 for xo=0 .. but we need to increment by 1 for both
    } else {
      INTEGER(newstarts)[i] = tmp+1;
      tmp += INTEGER(newlen)[i];
      j += INTEGER(newlen)[i];
    }
  }
  UNPROTECT(1);
  return (ans);
}
