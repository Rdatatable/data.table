#include "data.table.h"
#include <string.h>

// TODO: Add oxygen style comments and cleanup var names.
// See other TODOs inside the function.
SEXP nqRecreateIndices(SEXP xo, SEXP len, SEXP indices, SEXP nArg, SEXP nomatch)
{
  const R_len_t n = INTEGER_RO(nArg)[0], xn = length(xo);
  SEXP ans, newstarts, newlen;
  ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, (newstarts = allocVector(INTSXP, n)));
  SET_VECTOR_ELT(ans, 1, (newlen = allocVector(INTSXP, n)));

  int *inewlen = INTEGER(newlen);
  const int *iindices = INTEGER_RO(indices);
  const int *ilen = INTEGER_RO(len);
  const int *ixo = INTEGER_RO(xo);
  const int inomatch = isNull(nomatch) ? 0 : INTEGER_RO(nomatch)[0];
  int *inewstarts = INTEGER(newstarts);

  memset(inewlen, 0, n * sizeof(int));

  // simplifying logic ... also fixes #2275
  for (int i = 0; i < length(indices); i++) {
    inewlen[iindices[i] - 1] += ilen[i];
  }
  // fix for #2360, rewriting the for-loop from before
  // TODO: revisit to see if this be simplified further when I've some time.
  R_len_t j = 0, tmp = 0;
  for (int i = 0; i < n; i++) {
    if (j >= xn || ixo[j] <= 0) {
      // NA_integer_ = INT_MIN is checked in init.c
      // j >= xn needed for special nomatch=NULL case, see issue#4388 (due to xo[irows] from R removing '0' value in xo)
      inewstarts[i] = inomatch;
      j++; // newlen will be 1 for xo=NA and 0 for xo=0 .. but we need to increment by 1 for both
    } else {
      inewstarts[i] = tmp + 1;
      tmp += inewlen[i];
      j += inewlen[i];
    }
  }
  UNPROTECT(1);
  return ans;
}
