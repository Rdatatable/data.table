#include "data.table.h"

SEXP cbindlist(SEXP x, SEXP copyArg) {
  if (!isNewList(x))
    error("'x' must be a list");
  if (!IS_TRUE_OR_FALSE(copyArg))
    error("'copy' must be TRUE or FALSE");
  bool copy = (bool)LOGICAL(copyArg)[0];
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  int nx = length(x);
  int *nnx = (int*)R_alloc(nx, sizeof(int));
  int nans = 0;
  R_len_t nr;
  for (int i=0; i<nx; ++i) {
    SEXP thisx = VECTOR_ELT(x, i);
    if (!INHERITS(thisx, char_datatable))
      error("The %d element of 'x' list is not a data.table", i+1);
    R_len_t thisnr = length(VECTOR_ELT(thisx, 0));
    if (!i)
      nr = thisnr;
    else if (nr != thisnr)
      error("The %d element of 'x' has nrow different than previous ones, ", i+1);
    nnx[i] = length(thisx);
    nans += nnx[i];
  }
  SEXP ans = PROTECT(allocVector(VECSXP, nans));
  SEXP names = PROTECT(allocVector(STRSXP, nans));
  for (int i=0, ians=0; i<nx; ++i) {
    SEXP thisx = VECTOR_ELT(x, i);
    SEXP thisnames = getAttrib(thisx, R_NamesSymbol);
    for (int j=0; j<nnx[i]; ++j, ++ians) {
      SEXP thisxcol = VECTOR_ELT(thisx, j);
      SET_VECTOR_ELT(ans, ians, copy ? duplicate(thisxcol) : thisxcol);
      SET_STRING_ELT(names, ians, STRING_ELT(thisnames, j));
    }
  }
  setAttrib(ans, R_NamesSymbol, names);
  if (verbose)
    Rprintf("cbindlist: took %.3fs\n", omp_get_wtime()-tic);
  UNPROTECT(2);
  return ans;
}
