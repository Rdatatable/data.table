#include "data.table.h"

SEXP cbindlist(SEXP x, SEXP copyArg) {
  if (!isNewList(x) || INHERITS(x, char_dataframe))
    error("'x' must be a list");
  if (!IS_TRUE_OR_FALSE(copyArg))
    error("'copy' must be TRUE or FALSE");
  bool copy = (bool)LOGICAL(copyArg)[0];
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  int nx = length(x), nans = 0, nr = -1, *nnx = (int*)R_alloc(nx, sizeof(int));
  bool recycle = false;
  for (int i=0; i<nx; ++i) {
    SEXP thisx = VECTOR_ELT(x, i);
    if (!perhapsDataTable(thisx))
      error("The %d element of 'x' list is not a data.table type", i+1);
    nnx[i] = NCOL(thisx);
    if (!nnx[i])
      continue;
    int thisnr = NROW(thisx);
    if (nr < 0) // first (non-zero length table) iteration
      nr = thisnr;
    else if (nr != thisnr) {
      if (!copy)
        error("For copy=FALSE all non-zero length tables in 'l' has to have equal nrow, element %d has different nrow (%d) than the previous non-zero length element nrow (%d)", i+1, thisnr, nr);
      recycle = true;
    }
    nans += nnx[i];
  }
  if (recycle)
    error("Recycling rows for objects of different nrow is not yet implemented"); // dont we have a routines for that already somewhere?
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
  ans = setDT(ans); // this is not really in-place!
  if (verbose)
    Rprintf("cbindlist: took %.3fs\n", omp_get_wtime()-tic);
  UNPROTECT(2);
  return ans;
}
