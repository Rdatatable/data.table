#include "data.table.h"

// set(x, NULL, cols, copy(unclass(x)[cols])) ## but keeps the index
SEXP copyCols(SEXP x, SEXP cols) {
  // used in R/mergelist.R
  if (!isDataTable(x))
    internal_error(__func__, "'x' must be a data.table"); // # nocov
  if (!isInteger(cols))
    internal_error(__func__, "'cols' must be integer"); // # nocov
  int nx = length(x), ncols = LENGTH(cols), *colsp = INTEGER(cols);
  if (!nx || !ncols)
    return R_NilValue;
  for (int i=0; i<ncols; ++i) {
    int thiscol = colsp[i]-1;
    SET_VECTOR_ELT(x, thiscol, duplicate(VECTOR_ELT(x, thiscol)));
  }
  return R_NilValue;
}

void mergeIndexAttrib(SEXP to, SEXP from) {
  if (!isInteger(to) || LENGTH(to)!=0)
    internal_error(__func__, "'to' must be integer() already"); // # nocov
  if (isNull(from))
    return;
  SEXP t = ATTRIB(to), f = ATTRIB(from);
  if (isNull(t)) // target has no attributes -> overwrite
    SET_ATTRIB(to, shallow_duplicate(f));
  else {
    for (t = ATTRIB(to); CDR(t) != R_NilValue; t = CDR(t)); // traverse to end of attributes list of to
    SETCDR(t, shallow_duplicate(f));
  }
}

SEXP cbindlist(SEXP x, SEXP copyArg) {
  if (!isNewList(x) || isFrame(x))
    error(_("'%s' must be a list"), "x");
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
      error(_("Element %d of 'l' list is not a data.table."), i+1);
    nnx[i] = n_columns(thisx);
    if (!nnx[i])
      continue;
    int thisnr = n_rows(thisx);
    if (nr < 0) // first (non-zero length table) iteration
      nr = thisnr;
    else if (nr != thisnr) {
      if (!copy)
        error(_("For copy=FALSE all non-empty tables in 'l' have to have the same number of rows, but l[[%d]] has %d rows which differs from the previous non-zero number of rows (%d)."), i+1, thisnr, nr);
      recycle = true;
    }
    nans += nnx[i];
  }
  if (recycle)
    error(_("Recycling rows is not yet implemented.")); // dont we have a routines for that already somewhere?
  SEXP ans = PROTECT(allocVector(VECSXP, nans));
  SEXP index = PROTECT(allocVector(INTSXP, 0));
  SEXP key = R_NilValue;
  setAttrib(ans, sym_index, index);
  SEXP names = PROTECT(allocVector(STRSXP, nans));
  for (int i=0, ians=0; i<nx; ++i) {
    int protecti =0;
    SEXP thisx = VECTOR_ELT(x, i);
    SEXP thisnames = PROTECT(getAttrib(thisx, R_NamesSymbol)); protecti++;
    for (int j=0; j<nnx[i]; ++j, ++ians) {
      SEXP thisxcol;
      if (copy) {
        thisxcol = PROTECT(duplicate(VECTOR_ELT(thisx, j))); protecti++;
      } else {
        thisxcol = VECTOR_ELT(thisx, j);
      }
      SET_VECTOR_ELT(ans, ians, thisxcol);
      SET_STRING_ELT(names, ians, STRING_ELT(thisnames, j));
    }
    mergeIndexAttrib(index, getAttrib(thisx, sym_index));
    if (isNull(key)) // first key is retained
      key = getAttrib(thisx, sym_sorted);
    UNPROTECT(protecti); // thisnames, thisxcol
  }
  if (isNull(ATTRIB(index)))
    setAttrib(ans, sym_index, R_NilValue);
  setAttrib(ans, R_NamesSymbol, names);
  setAttrib(ans, sym_sorted, key);
  if (verbose)
    Rprintf(_("cbindlist: took %.3fs\n"), omp_get_wtime()-tic);
  UNPROTECT(3); // ans, index, names
  return ans;
}
