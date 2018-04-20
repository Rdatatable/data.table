#include "data.table.h"
#include <Rdefines.h>

SEXP rollmean(SEXP obj, SEXP k, SEXP fill) {

  R_len_t i=0, j, m, nx, nk, xrows, thisk, protecti=0;
  SEXP x, tmp=R_NilValue, this, ans, thisfill;
  long double window;
  if (!length(obj)) return(obj); // NULL, list()
  if (isVectorAtomic(obj)) {
    x = allocVector(VECSXP, 1);
    SET_VECTOR_ELT(x, 0, obj);
  } else x = obj;
  if (!isNewList(x))
    error("x must be a list, data.frame or data.table");
  if (!isInteger(k))
    error("Internal error: n must be integer");
  if (length(fill) != 1)
    error("fill must be a vector of length 1");

  nx = length(x); nk = length(k);
  i = 0;
  while(i < nk && INTEGER(k)[i] >= 0) i++;
  if (i != nk)
    error("n must be non-negative integer values (>= 0)");
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;

  thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
  
  // loop over columns in 'obj' input
  for (i=0; i<nx; i++) {
    Rprintf("rollmean.c: col %d\n", i);
    this  = VECTOR_ELT(x, i);
    if (!isReal(this)) error("Currently only 'double' type is supported in rollmean");
    xrows = length(this);

    // loop over multiple windows provided as 'k' input
    for (j=0; j<nk; j++) {
      Rprintf("rollmean.c: col %d: win %d\n", i, j);
      thisk = (xrows >= INTEGER(k)[j]) ? INTEGER(k)[j] : xrows;
      tmp = allocVector(REALSXP, xrows);
      SET_VECTOR_ELT(ans, i*nk+j, tmp);
      
      window = 0.;
      for (m=0; m<xrows; m++) {
        Rprintf("rollmean.c: col %d: win %d: row %d\n", i, j, m);
        // add current row to window sum
        Rprintf("rollmean.c: col %d: win %d: row %d: to add to window: %8.3f\n", i, j, m, REAL(this)[m]);
        window += REAL(this)[m];
        // remove row from window sum that is outside of the window already
        if (m - INTEGER(k)[j] >= 0) {
          //REAL(VECTOR_ELT(tmp, m - INTEGER(k)[j] + 1))[0]
          Rprintf("rollmean.c: col %d: win %d: row %d: to subtract from window: %8.3f\n", i, j, m, REAL(this)[m-INTEGER(k)[j]+1]);
          window -= REAL(this)[m-INTEGER(k)[j]+1];
        } else {
          Rprintf("rollmean.c: col %d: win %d: row %d: no subtract from window because window does not have full length yet\n", i, j, m);
        }
        Rprintf("rollmean.c: col %d: win %d: row %d: window sum: %8.3f\n", i, j, m, window);
        if (m<thisk) {
          REAL(tmp)[m] = REAL(thisfill)[0];
        } else {
          REAL(tmp)[m] = REAL(thisfill)[0]; //TODO
          //REAL(tmp)[m] = window / INTEGER(k)[j]
        }
      }
      copyMostAttrib(this, tmp);
    }
  }
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
