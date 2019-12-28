#include "data.table.h"

SEXP asmatrix(SEXP dt, SEXP nrow, SEXP ncol) {
  int n, p; // row and column numbers
  SEXP mat; // output matrix
  SEXP col; // vector to hold each column in dt
  double *pmat, *pcol; // pointers to casted R objects
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(REALSXP, n*p));
  
  // Iterate through dt and copy into mat 
  pmat = REAL(mat);
  for (int jj = 0; jj < p; jj++) {
    col = VECTOR_ELT(dt, jj);
    pcol = REAL(col);
    for (int ii = 0; ii < n; ii++) {
      pmat[vecIdx] = pcol[ii];
      vecIdx++;
    }
  }

  UNPROTECT(1);
  return mat;
}