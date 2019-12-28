#include "data.table.h"

SEXP asmatrix(SEXP dt, SEXP nrow, SEXP ncol) {
  int n = asInteger(nrow);
  int p = asInteger(ncol);
  SEXP mat = PROTECT(allocVector(REALSXP, n*p));
  int vecIndex = 0; // keep track of where we are in mat 
  
  for (int jj = 0; jj < p; jj++) {
    for (int ii = 0; ii < n; ii++) {
      REAL(mat)[vecIndex] = REAL(VECTOR_ELT(dt, jj))[ii];
      vecIndex++;
    }
  }

  UNPROTECT(1);
  return mat;
}