#include "data.table.h"

/* To handle different matrix types (e.g. numeric, character, etc)
 * there is a single dispatch function asmatrix at the bottom of the 
 * file that detects the appropriate R atomic type then calls the
 * corresponding asmatrix_<type> function.
 */

SEXP asmatrix_logical(SEXP dt, SEXP nrow, SEXP ncol) {
  int n, p; // row and column numbers
  SEXP mat; // output matrix
  int *pmat, *pcol; // pointers to casted R objects
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(LGLSXP, n*p));
  
  // Iterate through dt and copy into mat 
  pmat = LOGICAL(mat);
  for (int jj = 0; jj < p; jj++) {
    pcol = LOGICAL(VECTOR_ELT(dt, jj));
    for (int ii = 0; ii < n; ii++) {
      pmat[vecIdx] = pcol[ii];
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_integer(SEXP dt, SEXP nrow, SEXP ncol) {
  int n, p; // row and column numbers
  SEXP mat; // output matrix
  int *pmat, *pcol; // pointers to casted R objects
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(INTSXP, n*p));
  
  // Iterate through dt and copy into mat 
  pmat = INTEGER(mat);
  for (int jj = 0; jj < p; jj++) {
    pcol = INTEGER(VECTOR_ELT(dt, jj));
    for (int ii = 0; ii < n; ii++) {
      pmat[vecIdx] = pcol[ii];
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_numeric(SEXP dt, SEXP nrow, SEXP ncol) {
  int n, p; // row and column numbers
  SEXP mat; // output matrix
  double *pmat, *pcol; // pointers to casted R objects
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(REALSXP, n*p));
  
  // Iterate through dt and copy into mat 
  pmat = REAL(mat);
  for (int jj = 0; jj < p; jj++) {
    pcol = REAL(VECTOR_ELT(dt, jj));
    for (int ii = 0; ii < n; ii++) {
      pmat[vecIdx] = pcol[ii];
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_complex(SEXP dt, SEXP nrow, SEXP ncol) {
  int n, p; // row and column numbers
  SEXP mat; // output matrix
  Rcomplex *pmat, *pcol; // pointers to casted R objects
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(CPLXSXP, n*p));
  
  // Iterate through dt and copy into mat 
  pmat = COMPLEX(mat);
  for (int jj = 0; jj < p; jj++) {
    pcol = COMPLEX(VECTOR_ELT(dt, jj));
    for (int ii = 0; ii < n; ii++) {
      pmat[vecIdx] = pcol[ii];
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

// Dispatch function for different atomic types
SEXP asmatrix(SEXP dt, SEXP nrow, SEXP ncol) {
  // Conversion to common type handled in R
  SEXPTYPE R_atomic_type = TYPEOF(VECTOR_ELT(dt, 1)); 
  
  switch(R_atomic_type) {
    case LGLSXP: 
      return(asmatrix_logical(dt, nrow, ncol));
    case INTSXP: 
      return(asmatrix_integer(dt, nrow, ncol));
    case REALSXP: 
      return(asmatrix_numeric(dt, nrow, ncol));
    case CPLXSXP: 
      return(asmatrix_complex(dt, nrow, ncol));
    default:
      error("Unsupported matrix type '%s'", type2char(R_atomic_type));
  }
}

