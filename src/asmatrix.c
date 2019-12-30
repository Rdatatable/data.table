#include "data.table.h"

/* To handle different matrix types (e.g. numeric, character, etc)
 * there is a single dispatch function asmatrix at the bottom of the 
 * file that detects the appropriate R atomic type then calls the
 * corresponding asmatrix_<type> function.
 */

SEXP asmatrix_logical(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  SEXP mat = PROTECT(allocVector(LGLSXP, matlen)); // output matrix
  int *pmat, *pcol; // pointers to casted R objects
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix

  // Iterate through dt and copy into mat 
  pmat = LOGICAL(mat);
  for (R_xlen_t jj = 0; jj < xlength(dt); jj++) {
    if (jj == rncolnum) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = LOGICAL(VECTOR_ELT(dt, jj));
    memcpy(pmat + vecIdx, pcol, sizeof(int)*n);
    vecIdx += n;
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_integer(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  SEXP mat = PROTECT(allocVector(INTSXP, matlen)); // output matrix
  int *pmat, *pcol; // pointers to casted R objects
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Iterate through dt and copy into mat 
  pmat = INTEGER(mat);
  for (R_xlen_t jj = 0; jj < xlength(dt); jj++) {
    if (jj == rncolnum) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = INTEGER(VECTOR_ELT(dt, jj));
    memcpy(pmat + vecIdx, pcol, sizeof(int)*n);
    vecIdx += n;
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_numeric(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  SEXP mat = PROTECT(allocVector(REALSXP, matlen)); // output matrix
  double *pmat, *pcol; // pointers to casted R objects
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Iterate through dt and copy into mat 
  pmat = REAL(mat);
  for (R_xlen_t jj = 0; jj < xlength(dt); jj++) {
    if (jj == rncolnum) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = REAL(VECTOR_ELT(dt, jj));
    memcpy(pmat + vecIdx, pcol, sizeof(double)*n);
    vecIdx += n;
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_complex(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  SEXP mat = PROTECT(allocVector(CPLXSXP, matlen)); // output matrix
  Rcomplex *pmat, *pcol; // pointers to casted R objects
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Iterate through dt and copy into mat 
  pmat = COMPLEX(mat);
  for (R_xlen_t jj = 0; jj < xlength(dt); jj++) {
    if (jj == rncolnum) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = COMPLEX(VECTOR_ELT(dt, jj));
    memcpy(pmat + vecIdx, pcol, sizeof(Rcomplex)*n);
    vecIdx += n;
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_character(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  SEXP mat = PROTECT(allocVector(STRSXP, matlen)); // output matrix
  SEXP pcol; // pointers to casted R objects
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Iterate through dt and copy into mat 
  for (R_xlen_t jj = 0; jj < xlength(dt); jj++) {
    if (jj == rncolnum) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = VECTOR_ELT(dt, jj);
    for (R_xlen_t ii = 0; ii < n; ii++) {
      SET_STRING_ELT(mat, vecIdx, STRING_ELT(pcol, ii));
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_list(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  SEXP mat = PROTECT(allocVector(VECSXP, matlen)); // output matrix
  SEXP pcol; // pointers to casted R objects
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Iterate through dt and copy into mat 
  for (R_xlen_t jj = 0; jj < xlength(dt); jj++) {
    if (jj == rncolnum) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = VECTOR_ELT(dt, jj);
    for (R_xlen_t ii = 0; ii < n; ii++) {
      SET_VECTOR_ELT(mat, vecIdx, VECTOR_ELT(pcol, ii));
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

// Dispatch function for different atomic types
SEXP asmatrix(SEXP dt, SEXP nrow, SEXP ncol, SEXP rownames) {
  // Determine the length of the matrix vector given its dimensions
  R_xlen_t n = asInteger(nrow);
  R_xlen_t p = asInteger(ncol);
  R_xlen_t matlen = n * p;
  
  /* Determine the column number in which the rownames is stored 
   * to skip when filling in the matrix. If there are no rownames, this
   * number will be larger than the number of columns in the data.table.
   */
  R_xlen_t rncolnum = asInteger(rownames) - 1; // -1 to convert from 1-based to 0-based indexing
  
  /* Conversion to a common atomic type is handled in R. We detect the
   * atomic type from the first column; unless that column is the 
   * rownames column we want to drop.
   */
  int firstcol = 0;
  if (rncolnum == 0) firstcol++;
  SEXPTYPE R_atomic_type = TYPEOF(VECTOR_ELT(dt, firstcol));
  
  switch(R_atomic_type) {
    case LGLSXP: 
      return(asmatrix_logical(dt, matlen, n, rncolnum));
    case INTSXP: 
      return(asmatrix_integer(dt, matlen, n, rncolnum));
    case REALSXP: 
      return(asmatrix_numeric(dt, matlen, n, rncolnum));
    case CPLXSXP: 
      return(asmatrix_complex(dt, matlen, n, rncolnum));
    case STRSXP: 
      return(asmatrix_character(dt, matlen, n, rncolnum));
    case VECSXP:
      return(asmatrix_list(dt, matlen, n, rncolnum));
    default:
      error("Unsupported matrix type '%s'", type2char(R_atomic_type));
  }
}

