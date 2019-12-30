#include "data.table.h"

/* To handle different matrix types (e.g. numeric, character, etc)
 * there is a single dispatch function asmatrix at the bottom of the 
 * file that detects the appropriate R atomic type then calls the
 * corresponding asmatrix_<type> function.
 */

SEXP asmatrix_logical(SEXP dt, SEXP nrow, SEXP ncol, SEXP rownames) {
  int n, p; // row and column numbers
  int rncol; // column number of rownames to skip. If no rownames, rncol will be > ncol  
  int dtcol; // number of columns in dt, either p + 1 if there are rownames, or p if there are no rownames
  SEXP mat; // output matrix
  int *pmat, *pcol; // pointers to casted R objects
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Extract column number of rownames, convert to from R's 1-index to C's 0-index.
  rncol = asInteger(rownames) - 1;
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(LGLSXP, n*p));
  
  // Determine number of columns in DT
  dtcol = length(dt);

  // Iterate through dt and copy into mat 
  pmat = LOGICAL(mat);
  for (int jj = 0; jj < dtcol; jj++) {
    if (jj == rncol) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = LOGICAL(VECTOR_ELT(dt, jj));
    for (int ii = 0; ii < n; ii++) {
      pmat[vecIdx] = pcol[ii];
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_integer(SEXP dt, SEXP nrow, SEXP ncol, SEXP rownames) {
  int n, p; // row and column numbers
  int rncol; // column number of rownames to skip. If no rownames, rncol will be > ncol  
  int dtcol; // number of columns in dt, either p + 1 if there are rownames, or p if there are no rownames
  SEXP mat; // output matrix
  int *pmat, *pcol; // pointers to casted R objects
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Extract column number of rownames, convert to from R's 1-index to C's 0-index.
  rncol = asInteger(rownames) - 1;
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(INTSXP, n*p));
  
  // Determine number of columns in DT
  dtcol = length(dt);

  // Iterate through dt and copy into mat 
  pmat = INTEGER(mat);
  for (int jj = 0; jj < dtcol; jj++) {
    if (jj == rncol) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = INTEGER(VECTOR_ELT(dt, jj));
    for (int ii = 0; ii < n; ii++) {
      pmat[vecIdx] = pcol[ii];
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_numeric(SEXP dt, SEXP nrow, SEXP ncol, SEXP rownames) {
  int n, p; // row and column numbers
  int rncol; // column number of rownames to skip. If no rownames, rncol will be > ncol 
  int dtcol; // number of columns in dt, either p + 1 if there are rownames, or p if there are no rownames
  SEXP mat; // output matrix
  double *pmat, *pcol; // pointers to casted R objects
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Extract column number of rownames, convert to from R's 1-index to C's 0-index.
  rncol = asInteger(rownames) - 1;
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(REALSXP, n*p));
  
  // Determine number of columns in DT
  dtcol = length(dt);
  
  // Iterate through dt and copy into mat 
  pmat = REAL(mat);
  for (int jj = 0; jj < dtcol; jj++) {
    if (jj == rncol) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = REAL(VECTOR_ELT(dt, jj));
    for (int ii = 0; ii < n; ii++) {
      pmat[vecIdx] = pcol[ii];
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_complex(SEXP dt, SEXP nrow, SEXP ncol, SEXP rownames) {
  int n, p; // row and column numbers
  int rncol; // column number of rownames to skip. If no rownames, rncol will be > ncol
  int dtcol; // number of columns in dt, either p + 1 if there are rownames, or p if there are no rownames
  SEXP mat; // output matrix
  Rcomplex *pmat, *pcol; // pointers to casted R objects
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Extract column number of rownames, convert to from R's 1-index to C's 0-index.
  rncol = asInteger(rownames) - 1;
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(CPLXSXP, n*p));
  
  // Determine number of columns in DT
  dtcol = length(dt);
  
  // Iterate through dt and copy into mat 
  pmat = COMPLEX(mat);
  for (int jj = 0; jj < dtcol; jj++) {
    if (jj == rncol) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = COMPLEX(VECTOR_ELT(dt, jj));
    for (int ii = 0; ii < n; ii++) {
      pmat[vecIdx] = pcol[ii];
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_character(SEXP dt, SEXP nrow, SEXP ncol, SEXP rownames) {
  int n, p; // row and column numbers
  int rncol; // column number of rownames to skip. If no rownames, rncol will be > ncol
  int dtcol; // number of columns in dt, either p + 1 if there are rownames, or p if there are no rownames
  SEXP mat; // output matrix
  SEXP pcol; // pointer to column in dt
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Extract column number of rownames, convert to from R's 1-index to C's 0-index.
  rncol = asInteger(rownames) - 1;
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(STRSXP, n*p));
  
  // Determine number of columns in DT
  dtcol = length(dt);
  
  // Iterate through dt and copy into mat 
  for (int jj = 0; jj < dtcol; jj++) {
    if (jj == rncol) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = VECTOR_ELT(dt, jj);
    for (int ii = 0; ii < n; ii++) {
      SET_STRING_ELT(mat, vecIdx, STRING_ELT(pcol, ii));
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_list(SEXP dt, SEXP nrow, SEXP ncol, SEXP rownames) {
  int n, p; // row and column numbers
  int rncol; // column number of rownames to skip. If no rownames, rncol will be > ncol
  int dtcol; // number of columns in dt, either p + 1 if there are rownames, or p if there are no rownames
  SEXP mat; // output matrix
  SEXP pcol; // pointer to column in dt
  int vecIdx = 0; // counter to track place in vector underlying matrix
  
  // Extract column number of rownames, convert to from R's 1-index to C's 0-index.
  rncol = asInteger(rownames) - 1;
  
  // Setup output matrix vector
  n = asInteger(nrow);
  p = asInteger(ncol);
  mat = PROTECT(allocVector(VECSXP, n*p));
  
  // Determine number of columns in DT
  dtcol = length(dt);
  
  // Iterate through dt and copy into mat 
  for (int jj = 0; jj < dtcol; jj++) {
    if (jj == rncol) continue; // skip rownames. TODO: make sure this does slow down because of branch prediction
    pcol = VECTOR_ELT(dt, jj);
    for (int ii = 0; ii < n; ii++) {
      SET_VECTOR_ELT(mat, vecIdx, VECTOR_ELT(pcol, ii));
      vecIdx++;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

// Dispatch function for different atomic types
SEXP asmatrix(SEXP dt, SEXP nrow, SEXP ncol, SEXP rownames) {
  /* Conversion to a common atomic type is handled in R. We detect the
   * atomic type from the first column; unless that column is the 
   * rownames column we want to drop.
   */
  int firstcol = 0;
  int rncol = asInteger(rownames) - 1;
  SEXPTYPE R_atomic_type;
  
  if (rncol == 0) {
    firstcol++;
  }
  R_atomic_type = TYPEOF(VECTOR_ELT(dt, firstcol));
  
  switch(R_atomic_type) {
    case LGLSXP: 
      return(asmatrix_logical(dt, nrow, ncol, rownames));
    case INTSXP: 
      return(asmatrix_integer(dt, nrow, ncol, rownames));
    case REALSXP: 
      return(asmatrix_numeric(dt, nrow, ncol, rownames));
    case CPLXSXP: 
      return(asmatrix_complex(dt, nrow, ncol, rownames));
    case STRSXP: 
      return(asmatrix_character(dt, nrow, ncol, rownames));
    case VECSXP:
      return(asmatrix_list(dt, nrow, ncol, rownames));
    default:
      error("Unsupported matrix type '%s'", type2char(R_atomic_type));
  }
}

