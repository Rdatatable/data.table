#include "data.table.h"

/* To handle different matrix types (e.g. numeric, character, etc)
 * there is a single dispatch function asmatrix at the bottom of the 
 * file that detects the appropriate R atomic type then calls the
 * corresponding asmatrix_<type> function.
 */

SEXP asmatrix_logical(SEXP dt) {
  // Determine the number of rows and columns in the data.table
  R_len_t p = length(dt);
  R_len_t n = length(VECTOR_ELT(dt, 0));
  
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocMatrix(LGLSXP, n, p));
  int *pmat; 
  pmat = LOGICAL(mat);
  
  /* Create an array of pointers for the individual columns in DT 
   * so that we avoid creating these and calling the helper functions
   * in the parallel OMP threads
   */
  int **pcol;
  pcol = (int **) R_alloc(p, sizeof(int *));
  for (R_len_t jj = 0; jj < p; jj++) {
    pcol[jj] = LOGICAL(VECTOR_ELT(dt, jj));
  }

  // Multithreaded for loop to copy each column into the matrix memory location
  #pragma omp parallel for num_threads(getDTthreads())
  for (R_len_t jj = 0; jj < p; jj++) {
    memcpy(pmat + (jj * n), pcol[jj], sizeof(int)*n);
  }
  
  return mat;
}

SEXP asmatrix_raw(SEXP dt) {
  // Determine the number of rows and columns in the data.table
  R_len_t p = length(dt);
  R_len_t n = length(VECTOR_ELT(dt, 0));
  
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocMatrix(RAWSXP, n, p));
  Rbyte *pmat; 
  pmat = RAW(mat);
  
  // Create an array of pointers for the individual columns in DT 
  Rbyte **pcol;
  pcol = (Rbyte **) R_alloc(p, sizeof(Rbyte *));
  for (R_len_t jj = 0; jj < p; jj++) {
    pcol[jj] = RAW(VECTOR_ELT(dt, jj));
  }
  
  // Multithreaded for loop to copy each column into the matrix memory location
  #pragma omp parallel for num_threads(getDTthreads())
  for (R_len_t jj = 0; jj < p; jj++) {
    memcpy(pmat + (jj * n), pcol[jj], sizeof(Rbyte)*n);
  }

  return mat;
}

SEXP asmatrix_integer(SEXP dt) {
  // Determine the number of rows and columns in the data.table
  R_len_t p = length(dt);
  R_len_t n = length(VECTOR_ELT(dt, 0));
  
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocMatrix(INTSXP, n, p));
  int *pmat; 
  pmat = INTEGER(mat);
  
  // Create an array of pointers for the individual columns in DT 
  int **pcol;
  pcol = (int **) R_alloc(p, sizeof(int *));
  for (R_len_t jj = 0; jj < p; jj++) {
    pcol[jj] = INTEGER(VECTOR_ELT(dt, jj));
  }
  
  // Multithreaded for loop to copy each column into the matrix memory location
  #pragma omp parallel for num_threads(getDTthreads())
  for (R_len_t jj = 0; jj < p; jj++) {
    memcpy(pmat + (jj * n), pcol[jj], sizeof(int)*n);
  }
  
  return mat;
}

SEXP asmatrix_numeric(SEXP dt) {
  // Determine the number of rows and columns in the data.table
  R_len_t p = length(dt);
  R_len_t n = length(VECTOR_ELT(dt, 0));
  
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocMatrix(REALSXP, n, p));
  double *pmat; 
  pmat = REAL(mat);
  
  // Create an array of pointers for the individual columns in DT 
  double **pcol;
  pcol = (double **) R_alloc(p, sizeof(double *));
  for (R_len_t jj = 0; jj < p; jj++) {
    pcol[jj] = REAL(VECTOR_ELT(dt, jj));
  }
  
  // Multithreaded for loop to copy each column into the matrix memory location
  #pragma omp parallel for num_threads(getDTthreads())
  for (R_len_t jj = 0; jj < p; jj++) {
    memcpy(pmat + (jj * n), pcol[jj], sizeof(double)*n);
  }
  
  return mat;
}

SEXP asmatrix_complex(SEXP dt) {
  // Determine the number of rows and columns in the data.table
  R_len_t p = length(dt);
  R_len_t n = length(VECTOR_ELT(dt, 0));
  
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocMatrix(CPLXSXP, n, p));
  Rcomplex *pmat; 
  pmat = COMPLEX(mat);
  
  // Create an array of pointers for the individual columns in DT 
  Rcomplex **pcol;
  pcol = (Rcomplex **) R_alloc(p, sizeof(Rcomplex *));
  for (R_len_t jj = 0; jj < p; jj++) {
    pcol[jj] = COMPLEX(VECTOR_ELT(dt, jj));
  }
  
  // Multithreaded for loop to copy each column into the matrix memory location
  #pragma omp parallel for num_threads(getDTthreads())
  for (R_len_t jj = 0; jj < p; jj++) {
    memcpy(pmat + (jj * n), pcol[jj], sizeof(Rcomplex)*n);
  }

  return mat;
}

SEXP asmatrix_character(SEXP dt) {
  // Determine the number of rows and columns in the data.table
  R_len_t p = length(dt);
  R_len_t n = length(VECTOR_ELT(dt, 0));
  
  // Create output matrix, allocate memory, and create a pointer to it
  SEXP mat = PROTECT(allocMatrix(STRSXP, n, p)); // output matrix
  SEXP pcol; // pointers to casted R objects

  // Iterate through dt and copy into mat 
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  for (R_len_t jj = 0; jj < p; jj++) {
    pcol = VECTOR_ELT(dt, jj);
    for (R_len_t ii = 0; ii < n; ii++) {
      SET_STRING_ELT(mat, vecIdx, STRING_ELT(pcol, ii));
      vecIdx++;
    }
  }
  
  return mat;
}

SEXP asmatrix_list(SEXP dt) {
  // Determine the number of rows and columns in the data.table
  R_len_t p = length(dt);
  R_len_t n = length(VECTOR_ELT(dt, 0));
  
  // Create output matrix, allocate memory, and create a pointer to it
  SEXP mat = PROTECT(allocMatrix(VECSXP, n, p)); // output matrix
  SEXP pcol; // pointers to casted R objects
  
  // Iterate through dt and copy into mat 
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  for (R_len_t jj = 0; jj < p; jj++) {
    pcol = VECTOR_ELT(dt, jj);
    for (R_len_t ii = 0; ii < n; ii++) {
      SET_VECTOR_ELT(mat, vecIdx, VECTOR_ELT(pcol, ii));
      vecIdx++;
    }
  }
  
  return mat;
}

// Dispatch function for different atomic types
SEXP asmatrix(SEXP dt) {
  // Timing
  const bool verbose = GetVerbose(); 
  double tic = 0; 
  if (verbose) 
    tic = omp_get_wtime(); // # nocov
  
  /* Conversion to a common atomic type is handled in R. We detect the
   * atomic type from the first column. */
  SEXPTYPE R_atomic_type = TYPEOF(VECTOR_ELT(dt, 0));
  
  // Copy values from dt into the matrix mat using appropriately typed function
  SEXP mat;
  switch(R_atomic_type) {
    case RAWSXP:
      mat = asmatrix_raw(dt);
      break;
    case LGLSXP:
      mat = asmatrix_logical(dt);
      break;
    case INTSXP: 
      mat = asmatrix_integer(dt);
      break;
    case REALSXP: 
      mat = asmatrix_numeric(dt);
      break;
    case CPLXSXP: 
      mat = asmatrix_complex(dt);
      break;
    case STRSXP: 
      mat = asmatrix_character(dt);
      break;
    case VECSXP:
      mat = asmatrix_list(dt);
      break;
    default:
      error("Internal error: unsupported matrix type '%s'", type2char(R_atomic_type)); // # nocov
  }
  
  if (verbose) 
    Rprintf("%s: took %.3fs\n", __func__, omp_get_wtime()-tic); // # nocov
  
  UNPROTECT(1);
  return(mat);
}

