#include "data.table.h"

/* To handle different matrix types (e.g. numeric, character, etc)
 * there is a single dispatch function asmatrix at the bottom of the 
 * file that detects the appropriate R atomic type then calls the
 * corresponding asmatrix_<type> function.
 */

SEXP asmatrix_integer(SEXP dt, R_xlen_t matlen, R_xlen_t n) {
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocVector(LGLSXP, matlen));
  int *pmat; 
  pmat = LOGICAL(mat);
  
  /* Create an array of pointers for the individual columns in DT 
   * so that we avoid creating these and calling the helper functions
   * in the parallel OMP threads
   */
  R_len_t dtncol = length(dt);
  int **pcol;
  pcol = (int **) R_alloc(dtncol, sizeof(int *));
  for (R_len_t jj = 0; jj < dtncol; jj++) {
    pcol[jj] = LOGICAL(VECTOR_ELT(dt, jj));
  }

  /* Using OMP is slightly less straightforward here because the number 
   * of columns in the matrix may differ from the number of columns in
   * the data.table if there is a rownames column we're skipping over.
   * This means we have a lot of manual set up to make sure all our 
   * array indices are pointing to the right place in memory across 
   * threads. 
   */
  
  R_len_t chunksize = dtncol/getDTthreads() + 1; 
  #pragma omp parallel num_threads(getDTthreads())
  { 
    // Determine which columns in DT we're iterating through on this thread
    int threadnum = omp_get_thread_num();
    R_len_t startcol = threadnum * chunksize;
    R_len_t endcol = startcol + chunksize - 1;
    if (endcol >= dtncol)
      endcol = dtncol - 1;
    
    // Determine where to fill in the matrix vector
    R_xlen_t vecIdx = startcol * n;
    
    // Iterate through columns in DT, copying the contents to the matrix column
    for (R_len_t jj = startcol; jj <= endcol; jj++) {
      memcpy(pmat + vecIdx, pcol[jj], sizeof(int)*n);
      vecIdx += n;
    }
  }
  
  return mat;
}

SEXP asmatrix_raw(SEXP dt, R_xlen_t matlen, R_xlen_t n) {
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocVector(RAWSXP, matlen));
  Rbyte *pmat; 
  pmat = RAW(mat);
  
  // Create an array of pointers for the individual columns in DT 
  R_len_t dtncol = length(dt);
  Rbyte **pcol;
  pcol = (Rbyte **) R_alloc(dtncol, sizeof(Rbyte *));
  for (R_len_t jj = 0; jj < dtncol; jj++) {
    pcol[jj] = RAW(VECTOR_ELT(dt, jj));
  }
  
  // Set up parallel OMP chunk
  R_len_t chunksize = dtncol/getDTthreads() + 1; 
  #pragma omp parallel num_threads(getDTthreads())
  { 
    // Determine which columns in DT we're iterating through on this thread
    int threadnum = omp_get_thread_num();
    R_len_t startcol = threadnum * chunksize;
    R_len_t endcol = startcol + chunksize - 1;
    if (endcol >= dtncol) 
      endcol = dtncol - 1;
    
    // Determine where to fill in the matrix vector
    R_xlen_t vecIdx = startcol * n;
    
    // Iterate through columns in DT, copying the contents to the matrix column
    for (R_len_t jj = startcol; jj <= endcol; jj++) {
      memcpy(pmat + vecIdx, pcol[jj], sizeof(Rbyte)*n);
      vecIdx += n;
    }
  }

return mat;
}

SEXP asmatrix_numeric(SEXP dt, R_xlen_t matlen, R_xlen_t n) {
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocVector(REALSXP, matlen));
  double *pmat; 
  pmat = REAL(mat);
  
  // Create an array of pointers for the individual columns in DT 
  R_len_t dtncol = length(dt);
  double **pcol;
  pcol = (double **) R_alloc(dtncol, sizeof(double *));
  for (R_len_t jj = 0; jj < dtncol; jj++) {
    pcol[jj] = REAL(VECTOR_ELT(dt, jj));
  }
  
  // Set up parallel OMP chunk
  R_len_t chunksize = dtncol/getDTthreads() + 1; 
  #pragma omp parallel num_threads(getDTthreads())
  { 
    // Determine which columns in DT we're iterating through on this thread
    int threadnum = omp_get_thread_num();
    R_len_t startcol = threadnum * chunksize;
    R_len_t endcol = startcol + chunksize - 1;
    if (endcol >= dtncol) 
      endcol = dtncol - 1;
    
    // Determine where to fill in the matrix vector
    R_xlen_t vecIdx = startcol * n;

    // Iterate through columns in DT, copying the contents to the matrix column
    for (R_len_t jj = startcol; jj <= endcol; jj++) {
      memcpy(pmat + vecIdx, pcol[jj], sizeof(double)*n);
      vecIdx += n;
    }
  }
  
  return mat;
}

SEXP asmatrix_complex(SEXP dt, R_xlen_t matlen, R_xlen_t n) {
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocVector(CPLXSXP, matlen));
  Rcomplex *pmat; 
  pmat = COMPLEX(mat);
  
  // Create an array of pointers for the individual columns in DT 
  R_len_t dtncol = length(dt);
  Rcomplex **pcol;
  pcol = (Rcomplex **) R_alloc(dtncol, sizeof(Rcomplex *));
  for (R_len_t jj = 0; jj < dtncol; jj++) {
    pcol[jj] = COMPLEX(VECTOR_ELT(dt, jj));
  }
  
  // Set up parallel OMP chunk
  R_len_t chunksize = dtncol/getDTthreads() + 1; 
  #pragma omp parallel num_threads(getDTthreads())
  { 
    // Determine which columns in DT we're iterating through on this thread
    int threadnum = omp_get_thread_num();
    R_len_t startcol = threadnum * chunksize;
    R_len_t endcol = startcol + chunksize - 1;
    if (endcol >= dtncol) 
      endcol = dtncol - 1;
    
    // Determine where to fill in the matrix vector
    R_xlen_t vecIdx = startcol * n;
    
    // Iterate through columns in DT, copying the contents to the matrix column
    for (R_len_t jj = startcol; jj <= endcol; jj++) {
      memcpy(pmat + vecIdx, pcol[jj], sizeof(Rcomplex)*n);
      vecIdx += n;
    }
  }

  return mat;
}

SEXP asmatrix_character(SEXP dt, R_xlen_t matlen, R_len_t n) {
  // Create output matrix, allocate memory, and create a pointer to it
  SEXP mat = PROTECT(allocVector(STRSXP, matlen)); // output matrix
  SEXP pcol; // pointers to casted R objects
  
  // Determine number of columns and rows
  R_len_t dtncol = length(dt);

  // Iterate through dt and copy into mat 
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  for (R_len_t jj = 0; jj < dtncol; jj++) {
    pcol = VECTOR_ELT(dt, jj);
    for (R_len_t ii = 0; ii < n; ii++) {
      SET_STRING_ELT(mat, vecIdx, STRING_ELT(pcol, ii));
      vecIdx++;
    }
  }
  
  return mat;
}

SEXP asmatrix_list(SEXP dt, R_xlen_t matlen, R_len_t n) {
  // Create output matrix, allocate memory, and create a pointer to it
  SEXP mat = PROTECT(allocVector(VECSXP, matlen)); // output matrix
  SEXP pcol; // pointers to casted R objects
  
  // Determine number of columns and rows
  R_len_t dtncol = length(dt);

  // Iterate through dt and copy into mat 
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  for (R_len_t jj = 0; jj < dtncol; jj++) {
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
    tic = omp_get_wtime();
  
  // Determine the number of rows and columns in the data.table
  R_xlen_t p = xlength(dt);
  R_xlen_t n = xlength(VECTOR_ELT(dt, 0));
  
  // Determine the number of elements in the resulting matrix
  R_xlen_t matlen = n * p;
  
  /* Conversion to a common atomic type is handled in R. We detect the
   * atomic type from the first column. */
  SEXPTYPE R_atomic_type = TYPEOF(VECTOR_ELT(dt, 0));
  
  // Copy values from dt into the matrix mat using appropriately typed function
  SEXP mat;
  switch(R_atomic_type) {
    case RAWSXP:
      mat = asmatrix_raw(dt, matlen, n);
      break;
    case LGLSXP:
    case INTSXP: 
      mat = asmatrix_integer(dt, matlen, n);
      break;
    case REALSXP: 
      mat = asmatrix_numeric(dt, matlen, n);
      break;
    case CPLXSXP: 
      mat = asmatrix_complex(dt, matlen, n);
      break;
    case STRSXP: 
      mat = asmatrix_character(dt, matlen, (R_len_t) n);
      break;
    case VECSXP:
      mat = asmatrix_list(dt, matlen, (R_len_t) n);
      break;
    default:
      error("Internal error: unsupported matrix type '%s'", type2char(R_atomic_type)); // nocov
  }
  
  if (verbose) 
    Rprintf("%s: took %.3fs\n", __func__, omp_get_wtime()-tic);
  
  UNPROTECT(1);
  return(mat);
}

