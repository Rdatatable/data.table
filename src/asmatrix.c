#include "data.table.h"

/* To handle different matrix types (e.g. numeric, character, etc)
 * there is a single dispatch function asmatrix at the bottom of the 
 * file that detects the appropriate R atomic type then calls the
 * corresponding asmatrix_<type> function.
 */

SEXP asmatrix_logical(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocVector(LGLSXP, matlen));
  int *pmat; 
  pmat = LOGICAL(mat);
  
  /* Create an array of pointers for the individual columns in DT 
   * so that we avoid creating these and calling the helper functions
   * in the parallel OMP threads
   */
  R_xlen_t dtncol = xlength(dt);
  int *pcol[dtncol];
  for (R_xlen_t jj = 0; jj < dtncol; jj++) {
    pcol[jj] = LOGICAL(VECTOR_ELT(dt, jj));
  }

  /* Using OMP is slightly less straightforward here because the number 
   * of columns in the matrix may differ from the number of columns in
   * the data.table if there is a rownames column we're skipping over.
   * This means we have a lot of manual set up to make sure all our 
   * array indices are pointing to the right place in memory across 
   * threads. 
   */
  
  R_xlen_t chunksize = dtncol/getDTthreads() + 1; 
  #pragma omp parallel num_threads(getDTthreads())
  { 
    // Determine which columns in DT we're iterating through on this thread
    int threadnum = omp_get_thread_num();
    R_xlen_t startcol = threadnum * chunksize;
    R_xlen_t endcol = startcol + chunksize - 1;
    if (endcol >= dtncol) endcol = dtncol - 1;
    
    // Determine where to fill in the matrix vector
    R_xlen_t vecIdx = startcol * n;
    if (rncolnum < startcol) vecIdx -= n;
    
    // Iterate through columns in DT, copying the contents to the matrix column
    for (R_xlen_t jj = startcol; jj <= endcol; jj++) {
      if (jj == rncolnum) continue; // skip rownames.
      memcpy(pmat + vecIdx, pcol[jj], sizeof(int)*n);
      vecIdx += n;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_integer(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocVector(INTSXP, matlen));
  int *pmat; 
  pmat = INTEGER(mat);
  
  // Create an array of pointers for the individual columns in DT 
  R_xlen_t dtncol = xlength(dt);
  int *pcol[dtncol];
  for (R_xlen_t jj = 0; jj < dtncol; jj++) {
    pcol[jj] = INTEGER(VECTOR_ELT(dt, jj));
  }

  // Set up parallel OMP chunk
  R_xlen_t chunksize = dtncol/getDTthreads() + 1; 
  #pragma omp parallel num_threads(getDTthreads())
  { 
    // Determine which columns in DT we're iterating through on this thread
    int threadnum = omp_get_thread_num();
    R_xlen_t startcol = threadnum * chunksize;
    R_xlen_t endcol = startcol + chunksize - 1;
    if (endcol >= dtncol) endcol = dtncol - 1;
    
    // Determine where to fill in the matrix vector
    R_xlen_t vecIdx = startcol * n;
    if (rncolnum < startcol) vecIdx -= n;
    
    // Iterate through columns in DT, copying the contents to the matrix column
    for (R_xlen_t jj = startcol; jj <= endcol; jj++) {
      if (jj == rncolnum) continue; // skip rownames.
      memcpy(pmat + vecIdx, pcol[jj], sizeof(int)*n);
      vecIdx += n;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_numeric(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocVector(REALSXP, matlen));
  double *pmat; 
  pmat = REAL(mat);
  
  // Create an array of pointers for the individual columns in DT 
  R_xlen_t dtncol = xlength(dt);
  double *pcol[dtncol];
  for (R_xlen_t jj = 0; jj < dtncol; jj++) {
    pcol[jj] = REAL(VECTOR_ELT(dt, jj));
  }
  
  // Set up parallel OMP chunk
  R_xlen_t chunksize = dtncol/getDTthreads() + 1; 
  #pragma omp parallel num_threads(getDTthreads())
  { 
    // Determine which columns in DT we're iterating through on this thread
    int threadnum = omp_get_thread_num();
    R_xlen_t startcol = threadnum * chunksize;
    R_xlen_t endcol = startcol + chunksize - 1;
    if (endcol >= dtncol) endcol = dtncol - 1;
    
    // Determine where to fill in the matrix vector
    R_xlen_t vecIdx = startcol * n;
    if (rncolnum < startcol) vecIdx -= n;
    
    // Iterate through columns in DT, copying the contents to the matrix column
    for (R_xlen_t jj = startcol; jj <= endcol; jj++) {
      if (jj == rncolnum) continue; // skip rownames.
      memcpy(pmat + vecIdx, pcol[jj], sizeof(double)*n);
      vecIdx += n;
    }
  }
  
  UNPROTECT(1);
  return mat;
}

SEXP asmatrix_complex(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  // Create output matrix, allocate memory, and create a pointer to it.
  SEXP mat = PROTECT(allocVector(CPLXSXP, matlen));
  Rcomplex *pmat; 
  pmat = COMPLEX(mat);
  
  // Create an array of pointers for the individual columns in DT 
  R_xlen_t dtncol = xlength(dt);
  Rcomplex *pcol[dtncol];
  for (R_xlen_t jj = 0; jj < dtncol; jj++) {
    pcol[jj] = COMPLEX(VECTOR_ELT(dt, jj));
  }
  
  // Set up parallel OMP chunk
  R_xlen_t chunksize = dtncol/getDTthreads() + 1; 
  #pragma omp parallel num_threads(getDTthreads())
  { 
    // Determine which columns in DT we're iterating through on this thread
    int threadnum = omp_get_thread_num();
    R_xlen_t startcol = threadnum * chunksize;
    R_xlen_t endcol = startcol + chunksize - 1;
    if (endcol >= dtncol) endcol = dtncol - 1;
    
    // Determine where to fill in the matrix vector
    R_xlen_t vecIdx = startcol * n;
    if (rncolnum < startcol) vecIdx -= n;
    
    // Iterate through columns in DT, copying the contents to the matrix column
    for (R_xlen_t jj = startcol; jj <= endcol; jj++) {
      if (jj == rncolnum) continue; // skip rownames.
      memcpy(pmat + vecIdx, pcol[jj], sizeof(Rcomplex)*n);
      vecIdx += n;
    }
  }

UNPROTECT(1);
return mat;
}

SEXP asmatrix_character(SEXP dt, R_xlen_t matlen, R_xlen_t n, R_xlen_t rncolnum) {
  // Create output matrix, allocate memory, and create a pointer to it
  SEXP mat = PROTECT(allocVector(STRSXP, matlen)); // output matrix
  SEXP pcol; // pointers to casted R objects
  
  // Iterate through dt and copy into mat 
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
  for (R_xlen_t jj = 0; jj < xlength(dt); jj++) {
    if (jj == rncolnum) continue; // skip rownames.
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
  // Create output matrix, allocate memory, and create a pointer to it
  SEXP mat = PROTECT(allocVector(VECSXP, matlen)); // output matrix
  SEXP pcol; // pointers to casted R objects
  
  // Iterate through dt and copy into mat 
  R_xlen_t vecIdx = 0; // counter to track place in vector underlying matrix
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

