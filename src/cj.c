#include "data.table.h"

// workhorse for cycling a vector in the correct pattern,
//   essentially rep(x, m, each = n) for appropriate m,n at each j
void cycle_vector(SEXP source, SEXP target, int j, int eachrep, int nrow) {
  int thislen = LENGTH(source);
  int blocklen = thislen*eachrep;
  int ncopy = nrow/blocklen;
  switch(TYPEOF(source)) {
  case LGLSXP:
  case INTSXP: {
    const int *restrict sourceP = INTEGER(source);
    int *restrict targetP = INTEGER(target);
    #pragma omp parallel for num_threads(getDTthreads()) if (nrow >= OMP_MIN_VALUE)
    // default static schedule so two threads won't write to same cache line in last column
    // if they did write to same cache line (and will when last column's thislen is small) there's no correctness issue
    for (int i=0; i<thislen; ++i) {
      const int item = sourceP[i];
      const int end = (i+1)*eachrep;
      for (int j=i*eachrep; j<end; ++j) targetP[j] = item;  // no div, mod or read ops inside loop; just rep a const contiguous write
    }
    #pragma omp parallel for num_threads(getDTthreads()) if (nrow >= OMP_MIN_VALUE)
    for (int i=1; i<ncopy; ++i) {
      memcpy(targetP + i*blocklen, targetP, blocklen*sizeof(int));
    }
  } break;
  case REALSXP: {
    const double *restrict sourceP = REAL(source);
    double *restrict targetP = REAL(target);
    #pragma omp parallel for num_threads(getDTthreads()) if (nrow >= OMP_MIN_VALUE)
    for (int i=0; i<thislen; ++i) {
      const double item = sourceP[i];
      const int end=(i+1)*eachrep;
      for (int j=i*eachrep; j<end; ++j) targetP[j] = item;
    }
    #pragma omp parallel for num_threads(getDTthreads()) if (nrow >= OMP_MIN_VALUE)
    for (int i=1; i<ncopy; ++i) {
      memcpy(targetP + i*blocklen, targetP, blocklen*sizeof(double));
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict sourceP = COMPLEX(source);
    Rcomplex *restrict targetP = COMPLEX(target);
    #pragma omp parallel for num_threads(getDTthreads()) if (nrow >= OMP_MIN_VALUE)
    for (int i=0; i<thislen; ++i) {
      const Rcomplex item = sourceP[i];
      const int end=(i+1)*eachrep;
      for (int j=i*eachrep; j<end; ++j) targetP[j] = item;
    }
    #pragma omp parallel for num_threads(getDTthreads()) if (nrow >= OMP_MIN_VALUE)
    for (int i=1; i<ncopy; ++i) {
      memcpy(targetP + i*blocklen, targetP, blocklen*sizeof(Rcomplex));
    }
  } break;
  case STRSXP: {
    const SEXP *sourceP = STRING_PTR(source);
    int start = 0;
    for (int i=0; i<ncopy; ++i) {
      for (int j=0; j<thislen; ++j) {
        const SEXP item = sourceP[j];
        const int end = start+eachrep;
        for (int k=start; k<end; ++k) SET_STRING_ELT(target, k, item);  // no div, mod, or read-API call to STRING_ELT
        start = end;
      }
    }
  } break;
  case VECSXP: {
    const SEXP *sourceP = VECTOR_PTR(source);
    int start = 0;
    for (int i=0; i<ncopy; ++i) {
      for (int j=0; j<thislen; ++j) {
        const SEXP item = sourceP[j];
        const int end = start+eachrep;
        for (int k=start; k<end; ++k) SET_VECTOR_ELT(target, k, item);
        start = end;
      }
    }
  } break;
  default:
    error(_("Type '%s' not supported by for cross-join."), type2char(TYPEOF(source)));
  }
}

SEXP cj(SEXP base_list) {
  int ncol = LENGTH(base_list);
  SEXP out = PROTECT(allocVector(VECSXP, ncol));
  // start with double to allow overflow
  double nrow_dbl=1;
  int nrow;
  for (int j=0; j<ncol; ++j) nrow_dbl *= length(VECTOR_ELT(base_list, j));
  if (nrow_dbl > INT_MAX) {
      error(_("Cross product of elements provided to CJ() would result in %.0f rows which exceeds .Machine$integer.max == %d"), nrow_dbl, INT_MAX);
  } else {
    nrow = (int) nrow_dbl;
  }
  int eachrep = 1;
  for (int j=ncol-1; j>=0; --j) {
    SEXP source = VECTOR_ELT(base_list, j), target;
    SET_VECTOR_ELT(out, j, target=allocVector(TYPEOF(source), nrow));
    copyMostAttrib(source, target);  // includes levels of factors, integer64, custom classes, etc
    if (nrow==0) continue;  // one or more columns are empty so the result will be empty, #2511
    cycle_vector(source, target, j, eachrep, nrow);
    eachrep *= LENGTH(source);
  }
  UNPROTECT(1);
  return out;
}
