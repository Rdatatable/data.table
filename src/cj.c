#include "data.table.h"

SEXP cj(SEXP base_list) {
  int ncol = LENGTH(base_list);
  SEXP out = PROTECT(allocVector(VECSXP, ncol));
  int nrow = 1;
  // already confirmed to be less than .Machine$integer.max at R level
  for (int j=0; j<ncol; ++j) nrow *= length(VECTOR_ELT(base_list, j));
  int eachrep = 1;
  for (int j=ncol-1; j>=0; --j) {
    SEXP source = VECTOR_ELT(base_list, j), target;
    SET_VECTOR_ELT(out, j, target=allocVector(TYPEOF(source), nrow));
    copyMostAttrib(source, target);  // includes levels of factors, integer64, custom classes, etc
    if (nrow==0) continue;  // one or more columns are empty so the result will be empty, #2511
    int thislen = LENGTH(source);
    int blocklen = thislen*eachrep;
    int ncopy = nrow/blocklen;
    switch(TYPEOF(source)) {
    case LGLSXP:
    case INTSXP: {
      const int *restrict sourceP = INTEGER(source);
      int *restrict targetP = INTEGER(target);
      #pragma omp parallel for num_threads(getDTthreads(thislen*eachrep, true))
      // default static schedule so two threads won't write to same cache line in last column
      // if they did write to same cache line (and will when last column's thislen is small) there's no correctness issue
      for (int i=0; i<thislen; ++i) {
        const int item = sourceP[i];
        const int end = (i+1)*eachrep;
        for (int j=i*eachrep; j<end; ++j) targetP[j] = item;  // no div, mod or read ops inside loop; just rep a const contiguous write
      }
      #pragma omp parallel for num_threads(getDTthreads(ncopy*blocklen, true))
      for (int i=1; i<ncopy; ++i) {
        memcpy(targetP + i*blocklen, targetP, blocklen*sizeof(int));
      }
    } break;
    case REALSXP: {
      const double *restrict sourceP = REAL(source);
      double *restrict targetP = REAL(target);
      #pragma omp parallel for num_threads(getDTthreads(thislen*eachrep, true))
      for (int i=0; i<thislen; ++i) {
        const double item = sourceP[i];
        const int end=(i+1)*eachrep;
        for (int j=i*eachrep; j<end; ++j) targetP[j] = item;
      }
      #pragma omp parallel for num_threads(getDTthreads(ncopy*blocklen, true))
      for (int i=1; i<ncopy; ++i) {
        memcpy(targetP + i*blocklen, targetP, blocklen*sizeof(double));
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict sourceP = COMPLEX(source);
      Rcomplex *restrict targetP = COMPLEX(target);
      #pragma omp parallel for num_threads(getDTthreads(thislen*eachrep, true))
      for (int i=0; i<thislen; ++i) {
        const Rcomplex item = sourceP[i];
        const int end=(i+1)*eachrep;
        for (int j=i*eachrep; j<end; ++j) targetP[j] = item;
      }
      #pragma omp parallel for num_threads(getDTthreads(ncopy*blocklen, true))
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
      const SEXP *sourceP = SEXPPTR_RO(source);
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
      error(_("Type '%s' not supported by CJ."), type2char(TYPEOF(source)));
    }
    eachrep *= thislen;
  }
  UNPROTECT(1);
  return out;
}

