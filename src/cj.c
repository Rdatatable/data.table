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

SEXP unnest(SEXP x, SEXP cols) {
  int k = LENGTH(cols);
  // nothing to unnest -- need to go further, just be sure to copy
  if (k == 0)
    return (duplicate(x));
  int n = LENGTH(VECTOR_ELT(x, 0));
  int p = LENGTH(x);

  if (TYPEOF(cols) != INTSXP)
    error(_("cols must be an integer vector, got %s"), type2char(TYPEOF(cols)));
  int *colp = INTEGER(cols);

  // ignore non-VECSXP elements of cols; use lcols and lk (list-only versions)
  int lcols[k], j;
  int lk=0;
  for (int i=0; i<k; i++) {
    j = colp[i];
    if (j < 1 || j > p)
      error(_("cols to unnest must be in [1, ncol(x)=%d], but cols[%d]=%d"), p, i, j);
    switch(TYPEOF(VECTOR_ELT(x, j-1))) {
    case RAWSXP :
    case LGLSXP :
    case INTSXP :
    case REALSXP :
    case CPLXSXP :
    case STRSXP : break;
    case VECSXP : {
      lcols[lk++] = j-1; // move to 0-based
    } break;
    default:
      error(_("Unsupported type: %s"), type2char(TYPEOF(VECTOR_ELT(x, j))));
    }
  }

  int row_counts[n];
  int eachrep[n];

  double this_row;
  int out_rows=0, i=0;
  // find the mapping i -> # rows corresponding to i in output,
  //   also check feasibility of output
  while (i < n) {
    this_row = 1;
    for (int jj=0; jj<lk; jj++)
      this_row *= LENGTH(VECTOR_ELT(VECTOR_ELT(x, lcols[jj]), i));
    if (this_row > INT_MAX)
      error("Implied number of unnested rows from row %d (%.0f) exceeds %d, the maximum currently allowed.", i, this_row, INT_MAX);
    if (out_rows > INT_MAX - this_row) {
      double out_rows_dbl = (double) out_rows;
      for (;i<n; i++)
        out_rows_dbl += row_counts[i];
      error("Implied number of unnested rows (%.0f) exceeds %d, the maximum currently allowed.", out_rows_dbl, INT_MAX);
    }
    row_counts[i] = (int) this_row;
    out_rows += row_counts[i++];
    eachrep[i] = 1; // initializing
  }

  // TODO: factor columns?
  SEXP ans = PROTECT(allocVector(VECSXP, p));
  copyMostAttrib(x, ans);
  int outi=0;
  for (int j=p-1, lj=lk-1; j>=0; j--) {
    Rprintf("j=%d\n", j);
    SEXP xj = VECTOR_ELT(x, j), ansj;
    if (lj >= 0 && j == lcols[lj]) { // vec col: apply unnesting
      // TODO: type bumping for mismatch
      SET_VECTOR_ELT(ans, j, ansj=allocVector(TYPEOF(VECTOR_ELT(xj, 0)), out_rows));
      for (int i=0; i<n; i++) {
        Rprintf("\ti=%d\n", i);
        cycle_vector(VECTOR_ELT(xj, i), ansj + outi, lj, eachrep[i], row_counts[i]);
        eachrep[i] *= LENGTH(VECTOR_ELT(xj, i));
        outi += row_counts[i];
      }
      lj--;
    } else { // non-vec col: simply copy
      SET_VECTOR_ELT(ans, j, ansj=allocVector(TYPEOF(xj), out_rows));
      switch(TYPEOF(xj)) {
      case RAWSXP: {
        const Rbyte *source = RAW(xj);
        Rbyte *target = RAW(ansj);
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            memcpy(target + outi + repi, &source[i], sizeof(Rbyte));
          }
          outi += row_counts[i];
        }
      } break;
      case LGLSXP:
      case INTSXP: {
        const int *source = INTEGER(xj);
        int *target = INTEGER(ansj);
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            memcpy(target + outi + repi, &source[i], sizeof(int));
          }
          outi += row_counts[i];
        }
      } break;
      case REALSXP: {
        const double *source = REAL(xj);
        double *target = REAL(ansj);
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            memcpy(target + outi + repi, &source[i], sizeof(double));
          }
          outi += row_counts[i];
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *source = COMPLEX(xj);
        Rcomplex *target = COMPLEX(ansj);
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            memcpy(target + outi + repi, &source[i], sizeof(Rcomplex));
          }
          outi += row_counts[i];
        }
      } break;
      case STRSXP: {
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            SET_STRING_ELT(ansj, outi + repi, STRING_ELT(xj, i));
          }
          outi += row_counts[i];
        }
      } break;
      default: error(_("Unsupported column type %s"), type2char(TYPEOF(xj)));
      }
    }
    copyMostAttrib(xj, ansj);
  }
  Rprintf("done\n");

  // copy names
  SEXP ansNames;
  SEXP xNames = PROTECT(getAttrib(x, R_NamesSymbol));
  setAttrib(ans, R_NamesSymbol, ansNames=allocVector(STRSXP, p));
  for (int j=0; j<p; j++) {
    SET_STRING_ELT(ansNames, j, STRING_ELT(xNames, j));
  }
  UNPROTECT(2);
  return (ans);
}
