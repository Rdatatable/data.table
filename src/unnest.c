#include "data.table.h"
#include <Rdefines.h>

SEXP unnest(SEXP x) {
  int n = LENGTH(VECTOR_ELT(x, 0));
  int p = LENGTH(x);
  int row_counts[n];
  SEXPTYPE col_types[p];

  bool all_atomic = true;

  for (int j=0; j<p; j++) {
    SEXPTYPE this_col = TYPEOF(VECTOR_ELT(x, j));
    switch(this_col) {
    case VECSXP :
      // don't break; drop through to record col_types
      all_atomic = false;
    case RAWSXP :
    case LGLSXP :
    case INTSXP :
    case REALSXP :
    case CPLXSXP :
    case STRSXP :
      col_types[j] = this_col;
      break;
    default:
      error(_("Unsupported type: %s"), type2char(this_col));
    }
  }

  // no need to go further, just be sure to copy
  if (all_atomic) {
    return (duplicate(x));
  }

  double this_row;
  SEXP xj;
  // find the mapping i -> # rows corresponding to i in output
  for (int i=0; i<n; i++) {
    this_row = 1;
    for (int j=0; j<p; j++) {
      xj = VECTOR_ELT(x, j);
      switch(col_types[j]) {
      case RAWSXP :
      case LGLSXP :
      case INTSXP :
      case REALSXP :
      case CPLXSXP :
      case STRSXP : break;
      case VECSXP :
        this_row *= LENGTH(VECTOR_ELT(xj, i));
        break;
      default:
        error(_("Unsupported type: %s"), type2char(col_types[j]));
      }
    }
    if (this_row > INT_MAX)
      error("Implied number of unnested rows from row %d (%.0f) exceeds %d, the maximum currently allowed.", i, this_row, INT_MAX);
    row_counts[i] = (int) this_row;
  }

  int out_rows=0;
  int i=0;
  while (i < n) {
    if (out_rows > INT_MAX - row_counts[i]) {
      Rprintf("out_rows=%d; row_counts[i]=%d\n", out_rows, row_counts[i]);
      double out_rows_dbl = (double) out_rows;
      for (;i<n; i++) out_rows_dbl += row_counts[i];
      error("Implied number of unnested rows (%.0f) exceeds %d, the maximum currently allowed.", out_rows_dbl, INT_MAX);
    }
    out_rows += row_counts[i++];
  }

  // TODO: factor columns?
  SEXP ans = PROTECT(allocVector(VECSXP, p));
  for (int j=0; j<p; j++) {
    xj = VECTOR_ELT(x, j);
    int outi=0;
    SEXP tmp;
    switch(col_types[j]) {
    case RAWSXP: {
      tmp = PROTECT(allocVector(RAWSXP, out_rows));
      Rbyte *tmpp = RAW(tmp);
      Rbyte *xjp = RAW(xj);
      for (int i=0; i<n; i++) {
        for (int repi=0; repi<row_counts[i]; repi++) {
          tmpp[outi++] = xjp[i];
        }
      }
    } break;
    case LGLSXP:
    case INTSXP: {
      tmp = PROTECT(allocVector(TYPEOF(xj), out_rows));
      int *tmpp = INTEGER(tmp);
      int *xjp = INTEGER(xj);
      for (int i=0; i<n; i++) {
        for (int repi=0; repi<row_counts[i]; repi++) {
          tmpp[outi++] = xjp[i];
        }
      }
    } break;
    case REALSXP: {
      tmp = PROTECT(allocVector(REALSXP, out_rows));
      double *tmpp = REAL(tmp);
      double *xjp = REAL(xj);
      for (int i=0; i<n; i++) {
        for (int repi=0; repi<row_counts[i]; repi++) {
          tmpp[outi++] = xjp[i];
        }
      }
    } break;
    case CPLXSXP: {
      tmp = PROTECT(allocVector(CPLXSXP, out_rows));
      Rcomplex *tmpp = COMPLEX(tmp);
      Rcomplex *xjp = COMPLEX(xj);
      for (int i=0; i<n; i++) {
        for (int repi=0; repi<row_counts[i]; repi++) {
          tmpp[outi++] = xjp[i];
        }
      }
    } break;
    case STRSXP: {
      tmp = PROTECT(allocVector(STRSXP, out_rows));
      for (int i=0; i<n; i++) {
        for (int repi=0; repi<row_counts[i]; repi++) {
          SET_STRING_ELT(tmp, outi++, STRING_ELT(xj, i));
        }
      }
    } break;
    case VECSXP: {
      // TODO: type bumping for mismatch
      SEXP xj0 = VECTOR_ELT(xj, 0);
      tmp = PROTECT(allocVector(TYPEOF(xj0), out_rows));
      switch(TYPEOF(xj0)) {
      case RAWSXP: {
        Rbyte *tmpp = RAW(tmp);
        for (int i=0; i<n; i++) {
          SEXP xji = VECTOR_ELT(xj, i);
          Rbyte *xjip = RAW(xji);
          for (int k=0; k<LENGTH(xji); k++) {
            for (int repi=0; repi<row_counts[i]/LENGTH(xji); repi++) {
              tmpp[outi++] = xjip[k];
            }
          }
        }
      } break;
      case LGLSXP:
      case INTSXP: {
        int *tmpp = INTEGER(tmp);
        for (int i=0; i<n; i++) {
          SEXP xji = VECTOR_ELT(xj, i);
          int *xjip = INTEGER(xji);
          for (int k=0; k<LENGTH(xji); k++) {
            for (int repi=0; repi<row_counts[i]/LENGTH(xji); repi++) {
              tmpp[outi++] = xjip[k];
            }
          }
        }
      } break;
      case REALSXP: {
        double *tmpp = REAL(tmp);
        for (int i=0; i<n; i++) {
          SEXP xji = VECTOR_ELT(xj, i);
          double *xjip = REAL(xji);
          for (int k=0; k<LENGTH(xji); k++) {
            for (int repi=0; repi<row_counts[i]/LENGTH(xji); repi++) {
              tmpp[outi++] = xjip[k];
            }
          }
        }
      } break;
      case CPLXSXP: {
        Rcomplex *tmpp = COMPLEX(tmp);
        for (int i=0; i<n; i++) {
          SEXP xji = VECTOR_ELT(xj, i);
          Rcomplex *xjip = COMPLEX(xji);
          for (int k=0; k<LENGTH(xji); k++) {
            for (int repi=0; repi<row_counts[i]/LENGTH(xji); repi++) {
              tmpp[outi++] = xjip[k];
            }
          }
        }
      } break;
      case STRSXP: {
        for (int i=0; i<n; i++) {
          SEXP xji = VECTOR_ELT(xj, i);
          for (int k=0; k<LENGTH(xji); k++) {
            for (int repi=0; repi<row_counts[i]/LENGTH(xji); repi++) {
              SET_STRING_ELT(tmp, outi++, STRING_ELT(xji, k));
            }
          }
        }
      } break;
      case VECSXP: {
        for (int i=0; i<n; i++) {
          SEXP xji = VECTOR_ELT(xj, i);
          for (int k=0; k<LENGTH(xji); k++) {
            for (int repi=0; repi<row_counts[i]/LENGTH(xji); repi++) {
              SET_VECTOR_ELT(tmp, outi++, VECTOR_ELT(xji, k));
            }
          }
        }
      } break;
      default: error("Invalid nested column type %s in column %d", type2char(TYPEOF(xj0)), p);
      }
    } break;
    default: error("Internal error: invalid column type %s should have been caught earlier in unnest", type2char(col_types[j]));
    }
    SET_VECTOR_ELT(ans, j, tmp);
    UNPROTECT(1);
  }

  copyMostAttrib(x, ans);
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
