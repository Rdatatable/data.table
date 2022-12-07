#include "data.table.h"
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

inline bool any_NA_int(const int n, const int *idx) {
  for (int i=0; i<n; ++i) {
    if (idx[i] == NA_INTEGER) return true;
  }
  return false;
}  

// TO DO: margins
SEXP fcast(SEXP lhs, SEXP val, SEXP nrowArg, SEXP ncolArg, SEXP idxArg, SEXP fill, SEXP fill_d, SEXP is_agg) {
  int nrows=INTEGER(nrowArg)[0], ncols=INTEGER(ncolArg)[0];
  int nlhs=length(lhs), nval=length(val), *idx = INTEGER(idxArg);
  SEXP target;

  SEXP ans = PROTECT(allocVector(VECSXP, nlhs + (nval * ncols)));
  // set lhs cols
  for (int i=0; i<nlhs; ++i) {
    SET_VECTOR_ELT(ans, i, VECTOR_ELT(lhs, i));
  }
  // get val cols
  bool some_fill = any_NA_int(nrows*ncols, idx);
  for (int i=0; i<nval; ++i) {
    SEXP thiscol = VECTOR_ELT(val, i);
    SEXP thisfill = fill;
    SEXPTYPE thistype = TYPEOF(thiscol);
    int nprotect = 0;
    if (some_fill) {
      if (isNull(fill)) {
	if (LOGICAL(is_agg)[0]) {
	  thisfill = PROTECT(allocNAVector(thistype, 1)); nprotect++;
	} else thisfill = VECTOR_ELT(fill_d, i);
      }
      if (TYPEOF(thisfill) != thistype) {
	thisfill = PROTECT(coerceVector(thisfill, thistype)); nprotect++;
      }
    }
    switch (thistype) {
    case INTSXP:
    case LGLSXP: {
      const int *ithiscol = INTEGER(thiscol);
      const int *ithisfill = 0;
      if (some_fill) ithisfill = INTEGER(thisfill);
      for (int j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target=allocVector(thistype, nrows) );
        int *itarget = INTEGER(target);
        copyMostAttrib(thiscol, target);
        for (int k=0; k<nrows; ++k) {
          int thisidx = idx[k*ncols + j];
          itarget[k] = (thisidx == NA_INTEGER) ? ithisfill[0] : ithiscol[thisidx-1];
        }
      }
    } break;
    case REALSXP: {
      const double *dthiscol = REAL(thiscol);
      const double *dthisfill = 0;
      if (some_fill) dthisfill = REAL(thisfill);
      for (int j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target=allocVector(thistype, nrows) );
        double *dtarget = REAL(target);
        copyMostAttrib(thiscol, target);
        for (int k=0; k<nrows; ++k) {
          int thisidx = idx[k*ncols + j];
          dtarget[k] = (thisidx == NA_INTEGER) ? dthisfill[0] : dthiscol[thisidx-1];
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *zthiscol = COMPLEX(thiscol);
      const Rcomplex *zthisfill = 0;
      if (some_fill) zthisfill = COMPLEX(thisfill);
      for (int j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target=allocVector(thistype, nrows) );
        Rcomplex *ztarget = COMPLEX(target);
        copyMostAttrib(thiscol, target);
        for (int k=0; k<nrows; ++k) {
          int thisidx = idx[k*ncols + j];
          ztarget[k] = (thisidx == NA_INTEGER) ? zthisfill[0] : zthiscol[thisidx-1];
        }
      }
    } break;
    case STRSXP:
      for (int j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target=allocVector(thistype, nrows) );
        copyMostAttrib(thiscol, target);
        for (int k=0; k<nrows; ++k) {
          int thisidx = idx[k*ncols + j];
          SET_STRING_ELT(target, k, (thisidx == NA_INTEGER) ? STRING_ELT(thisfill, 0) : STRING_ELT(thiscol, thisidx-1));
        }
      }
      break;
    case VECSXP:
      for (int j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target=allocVector(thistype, nrows) );
        copyMostAttrib(thiscol, target);
        for (int k=0; k<nrows; ++k) {
          int thisidx = idx[k*ncols + j];
          SET_VECTOR_ELT(target, k, (thisidx == NA_INTEGER) ? VECTOR_ELT(thisfill, 0) : VECTOR_ELT(thiscol, thisidx-1));
        }
      }
      break;
    default: error(_("Unsupported column type in fcast val: '%s'"), type2char(TYPEOF(thiscol))); // #nocov
    }
    UNPROTECT(nprotect);
  }
  UNPROTECT(1);
  return(ans);
}
