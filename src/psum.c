#include "data.table.h"

// like base::p{max,min}, but for sum, prod
SEXP psum(SEXP x, SEXP narmArg) {
  if (!isNewList(x)) error(_("Internal error: x must be a list"));
  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL) error(_("na.rm must be TRUE or FALSE"));

  int J=LENGTH(x);
  SEXPTYPE outtype = INTSXP;
  int n = -1;
  for (int j=0; j<J; j++) {
    SEXP xj=VECTOR_ELT(x, j);
    switch(TYPEOF(xj)) {
    case INTSXP:
      break;
    case REALSXP:
      outtype = REALSXP;
      break;
    default:
      error(_("Only numeric inputs are supported for psum"));
    }
    if (n >= 0) {
      if (n != LENGTH(xj)) {
        error(_("Inconsistent input lengths -- first found %d, but %d element has length %d"), n, j+1, LENGTH(xj));
      }
    } else { // initialize
      n = LENGTH(xj);
    }
  }


  // TODO: support int->double conversion
  SEXP out = PROTECT(allocVector(outtype, n));
  if (LOGICAL(narmArg)[0]) {
    if (outtype == INTSXP) {
      int *outp = INTEGER(out);
      // initialize to NA to facilitate all-NA rows --> NA output
      for (int i=0; i<n; i++) outp[i] = NA_INTEGER;
      for (int j=0; j<J; j++) {
        int *xjp = INTEGER(VECTOR_ELT(x, j));
        for (int i=0; i<n; i++) {
          if (xjp[i] != NA_INTEGER) {
            outp[i] = outp[i] == NA_INTEGER ? xjp[i] : outp[i] + xjp[i];
          } // else remain as NA
        }
      }
    } else { // REALSXP; cut-and-paste except for NA
      double *outp = REAL(out);
      for (int i=0; i<n; i++) outp[i] = NA_REAL;
      for (int j=0; j<J; j++) {
        double *xjp = REAL(VECTOR_ELT(x, j));
        for (int i=0; i<n; i++) {
          if (xjp[i] != NA_REAL) {
            outp[i] = outp[i] == NA_REAL ? xjp[i] : outp[i] + xjp[i];
          }
        }
      }
    }
  } else { // na.rm=FALSE
    if (outtype == INTSXP) {
      int *outp = INTEGER(out);
      int *xj0p = INTEGER(VECTOR_ELT(x, 0));
      for (int i=0; i<n; i++) outp[i] = xj0p[i];
      if (J == 1) {
        UNPROTECT(1);
        return out;
      }
      for (int j=1; j<J; j++) {
        int *xjp = INTEGER(VECTOR_ELT(x, j));
        for (int i=0; i<n; i++) {
          outp[i] = outp[i] == NA_INTEGER || xjp[i] == NA_INTEGER ? NA_INTEGER : outp[i] + xjp[i];
        }
      }
    } else { // REALSXP
      double *outp = REAL(out);
      double *xj0p = REAL(VECTOR_ELT(x, 0));
      for (int i=0; i<n; i++) outp[i] = xj0p[i];
      if (J == 1) {
        UNPROTECT(1);
        return out;
      }
      for (int j=1; j<J; j++) {
        double *xjp = REAL(VECTOR_ELT(x, j));
        for (int i=0; i<n; i++) {
          outp[i] = outp[i] == NA_REAL || xjp[i] == NA_REAL ? NA_REAL : outp[i] + xjp[i];
        }
      }
    }
  }

  UNPROTECT(1);
  return out;
}
