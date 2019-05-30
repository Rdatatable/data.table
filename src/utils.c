#include "data.table.h"

/*
 * input validation and extraction
 */
bool trueFalseR(SEXP x) {
  if (!isLogical(x) || length(x)!=1 || LOGICAL(x)[0]==NA_LOGICAL)
    error("internal error: argument must be TRUE or FALSE"); // # nocov
  return LOGICAL(x)[0];
}

/*
 * which equals to
 * which_eq, !which_eq
 */
void which_eq_int(int *x, int nx, int *out, int *nout, int val, bool negate) {
  int j=0;
  if (negate) {
    for (int i=0; i<nx; i++) {
      if (x[i] != val) {
        out[j] = i;
        j++;
      }
    }
  } else {
    for (int i=0; i<nx; i++) {
      if (x[i] == val) {
        out[j] = i;
        j++;
      }
    }
  }
  nout[0] = j;
}
void which_eq_double(double *x, int nx, int *out, int *nout, double val, bool negate) {
  int j=0;
  if (ISNA(val)) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        if (!ISNA(x[i])) {
          out[j] = i;
          j++;
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        if (ISNA(x[i])) {
          out[j] = i;
          j++;
        }
      }
    }
  } else if (ISNAN(val)) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        if (!ISNAN(x[i]) || ISNA(x[i])) {
          out[j] = i;
          j++;
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        if (ISNAN(x[i]) && !ISNA(x[i])) {
          out[j] = i;
          j++;
        }
      }
    }
  } else {
    if (negate) {
      for (int i=0; i<nx; i++) {
        if (x[i] != val) {
          out[j] = i;
          j++;
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        if (x[i] == val) {
          out[j] = i;
          j++;
        }
      }
    }
  }
  nout[0] = j;
}
void which_eq_char(SEXP x, int nx, int *out, int *nout, SEXP val, bool negate) {
  int j=0;
  if (negate) {
    for (int i=0; i<nx; i++) {
      if (STRING_ELT(x, i) != val) {
        out[j] = i;
        j++;
      }
    }
  } else {
    for (int i=0; i<nx; i++) {
      if (STRING_ELT(x, i) == val) {
        out[j] = i;
        j++;
      }
    }
  }
  nout[0] = j;
}
SEXP which_eq_doubleR(SEXP x, SEXP val, SEXP negate) {
  int protecti = 0;
  R_len_t nx = length(x);
  SEXP ans = PROTECT(allocVector(INTSXP, nx)); protecti++;
  int *ians = INTEGER(ans);
  int nans=0;
  double dval = REAL(val)[0];
  bool bnegate = trueFalseR(negate);
  which_eq_double(REAL(x), nx, ians, &nans, dval, bnegate);
  SETLENGTH(ans, nans);
  UNPROTECT(protecti);
  return ans;
}
SEXP which_eq_charR(SEXP x, SEXP val, SEXP negate) {
  int protecti = 0;
  R_len_t nx = length(x);
  SEXP ans = PROTECT(allocVector(INTSXP, nx)); protecti++;
  int *ians = INTEGER(ans);
  int nans=0;
  bool bnegate = trueFalseR(negate);
  which_eq_char(x, nx, ians, &nans, STRING_ELT(val, 0), bnegate);
  SETLENGTH(ans, nans);
  UNPROTECT(protecti);
  return ans;
}
