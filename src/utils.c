#include "data.table.h"

/*
 * input validation
 */
bool isTrueFalse(SEXP x) {
  return !isLogical(x) || length(x)!=1 || LOGICAL(x)[0]==NA_LOGICAL;
}
int lenMiss(SEXP x, int n, bool scalar) {
  if (!isNewList(x)) error("x must be a list");
  int nx = length(x);
  for (int i=0; i<nx; i++) {
    int thisn = length(VECTOR_ELT(x, i));
    if (thisn!=n && (scalar && thisn!=1)) return i;
  }
  return -1;
}
int typeMiss(SEXP x, SEXPTYPE type) {
  if (!isNewList(x)) error("x must be a list");
  int nx = length(x);
  for (int i=0; i<nx; i++) {
    SEXPTYPE thistype = TYPEOF(VECTOR_ELT(x, i));
    if (thistype!=type) return i;
  }
  return -1;
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
  bool bnegate = isTrueFalse(negate);
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
  bool bnegate = isTrueFalse(negate);
  which_eq_char(x, nx, ians, &nans, STRING_ELT(val, 0), bnegate);
  SETLENGTH(ans, nans);
  UNPROTECT(protecti);
  return ans;
}
