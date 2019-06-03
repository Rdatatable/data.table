#include "data.table.h"

/*
 * input validation
 */
bool isTrueFalse(SEXP x) {
  return isLogical(x) && length(x)==1 && LOGICAL(x)[0]!=NA_LOGICAL;
}
int lengthMiss(SEXP x, int n, bool scalar) {
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
int classMiss(SEXP x, SEXP char_class) {
  if (!isNewList(x)) error("x must be a list");
  int nx = length(x);
  for (int i=0; i<nx; i++) {
    if (!INHERITS(VECTOR_ELT(x, i), char_class)) return i;
  }
  return -1;
}
bool charIdentical(SEXP x, SEXP y) {
  int nx = length(x);
  if (nx != length(y)) return false;
  for (int i=0; i<nx; i++) if (STRING_ELT(x,i) != STRING_ELT(y,i)) return false;
  return true;
}
int levelsMiss(SEXP x, SEXP levels) {
  if (!isNewList(x)) error("x must be a list");
  int nx = length(x);
  for (int i=0; i<nx; i++) {
    SEXP this_levels = getAttrib(VECTOR_ELT(x, i), R_LevelsSymbol);
    if (!charIdentical(this_levels, levels)) return i;
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
void which_eq_int64(int64_t *x, int nx, int *out, int *nout, int64_t val, bool negate) {
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
SEXP which_eqR(SEXP x, SEXP val, SEXP negate) {
  int protecti = 0;

  if (!isVectorAtomic(x)) error("%s: argument 'x' must be atomic vector", __func__);

  if (TYPEOF(x)!=TYPEOF(val)) error("%s: argument 'value' must of the same type as argument 'x'", __func__);

  if (!isTrueFalse(negate)) error("%s: argument 'negate' must be TRUE or FALSE", __func__);
  bool bnegate = LOGICAL(negate)[0];

  R_len_t nx = length(x);
  SEXP ans = PROTECT(allocVector(INTSXP, nx)); protecti++;
  int *iwhich = INTEGER(ans);
  int nwhich = 0;

  switch(TYPEOF(x)) {
  case LGLSXP: {
    which_eq_int(LOGICAL(x), nx, iwhich, &nwhich, LOGICAL(val)[0], bnegate);
  } break;
  case INTSXP: {
    which_eq_int(INTEGER(x), nx, iwhich, &nwhich, INTEGER(val)[0], bnegate);
  } break;
  case REALSXP: {
    if (inherits(x,"integer64")) {
      which_eq_int64((int64_t *)REAL(x), nx, iwhich, &nwhich, ((int64_t *)REAL(val))[0], bnegate);
    } else {
      which_eq_double(REAL(x), nx, iwhich, &nwhich, REAL(val)[0], bnegate);
    }
  } break;
  case STRSXP: {
    which_eq_char(x, nx, iwhich, &nwhich, STRING_ELT(val, 0), bnegate);
  } break;
  default: {
    error("%s: Incompatible type: %s", __func__, type2char(TYPEOF(x)));
  }
  }

  for(int i=0; i<nwhich; i++) iwhich[i]++;

  SETLENGTH(ans, nwhich);
  UNPROTECT(protecti);
  return ans;
}
