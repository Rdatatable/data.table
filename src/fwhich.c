#include "data.table.h"

/*
 * which equals to
 * which_eq, !which_eq
 */
static inline void which_eq_int(int *x, int nx, int *out, int *nout, int val, bool negate, int *y, int ny) {
  int j=0;
  if (ny<0) {
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
  } else if (ny>0) { // short circuit when intersect/y arg provided
    if (negate) {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] != val) {
          out[j] = y[i];
          j++;
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] == val) {
          out[j] = y[i];
          j++;
        }
      }
    }
  }
  nout[0] = j;
}
static inline void which_eq_double(double *x, int nx, int *out, int *nout, double val, bool negate) {
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
static inline void which_eq_char(SEXP x, int nx, int *out, int *nout, SEXP val, bool negate) {
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
static inline void which_eq_int64(int64_t *x, int nx, int *out, int *nout, int64_t val, bool negate) {
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
void which_eq(SEXP x, int nx, int *iwhich, int *nwhich, SEXP val, bool negate, int *y, int ny) {
  switch(TYPEOF(x)) {
  case LGLSXP: {
    which_eq_int(LOGICAL(x), nx, iwhich, nwhich, LOGICAL(val)[0], negate, y, ny);
  } break;
  case INTSXP: {
    which_eq_int(INTEGER(x), nx, iwhich, nwhich, INTEGER(val)[0], negate, y, ny);
  } break;
  case REALSXP: {
    if (Rinherits(x, char_integer64)) {
    which_eq_int64((int64_t *)REAL(x), nx, iwhich, nwhich, ((int64_t *)REAL(val))[0], negate);
  } else {
    which_eq_double(REAL(x), nx, iwhich, nwhich, REAL(val)[0], negate);
  }
  } break;
  case STRSXP: {
    which_eq_char(x, nx, iwhich, nwhich, STRING_ELT(val, 0), negate);
  } break;
  default: {
    error("%s: Incompatible type: %s", __func__, type2char(TYPEOF(x)));
  }
  }
}
SEXP which_eqR(SEXP x, SEXP val, SEXP negate, SEXP intersect) {
  int protecti = 0;
  if (!isVectorAtomic(x))
    error("%s: argument 'x' must be atomic vector", __func__);
  if (TYPEOF(x)!=TYPEOF(val))
    error("%s: argument 'value' must of the same type as argument 'x'", __func__);
  if (length(val)!=1)
    error("%s: argument 'value' must be length 1", __func__);
  if (!IS_TRUE_OR_FALSE(negate))
    error("%s: argument 'negate' must be TRUE or FALSE", __func__);
  if (!isNull(intersect) && !isInteger(intersect))
    error("%s: argument 'intersect' must be integer or NULL", __func__);

  // check all intersect values are smaller than length(x), no NAs, no dups
  R_len_t nx = length(x);
  int ny = isNull(intersect) ? -1 : length(intersect);
  int nans = isNull(intersect) ? nx : MIN(nx, ny);
  if (ny>0)
    intersect = duplicate(intersect);
  int *yp = INTEGER(intersect);
  for (int i=0; i<ny; i++) {
    yp[i]--; // shift 1-based index to 0-based index - this is for intersect argument, a short circuit
  }
  SEXP ans = PROTECT(allocVector(INTSXP, nans)); protecti++;
  int *iwhich = INTEGER(ans);
  int nwhich = 0;

  which_eq(x, nx, iwhich, &nwhich, val, LOGICAL(negate)[0], yp, ny);

  for (int i=0; i<nwhich; i++) {
    iwhich[i]++; // shift 0-based index to 1-based index
  }

  SETLENGTH(ans, nwhich);
  UNPROTECT(protecti);
  return ans;
}

// fast intersect only when x and y sorted
void fintersect(int *x, int nx, int *y, int ny, int *out, int *nans) {
  if (nx && ny) { // otherwise segfault when fintersect(integer(), integer())
    for (R_xlen_t i=0, j=0;;) {
      if (x[i]==y[j]) {
        out[nans[0]++] = x[i];
        i++; j++;
        if (i==nx || j==ny) break;
      } else if (x[i]>y[j]) {
        j++;
        if (j==ny) break;
      } else if (x[i]<y[j]) {
        i++;
        if (i==nx) break;
      }
    }
  }
}

// assumes x and y to be sorted
SEXP fintersectR(SEXP x, SEXP y) {
  if (!isInteger(x))
    error("fintersect accept only integer class");
  if (!isInteger(y))
    error("fintersect accept only integer class");
  // check no NAs
  // check no duplicates
  // check sorted
  R_len_t nx = xlength(x), ny = xlength(y);
  int *xp = INTEGER(x), *yp = INTEGER(y);

  SEXP ans = PROTECT(allocVector(INTSXP, MIN(nx, ny)));
  int *ansp = INTEGER(ans);
  int nans = 0;

  fintersect(xp, nx, yp, ny, ansp, &nans);

  SETLENGTH(ans, nans);
  UNPROTECT(1);
  return ans;
}
/* //outdated stuff below
bool is_and_expr(SEXP expr) {
  // iterate over expr pairlist
  return true;
}
SEXP disassemble_expr(SEXP expr) {
  // turn into non evaluated list
  return R_NilValue;
}
SEXP anding(SEXP expr, SEXP rho) {
  SEXP ans = R_NilValue;
  if (is_and_expr(expr)) {
    Rprintf("fwhich optimization\n");
    SEXP e = disassemble_expr(expr);
    // should we be lazy? DT[a == 1L & a == 2L & b == stop("ignored")]
    bool zero_ans = 0;
    for (int i=0; i<length(e); i++) {
      if (zero_ans) {
        continue;
      }
      // calculate which_equal, which_in, etc.
      SEXP this_ans = which_equal();
      if (length(this_ans)) {
        zero_ans = 1;
      }
      if (i>0) {
        ansp* = fintersect(ansp, this_ansp);
      }
    }
  } else {
    Rprintf("fwhich optimization not available for provided expression\n");
    ans = eval(expr, rho);
  }
  return ans;
}
*/
