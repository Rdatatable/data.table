#include "data.table.h"

bool isRealReallyInt(SEXP x) {
  if (!isReal(x)) return(false);
  R_xlen_t n=xlength(x), i=0;
  double *dx = REAL(x);
  while (i<n &&
         ( ISNA(dx[i]) ||
         ( R_FINITE(dx[i]) && dx[i] == (int)(dx[i])))) {
    i++;
  }
  return i==n;
}

SEXP isReallyReal(SEXP x) {
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = 0;
  // return 0 (FALSE) when not type double, or is type double but contains integers
  // used to error if not passed type double but this needed extra is.double() calls in calling R code
  // which needed a repeat of the argument. Hence simpler and more robust to return 0 when not type double.
  if (isReal(x)) {
    int n=length(x), i=0;
    double *dx = REAL(x);
    while (i<n &&
        ( ISNA(dx[i]) ||
        ( R_FINITE(dx[i]) && dx[i] == (int)(dx[i])))) {
      i++;
    }
    if (i<n) INTEGER(ans)[0] = i+1;  // return the location of first element which is really real; i.e. not an integer
  }
  UNPROTECT(1);
  return(ans);
}

SEXP colnamesInt(SEXP x, SEXP cols, SEXP check_dups, SEXP check_real) {
  if (!isNewList(x))
    error("'x' argument must be data.table compatible");
  if (!IS_TRUE_OR_FALSE(check_dups))
    error("'check_dups' argument must be TRUE or FALSE");
  if (!IS_TRUE_OR_FALSE(check_real))
    error("'check_real' argument must be TRUE or FALSE");
  int protecti = 0;
  R_len_t nx = length(x);
  R_len_t nc = length(cols);
  SEXP ricols = R_NilValue;
  if (isNull(cols)) { // seq_along(x)
    ricols = PROTECT(allocVector(INTSXP, nx)); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<nx; i++) icols[i] = i+1;
  } else if (length(cols)==0) { // integer(0)
    ricols = PROTECT(allocVector(INTSXP, 0)); protecti++;
  } else if (isInteger(cols) || isReal(cols)) {
    if (isInteger(cols)) {
      ricols = cols;
    } else if (isReal(cols)) {
      if (LOGICAL(check_real)[0] && !isRealReallyInt(cols))
        error("argument specifying columns is type 'double' and one or more items in it are not whole integers");
      ricols = PROTECT(coerceVector(cols, INTSXP)); protecti++;
    }
    int *icols = INTEGER(ricols);
    for (int i=0; i<nc; i++) {
      if ((icols[i]>nx) || (icols[i]<1))
        error("argument specifying columns specify non existing column(s): cols[%d]=%d", i+1, icols[i]); // handles NAs also
    }
  } else if (isString(cols)) {
    SEXP xnames = PROTECT(getAttrib(x, R_NamesSymbol)); protecti++;
    if (isNull(xnames))
      error("'x' argument data.table has no names");
    ricols = PROTECT(chmatch(cols, xnames, 0)); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<nc; i++) {
      if (icols[i]==0)
        error("argument specifying columns specify non existing column(s): cols[%d]='%s'", i+1, CHAR(STRING_ELT(cols, i))); // handles NAs also
    }
  } else {
    error("argument specifying columns must be character or numeric");
  }
  if (LOGICAL(check_dups)[0] && any_duplicated(ricols, FALSE))
    error("argument specifying columns specify duplicated column(s)");
  UNPROTECT(protecti);
  return ricols;
}

void coerceFill(SEXP fill, double *dfill, int32_t *ifill, int64_t *i64fill) {
  if (xlength(fill) != 1) error("%s: fill argument must be length 1", __func__);
  if (isInteger(fill)) {
    if (INTEGER(fill)[0]==NA_INTEGER) {
      ifill[0] = NA_INTEGER; dfill[0] = NA_REAL; i64fill[0] = NA_INTEGER64;
    } else {
      ifill[0] = INTEGER(fill)[0];
      dfill[0] = (double)(INTEGER(fill)[0]);
      i64fill[0] = (int64_t)(INTEGER(fill)[0]);
    }
  } else if (isReal(fill)) {
    if (INHERITS(fill,char_integer64) || INHERITS(fill,char_nanotime)) {
      long long *llfill = (long long *)REAL(fill);
      if (llfill[0]==NA_INT64_LL) {
        ifill[0] = NA_INTEGER; dfill[0] = NA_REAL; i64fill[0] = NA_INTEGER64;
      } else {
        ifill[0] = llfill[0]>INT32_MAX ? NA_INTEGER : (int32_t)(llfill[0]);
        dfill[0] = (double)(llfill[0]);
        i64fill[0] = (int64_t)(llfill[0]);
      }
    } else {
      if (ISNA(REAL(fill)[0])) {
        ifill[0] = NA_INTEGER; dfill[0] = NA_REAL; i64fill[0] = NA_INTEGER64;
      } else {
        ifill[0] = (int32_t)(REAL(fill)[0]);
        dfill[0] = REAL(fill)[0];
        i64fill[0] = (int64_t)(REAL(fill)[0]);
      }
    }
  } else if (isLogical(fill) && LOGICAL(fill)[0]==NA_LOGICAL) {
    ifill[0] = NA_INTEGER; dfill[0] = NA_REAL; i64fill[0] = NA_INTEGER64;
  } else {
    error("%s: fill argument must be numeric", __func__);
  }
}
SEXP coerceFillR(SEXP fill) {
  int protecti=0;
  double dfill=NA_REAL;
  int32_t ifill=NA_INTEGER;
  int64_t i64fill=NA_INTEGER64;
  coerceFill(fill, &dfill, &ifill, &i64fill);
  SEXP ans = PROTECT(allocVector(VECSXP, 3)); protecti++;
  SET_VECTOR_ELT(ans, 0, allocVector(INTSXP, 1));
  SET_VECTOR_ELT(ans, 1, allocVector(REALSXP, 1));
  SET_VECTOR_ELT(ans, 2, allocVector(REALSXP, 1));
  INTEGER(VECTOR_ELT(ans, 0))[0] = ifill;
  REAL(VECTOR_ELT(ans, 1))[0] = dfill;
  long long *ll = (long long *)REAL(VECTOR_ELT(ans, 2));
  ll[0] = i64fill;
  setAttrib(VECTOR_ELT(ans, 2), R_ClassSymbol, ScalarString(char_integer64));
  UNPROTECT(protecti);
  return ans;
}
