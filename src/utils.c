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

/* colnamesInt
 * for provided data.table (or a list-like) and a subset of its columns, it returns integer positions of those columns in DT
 * handle columns input as: integer, double, character and NULL (handled as seq_along(x))
 * adds validation for:
 *   correct range [1,ncol], and if type real checks whole integer
 *   existing columns for character
 *   optionally check for no duplicates
 */
SEXP colnamesInt(SEXP x, SEXP cols, SEXP check_dups) {
  if (!isNewList(x))
    error("'x' argument must be data.table compatible");
  if (!IS_TRUE_OR_FALSE(check_dups))
    error("'check_dups' argument must be TRUE or FALSE");
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
      if (!isRealReallyInt(cols))
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

/* coerceClass
 * coerce 'x' into same class as 'out', content of 'out' is ignored but attributes are copied
 * handle coercion between integer, numeric, integer64, all others are redirected to R's coerceVector
 */
SEXP coerceClass(SEXP x, SEXP out) {
  int protecti = 0;
  const bool verbose = GetVerbose();
  SEXP ans = R_NilValue;
  int64_t n = XLENGTH(x);

  bool to_integer = isInteger(out) || isLogical(out);
  bool to_real = isReal(out);
  bool to_longlong = to_real && (INHERITS(out,char_integer64) || INHERITS(out,char_nanotime));
  bool to_double = to_real && !to_longlong;
  bool from_integer = isInteger(x) || isLogical(x);
  bool from_real = isReal(x);
  bool from_longlong = from_real && (INHERITS(x,char_integer64) || INHERITS(x,char_nanotime));
  bool from_double = from_real && !from_longlong;

  if (to_integer) {
    if (!from_integer && !from_real) {
      if (verbose)
        Rprintf("%s: unknown to integer using coerceVector\n", __func__);
      ans = PROTECT(coerceVector(x, INTSXP)); protecti++;
    } else {
      ans = PROTECT(allocVector(INTSXP, n)); protecti++;
      int *ansP = INTEGER(ans);
      if (from_integer) {
        if (verbose)
          Rprintf("%s: integer to integer using memcpy\n", __func__);
        int *xP = INTEGER(x);
        memcpy(ansP, xP, sizeof(*ansP)*n);
      } else if (from_real) {
        if (from_longlong) {
          if (verbose)
            Rprintf("%s: long long to integer using loop\n", __func__);
          long long *xP = (long long *)REAL(x);
          for (int64_t i=0; i<n; i++) {
            ansP[i] = (xP[i]==NA_INT64_LL || (xP[i]>INT_MAX || xP[i]<INT_MIN)) ? NA_INTEGER : (int)xP[i];
          }
        } else if (from_double) {
          if (verbose)
            Rprintf("%s: double to integer using loop\n", __func__);
          double *xP = REAL(x);
          for (int64_t i=0; i<n; i++) {
            ansP[i] = (ISNAN(xP[i]) || (xP[i]>INT_MAX || xP[i]<INT_MIN)) ? NA_INTEGER : (int)xP[i];
          }
        } else {
          error("internal error in %s: real to integer, but real is not double and is not long long, should be one of them", __func__); // # nocov
        }
      } else {
        error("internal error in %s: this branch should not be reached", __func__); // # nocov
      }
    }
  } else if (to_real) {
    if (!from_integer && !from_real) {
      if (verbose) {
        if (to_double) {
          Rprintf("%s: unknown to double using coerceVector\n", __func__);
        } else if (to_longlong) {
          Rprintf("%s: unknown to long long using coerceVector\n", __func__);
        } else {
          error("internal error in %s: unknown to real, but real is not double and is not long long, should be one of them", __func__); // # nocov
        }
      }
      ans = PROTECT(coerceVector(x, REALSXP)); protecti++;
    } else {
      ans = PROTECT(allocVector(REALSXP, n)); protecti++;
      if (from_integer) {
        int *xP = INTEGER(x);
        if (to_double) {
          if (verbose)
            Rprintf("%s: integer to double using loop\n", __func__);
          double *ansP = REAL(ans);
          for (int64_t i=0; i<n; i++) {
            ansP[i] = xP[i]==NA_INTEGER ? NA_REAL : (double)xP[i];
          }
        } else if (to_longlong) {
          if (verbose)
            Rprintf("%s: integer to long long using loop\n", __func__);
          long long *ansP = (long long *)REAL(ans);
          for (int64_t i=0; i<n; i++) {
            ansP[i] = xP[i]==NA_INTEGER ? NA_INT64_LL : (long long)xP[i];
          }
        } else {
          error("internal error in %s: integer to real, but real is not double and is not long long, should be one of them", __func__); // # nocov
        }
      } else if (from_real) {
        if (from_double) {
          double *xP = REAL(x);
          if (to_double) {
            if (verbose)
              Rprintf("%s: double to double using memcpy\n", __func__);
            double *ansP = REAL(ans);
            memcpy(ansP, xP, sizeof(*ansP)*n);
          } else if (to_longlong) {
            if (verbose)
              Rprintf("%s: double to long long using loop\n", __func__);
            long long *ansP = (long long *)REAL(ans);
            for (int64_t i=0; i<n; i++) {
              ansP[i] = ISNAN(xP[i]) ? NA_INT64_LL : (long long)xP[i];
            }
          } else {
            error("internal error in %s: double to real, but real is not double and is not long long, should be one of them", __func__); // # nocov
          }
        } else if (from_longlong) {
          long long *xP = (long long *)REAL(x);
          if (to_double) {
            if (verbose)
              Rprintf("%s: long long to double using loop\n", __func__);
            double *ansP = REAL(ans);
            for (int64_t i=0; i<n; i++) {
              ansP[i] = xP[i]==NA_INT64_LL ? NA_REAL : (double)xP[i];
            }
          } else if (to_longlong) {
            if (verbose)
              Rprintf("%s: long long to long long using memcpy\n", __func__);
            long long *ansP = (long long *)REAL(ans);
            memcpy(ansP, xP, sizeof(*ansP)*n);
          } else {
            error("internal error in %s: long long to real, but real is not double and is not long long, should be one of them", __func__); // # nocov
          }
        } else {
          error("internal error in %s: real to real, but 'from' real is not double and is not long long, should be one of them", __func__); // # nocov
        }
      } else {
        error("internal error in %s: this branch should not be reached", __func__); // # nocov
      }
    }
  } else {
    if (verbose)
      Rprintf("%s: unknown to unknown using coerceVector\n", __func__);
    ans = PROTECT(coerceVector(x, TYPEOF(out))); protecti++;
  }

  copyMostAttrib(out, ans);
  UNPROTECT(protecti);
  return ans;
}
/* coerceClassR
 * vectorized coerceClass: taking list into 'out' and returning list of coerced objects, so 'x' can be coerced to different classes at once
 */
SEXP coerceClassR(SEXP x, SEXP out) {
  if (!isVectorAtomic(x))
    error("'x' argument must be atomic type");
  int protecti = 0;
  SEXP _out = out;
  if (isVectorAtomic(out)) {
    out = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(out, 0, _out);
  }
  if (!isNewList(out) || !length(out))
    error("'out' argument must be atomic type or a non-empty list");

  R_len_t n = LENGTH(out);
  SEXP ans = PROTECT(allocVector(VECSXP, n)); protecti++;
  for (int i=0; i<n; i++) {
    SET_VECTOR_ELT(ans, i, coerceClass(x, VECTOR_ELT(out, i)));
  }
  UNPROTECT(protecti);
  return isVectorAtomic(_out) && n==1 ? VECTOR_ELT(ans, 0) : ans;
}
