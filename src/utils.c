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

inline bool INHERITS(SEXP x, SEXP char_) {
  // Thread safe inherits() by pre-calling install() in init.c and then
  // passing those char_* in here for simple and fast non-API pointer compare.
  // The thread-safety aspect here is only currently actually needed for list columns in
  // fwrite() where the class of the cell's vector is tested; the class of the column
  // itself is pre-stored by fwrite (for example in isInteger64[] and isITime[]).
  // Thread safe in the limited sense of correct and intended usage :
  // i) no API call such as install() or mkChar() must be passed in.
  // ii) no attrib writes must be possible in other threads.
  SEXP klass;
  if (isString(klass = getAttrib(x, R_ClassSymbol))) {
    for (int i=0; i<LENGTH(klass); i++) {
      if (STRING_ELT(klass, i) == char_) return true;
    }
  }
  return false;
}

bool Rinherits(SEXP x, SEXP char_) {
 // motivation was nanotime which is S4 and inherits from integer64 via S3 extends
 // R's C API inherits() does not cover S4 and returns FALSE for nanotime, as does our own INHERITS above.
 // R's R-level inherits() calls objects.c:inherits2 which calls attrib.c:R_data_class2 and
 // then attrib.c:S4_extends which itself calls R level methods:::.extendsForS3 which then calls R level methods::extends.
 // Since that chain of calls is so complicated and involves evaluating R level anyway, let's just reuse it.
 // Rinherits prefix with 'R' to signify i) it calls R level and is not thread safe, and ii) is the R level inherits which covers S4.
 SEXP vec = PROTECT(ScalarString(char_));
 SEXP call = PROTECT(lang3(sym_inherits, x, vec));
 bool ans = LOGICAL(eval(call, R_GlobalEnv))[0]==1;
 UNPROTECT(2);
 return ans;
}

void copySharedColumns(SEXP x) {
  const int ncol = length(x);
  if (!isNewList(x) || ncol==1) return;
  bool *shared = (bool *)R_alloc(ncol, sizeof(bool)); // on R heap in case duplicate() fails
  int *savetl = (int *)R_alloc(ncol, sizeof(int));  // on R heap for convenience but could be a calloc
  int nShared = 0;
  const SEXP *xp = VECTOR_PTR(x);
  for (int i=0; i<ncol; ++i) {
    SEXP thiscol = xp[i];
    const int thistl = TRUELENGTH(thiscol);
    if (thistl<0) {
      shared[i] = true;
      nShared++;
      // do not duplicate() here as the duplicate() might fail. Careful to restore tl first to all columns.
      // Aside: thistl is which column shares the same address as this one in case that's ever useful in future.
    } else {
      shared[i] = false;
      savetl[i] = thistl;  // these are vectors which are all expected to have tl, unlike CHARSXP which often don't (savetl() has CHARSXP in mind)
      SET_TRUELENGTH(thiscol, -i-1);
    }
  }
  // now we know nShared and which ones they are (if any), restore original tl back to all columns
  for (int i=0; i<ncol; ++i) {
    if (!shared[i]) SET_TRUELENGTH(VECTOR_ELT(x, i), savetl[i]);
  }
  // now that truelength has been restored for all columns, we can finally call duplicate()
  if (nShared) {
    for (int i=0; i<ncol; ++i) {
      if (shared[i])
        SET_VECTOR_ELT(x, i, duplicate(VECTOR_ELT(x, i)));
    }
    if (GetVerbose()) Rprintf("Found and copied %d column%s with a shared memory address\n", nShared, nShared>1?"s":"");
    // GetVerbose() (slightly expensive call of all options) called here only when needed
  }
}

// lock, unlock and islocked at C level :
// 1) for speed to reduce overhead
// 2) to avoid an R level wrapper which bumps MAYBE_SHARED; see the unlock after eval(jval) in data.table.R, #1341 #2245
SEXP lock(SEXP DT) {
  setAttrib(DT, sym_datatable_locked, ScalarLogical(TRUE));
  return DT;
}
SEXP unlock(SEXP DT) {
  setAttrib(DT, sym_datatable_locked, R_NilValue);
  return DT;
}
bool islocked(SEXP DT) {
  SEXP att = getAttrib(DT, sym_datatable_locked);
  return isLogical(att) && LENGTH(att)==1 && LOGICAL(att)[0]==1;
}
SEXP islockedR(SEXP DT) {
  return ScalarLogical(islocked(DT));
}

