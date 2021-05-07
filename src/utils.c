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

bool allNA(SEXP x, bool errorForBadType) {
  // less space and time than all(is.na(x)) at R level because that creates full size is.na(x) first before all()
  // whereas this allNA can often return early on testing the first value without reading the rest
  const int n = length(x);
  if (n==0) // empty vectors (including raw(), NULL, and list()) same as R's all(is.na()) true result; tests 2116.*
    return true;
  switch (TYPEOF(x)) {
  case RAWSXP: // raw doesn't support NA so always false (other than length 0 case above)
    return false;
  case LGLSXP:
  case INTSXP: {
    const int *xd = INTEGER(x);
    for (int i=0; i<n; ++i)    if (xd[i]!=NA_INTEGER) {
      return false;
    }
    return true;
  }
  case REALSXP:
    if (Rinherits(x,char_integer64)) {
      const int64_t *xd = (int64_t *)REAL(x);
      for (int i=0; i<n; ++i)  if (xd[i]!=NA_INTEGER64) {
        return false;
      }
    } else {
      const double *xd = REAL(x);
      for (int i=0; i<n; ++i)  if (!ISNAN(xd[i])) {
        return false;
      }
    }
    return true;
  case CPLXSXP: {
    const Rcomplex *xd = COMPLEX(x);
    for (int i=0; i<n; ++i) if (!ISNAN_COMPLEX(xd[i])) { 
      return false;
    }
    return true;
  }
  case STRSXP: {
    const SEXP *xd = STRING_PTR(x);
    for (int i=0; i<n; ++i)    if (xd[i]!=NA_STRING) {
      return false;
    }
    return true;
  }}
  if (!errorForBadType) return false;
  error(_("Unsupported type '%s' passed to allNA()"), type2char(TYPEOF(x)));  // e.g. VECSXP; tests 2116.16-18
  // turned off allNA list support for now to avoid accidentally using it internally where we did not intend; allNA not yet exported
  //   https://github.com/Rdatatable/data.table/pull/3909#discussion_r329065950
}

SEXP allNAR(SEXP x) {
  return ScalarLogical(allNA(x, /*errorForBadType=*/true));
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
    error(_("'x' argument must be data.table compatible"));
  if (!IS_TRUE_OR_FALSE(check_dups))
    error(_("'check_dups' argument must be TRUE or FALSE"));
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
        error(_("argument specifying columns is type 'double' and one or more items in it are not whole integers"));
      ricols = PROTECT(coerceVector(cols, INTSXP)); protecti++;
    }
    int *icols = INTEGER(ricols);
    for (int i=0; i<nc; i++) {
      if ((icols[i]>nx) || (icols[i]<1))
        error(_("argument specifying columns specify non existing column(s): cols[%d]=%d"), i+1, icols[i]); // handles NAs also
    }
  } else if (isString(cols)) {
    SEXP xnames = PROTECT(getAttrib(x, R_NamesSymbol)); protecti++;
    if (isNull(xnames))
      error(_("'x' argument data.table has no names"));
    ricols = PROTECT(chmatch(cols, xnames, 0)); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<nc; i++) {
      if (icols[i]==0)
        error(_("argument specifying columns specify non existing column(s): cols[%d]='%s'"), i+1, CHAR(STRING_ELT(cols, i))); // handles NAs also
    }
  } else {
    error(_("argument specifying columns must be character or numeric"));
  }
  if (LOGICAL(check_dups)[0] && any_duplicated(ricols, FALSE))
    error(_("argument specifying columns specify duplicated column(s)"));
  UNPROTECT(protecti);
  return ricols;
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
  // Rinherits prefix with 'R' to signify i) it may call R level and is therefore not thread safe, and ii) includes R level inherits which covers S4.
  bool ans = INHERITS(x, char_);        // try standard S3 class character vector first
  if (!ans && char_==char_integer64)    // save the eval() for known S4 classes that inherit from integer64
    ans = INHERITS(x, char_nanotime);   // comment this out to test the eval() works for nanotime
  if (!ans && IS_S4_OBJECT(x)) {        // if it's not S4 we can save the overhead of R eval()
    SEXP vec = PROTECT(ScalarString(char_));           // TODO: cover this branch by making two new test S4 classes: one that
    SEXP call = PROTECT(lang3(sym_inherits, x, vec));  //       does inherit from integer64 and one that doesn't
    ans = LOGICAL(eval(call, R_GlobalEnv))[0]==1;
    UNPROTECT(2);
  }
  return ans;
}

SEXP copyAsPlain(SEXP x) {
  // v1.12.2 and before used standard R duplicate() to do this. But duplicate() is not guaranteed to not return an ALTREP.
  // e.g. ALTREP 'wrapper' on factor column (with materialized INTSXP) in package VIM under example(hotdeck)
  //      .Internal(inspect(x[[5]]))
  //      @558adf4d9508 13 INTSXP g0c0 [OBJ,NAM(7),ATT]  wrapper [srt=-2147483648,no_na=0]
  // 'AsPlain' is intended to convey unALTREP-ing; i.e. materializing and removing any ALTREP wrappers/attributes
  // For non-ALTREP this should do the same as R's duplicate().
  // Intended for use on columns; to either un-ALTREP them or duplicate shared memory columns; see copySharedColumns() below
  // Not intended to be called on a DT VECSXP where a concept of 'deep' might refer to whether the columns are copied
  
  if (isNull(x)) {
    // deal with up front because isNewList(R_NilValue) is true
    return R_NilValue;
  }
  if (!isVectorAtomic(x) && !isNewList(x)) {
    // e.g. defer to R the CLOSXP in test 173.3 where a list column item is the function 'mean'
    return duplicate(x);
  }
  const int64_t n = XLENGTH(x);
  SEXP ans = PROTECT(allocVector(TYPEOF(x), n));
  switch (TYPEOF(x)) {
  case RAWSXP:
    memcpy(RAW(ans),     RAW(x),     n*sizeof(Rbyte));
    break;
  case LGLSXP:
    memcpy(LOGICAL(ans), LOGICAL(x), n*sizeof(Rboolean));
    break;
  case INTSXP:
    memcpy(INTEGER(ans), INTEGER(x), n*sizeof(int));             // covered by 10:1 after test 178
    break;
  case REALSXP:
    memcpy(REAL(ans),    REAL(x),    n*sizeof(double));          // covered by as.Date("2013-01-01")+seq(1,1000,by=10) after test 1075
    break;
  case CPLXSXP:
    memcpy(COMPLEX(ans), COMPLEX(x), n*sizeof(Rcomplex));
    break;
  case STRSXP: {
    const SEXP *xp=STRING_PTR(x);                                // covered by as.character(as.hexmode(1:500)) after test 642
    for (int64_t i=0; i<n; ++i) SET_STRING_ELT(ans, i, xp[i]);
  } break;
  case VECSXP: {
    const SEXP *xp=SEXPPTR_RO(x);
    for (int64_t i=0; i<n; ++i) SET_VECTOR_ELT(ans, i, copyAsPlain(xp[i]));
  } break;
  default:                                                                                           // # nocov
    error(_("Internal error: unsupported type '%s' passed to copyAsPlain()"), type2char(TYPEOF(x))); // # nocov
  }
  DUPLICATE_ATTRIB(ans, x);
  // aside: unlike R's duplicate we do not copy truelength here; important for dogroups.c which uses negative truelenth to mark its specials
  if (ALTREP(ans))
    error(_("Internal error: copyAsPlain returning ALTREP for type '%s'"), type2char(TYPEOF(x))); // # nocov
  UNPROTECT(1);
  return ans;
}

void copySharedColumns(SEXP x) {
  const int ncol = length(x);
  if (!isNewList(x) || ncol==1) return;
  bool *shared = (bool *)R_alloc(ncol, sizeof(bool)); // on R heap in case alloc fails
  int *savetl = (int *)R_alloc(ncol, sizeof(int));  // on R heap for convenience but could be a calloc
  const SEXP *xp = SEXPPTR_RO(x);
  // first save the truelength, which may be negative on specials in dogroups, and set to zero; test 2157
  // the savetl() function elsewhere is for CHARSXP. Here, we are using truelength on atomic vectors.
  for (int i=0; i<ncol; ++i) {
    const SEXP thiscol = xp[i];
    savetl[i] = ALTREP(thiscol) ? 0 : TRUELENGTH(thiscol);
    SET_TRUELENGTH(thiscol, 0);
  } 
  int nShared=0;
  for (int i=0; i<ncol; ++i) {
    SEXP thiscol = xp[i];
    if (ALTREP(thiscol) || TRUELENGTH(thiscol)<0) {
      shared[i] = true;  // we mark ALTREP as 'shared' too, whereas 'tocopy' would be better word to use for ALTREP
      nShared++;
      // do not copyAsPlain() here yet, as its alloc might fail. Must restore tl first to all columns before attempting any copies.
    } else {
      shared[i] = false;              // so the first column will never be shared (unless it is an altrep) even it is shared
                                      // 'shared' means a later column shares an earlier column
      SET_TRUELENGTH(thiscol, -i-1);  // -i-1 so that if, for example, column 3 shares column 1, in iteration 3 we'll know not
                                      // only that the 3rd column is shared with an earlier column, but which one too. Although
                                      // we don't use that information currently, we could do in future.  
    }
  }
  // now we know nShared and which ones they are (if any), restore original tl back to the unique set of columns
  for (int i=0; i<ncol; ++i) {
    if (!shared[i]) SET_TRUELENGTH(xp[i], savetl[i]);
    //  ^^^^^^^^^^ important because if there are shared columns, the dup will have savetl==0 but we want the first restore to stand
  }
  // now that truelength has been restored for all columns, we can finally call copyAsPlain()
  if (nShared) {
    for (int i=0; i<ncol; ++i) {
      if (shared[i])
        SET_VECTOR_ELT(x, i, copyAsPlain(xp[i]));
    }
    if (GetVerbose()) Rprintf(_("Found and copied %d column%s with a shared memory address\n"), nShared, nShared>1?"s":"");
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

bool need2utf8(SEXP x) {
  const int xlen = length(x);
  SEXP *xd = STRING_PTR(x);
  for (int i=0; i<xlen; i++) {
    if (NEED2UTF8(xd[i]))
      return(true);
  }
  return(false);
}

SEXP coerceUtf8IfNeeded(SEXP x) {
  if (!need2utf8(x))
    return(x);
  const int xlen = length(x);
  SEXP ans = PROTECT(allocVector(STRSXP, xlen));
  SEXP *xd = STRING_PTR(x);
  for (int i=0; i<xlen; i++) {
    SET_STRING_ELT(ans, i, ENC2UTF8(xd[i]));
  }
  UNPROTECT(1);
  return(ans);
}

// class1 is used by coerseAs only, which is used by frollR.c and nafill.c only
const char *class1(SEXP x) {
  SEXP cl = getAttrib(x, R_ClassSymbol);
  if (length(cl))
    return(CHAR(STRING_ELT(cl, 0)));
  SEXP d = getAttrib(x, R_DimSymbol);
  int nd = length(d);
  if (nd) {
    if (nd==2)
      return "matrix";
    else
      return "array";
  }
  SEXPTYPE t = TYPEOF(x);
  // see TypeTable in src/main/utils.c to compare to the differences here vs type2char
  switch(t) {
  case CLOSXP: case SPECIALSXP: case BUILTINSXP:
    return "function";
  case REALSXP:
    return "numeric";
  case SYMSXP:
    return "name";
  case LANGSXP:
    return "call";
  default:
    return type2char(t);
  }
}

// main motivation for this function is to have coercion helper that is aware of int64 NAs, unline base R coerce #3913
SEXP coerceAs(SEXP x, SEXP as, SEXP copyArg) {
  // copyArg does not update in place, but only IF an object is of the same type-class as class to be coerced, it will return with no copy
  if (!isVectorAtomic(x))
    error("'x' is not atomic");
  if (!isVectorAtomic(as))
    error("'as' is not atomic");
  if (!isNull(getAttrib(x, R_DimSymbol)))
    error("'x' must not be matrix or array");
  if (!isNull(getAttrib(as, R_DimSymbol)))
    error("'as' must not be matrix or array");
  bool verbose = GetVerbose()>=2; // verbose level 2 required
  if (!LOGICAL(copyArg)[0] && TYPEOF(x)==TYPEOF(as) && class1(x)==class1(as)) {
    if (verbose)
      Rprintf("copy=false and input already of expected type and class %s[%s]\n", type2char(TYPEOF(x)), class1(x));
    copyMostAttrib(as, x); // so attrs like factor levels are same for copy=T|F
    return(x);
  }
  int len = LENGTH(x);
  SEXP ans = PROTECT(allocNAVectorLike(as, len));
  if (verbose)
    Rprintf("Coercing %s[%s] into %s[%s]\n", type2char(TYPEOF(x)), class1(x), type2char(TYPEOF(as)), class1(as));
  const char *ret = memrecycle(/*target=*/ans, /*where=*/R_NilValue, /*start=*/0, /*len=*/LENGTH(x), /*source=*/x, /*sourceStart=*/0, /*sourceLen=*/-1, /*colnum=*/0, /*colname=*/"");
  if (ret)
    warning(_("%s"), ret);
  UNPROTECT(1);
  return ans;
}

#ifndef NOZLIB
#include <zlib.h>
#endif
SEXP dt_zlib_version() {
  char out[51];
#ifndef NOZLIB
  snprintf(out, 50, "zlibVersion()==%s ZLIB_VERSION==%s", zlibVersion(), ZLIB_VERSION);
#else
  snprintf(out, 50, "zlib header files were not found when data.table was compiled");
#endif
  return ScalarString(mkChar(out));
}
