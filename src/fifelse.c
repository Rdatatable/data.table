#include "data.table.h"

SEXP fifelseR(SEXP l, SEXP a, SEXP b) {

  if (!isLogical(l)) error("Argument 'test' must be logical.");
  const int64_t len0 = xlength(l);
  const int64_t len1 = xlength(a);
  const int64_t len2 = xlength(b);
  SEXPTYPE ta = TYPEOF(a);
  SEXPTYPE tb = TYPEOF(b);
  int nprotect = 0;

  if (ta != tb) {
    if (ta == INTSXP && tb == REALSXP) {
      SEXP tmp = PROTECT(coerceVector(a, REALSXP)); nprotect++;
      a = tmp;
      ta = REALSXP;
    } else if (ta == REALSXP && tb == INTSXP) {
      SEXP tmp = PROTECT(coerceVector(b, REALSXP)); nprotect++;
      b = tmp;
      tb = REALSXP;
    } else {
      error("'yes' is of type %s but 'no' is of type %s. Please make sure that both arguments have the same type.", type2char(ta), type2char(tb));
    }
  }

  if (!R_compute_identical(PROTECT(getAttrib(a,R_ClassSymbol)), PROTECT(getAttrib(b,R_ClassSymbol)), 0))
    error("'yes' has different class than 'no'. Please make sure that both arguments have the same class.");
  UNPROTECT(2);

  if (isFactor(a)) {
    if (!R_compute_identical(PROTECT(getAttrib(a,R_LevelsSymbol)), PROTECT(getAttrib(b,R_LevelsSymbol)), 0))
      error("'yes' and 'no' are both type factor but their levels are different.");
    UNPROTECT(2);
  }

  if (len1!=1 && len1!=len0) error("Length of 'yes' is %lld but must be 1 or length of 'test' (%lld).", len1, len0);
  if (len2!=1 && len2!=len0) error("Length of 'no' is %lld but must be 1 or length of 'test' (%lld).", len2, len0);
  const int64_t amask = len1>1 ? INT64_MAX : 0; // for scalar 'a' bitwise AND will reset iterator to first element: pa[i & amask] -> pa[0]
  const int64_t bmask = len2>1 ? INT64_MAX : 0;

  const int *restrict pl = LOGICAL(l);
  SEXP ans = PROTECT(allocVector(TYPEOF(a), len0)); nprotect++;
  copyMostAttrib(a, ans);

  switch(TYPEOF(a)) {
  case LGLSXP: {
    int *restrict pans = LOGICAL(ans);
    const int *restrict pa   = LOGICAL(a);
    const int *restrict pb   = LOGICAL(b);
    #pragma omp parallel for num_threads(getDTthreads())
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : NA_LOGICAL);
    }
  } break;
  case INTSXP: {
    int *restrict pans = INTEGER(ans);
    const int *restrict pa   = INTEGER(a);
    const int *restrict pb   = INTEGER(b);
    #pragma omp parallel for num_threads(getDTthreads())
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : NA_INTEGER);
    }
  } break;
  case REALSXP: {
    double *restrict pans = REAL(ans);
    const double *restrict pa   = REAL(a);
    const double *restrict pb   = REAL(b);
    const double na_double = (INHERITS(a, char_integer64) || INHERITS(a, char_nanotime)) ? NA_INT64_D : NA_REAL; // integer64 and nanotime support
    #pragma omp parallel for num_threads(getDTthreads())
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : na_double);
    }
  } break;
  case STRSXP : {
    const SEXP *restrict pa = STRING_PTR(a);
    const SEXP *restrict pb = STRING_PTR(b);
    for (int64_t i=0; i<len0; ++i) {
      SET_STRING_ELT(ans, i, pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : NA_STRING));
    }
  } break;
  case CPLXSXP : {
    Rcomplex *restrict pans = COMPLEX(ans);
    const Rcomplex *restrict pa   = COMPLEX(a);
    const Rcomplex *restrict pb   = COMPLEX(b);
    #pragma omp parallel for num_threads(getDTthreads())
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : NA_CPLX);
    }
  } break;
  case VECSXP : {
    const SEXP *restrict pa = VECTOR_PTR(a);
    const SEXP *restrict pb = VECTOR_PTR(b);
    SEXP a_names = PROTECT(getAttrib(a,R_NamesSymbol)); nprotect++;
    SEXP b_names = PROTECT(getAttrib(b,R_NamesSymbol)); nprotect++;
    const SEXP *restrict pa_names = isNull(a_names) ? NULL : STRING_PTR(a_names);
    const SEXP *restrict pb_names = isNull(b_names) ? NULL : STRING_PTR(b_names);
    if (!pa_names && !pb_names) {
      for (int64_t i=0; i<len0; ++i) {
        if (pl[i]==NA_INTEGER) continue;  // allocVector already initialized with R_NilValue
        SET_VECTOR_ELT(ans, i, pl[i]==0 ? pb[i & bmask] : pa[i & amask]);
      }
    } else {
      SEXP ans_names = PROTECT(allocVector(STRSXP, len0)); nprotect++;
      setAttrib(ans, R_NamesSymbol, ans_names);
      for (int64_t i=0; i<len0; ++i) {
        if (pl[i]==NA_INTEGER) continue;  // allocVector already initialized with R_NilValue, and names already initialized with ""
        SET_VECTOR_ELT(ans, i, pl[i]==0 ? pb[i & bmask] : pa[i & amask]);
        if (pa_names && pl[i]==1) SET_STRING_ELT(ans_names, i, pa_names[i & amask]);
        if (pb_names && pl[i]==0) SET_STRING_ELT(ans_names, i, pb_names[i & bmask]);
      }
    }
  } break;
  default:
    error("Type %s is not supported.",type2char(TYPEOF(a)));
  }
  UNPROTECT(nprotect);
  return ans;
}

