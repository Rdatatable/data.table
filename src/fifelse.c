#include "data.table.h"

SEXP fifelseR(SEXP l, SEXP a, SEXP b, SEXP na) {
  if (!isLogical(l))
    error("Argument 'test' must be logical.");
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

  if (len1!=1 && len1!=len0)
    error("Length of 'yes' is %"PRId64" but must be 1 or length of 'test' (%"PRId64").", len1, len0);
  if (len2!=1 && len2!=len0)
    error("Length of 'no' is %"PRId64" but must be 1 or length of 'test' (%"PRId64").", len2, len0);
  const int64_t amask = len1>1 ? INT64_MAX : 0; // for scalar 'a' bitwise AND will reset iterator to first element: pa[i & amask] -> pa[0]
  const int64_t bmask = len2>1 ? INT64_MAX : 0;

  const int *restrict pl = LOGICAL(l);
  SEXP ans = PROTECT(allocVector(ta, len0)); nprotect++;
  copyMostAttrib(a, ans);

  bool nonna = !isNull(na);
  if (nonna) {
    if (xlength(na) != 1)
      error("Length of 'na' is %"PRId64" but must be 1", (int64_t)xlength(na));
    SEXPTYPE tn = TYPEOF(na);
    if (tn == LGLSXP && LOGICAL(na)[0]==NA_LOGICAL) {
      nonna = false;
    } else {
      if (tn != ta)
        error("'yes' is of type %s but 'na' is of type %s. Please make sure that both arguments have the same type.", type2char(ta), type2char(tn));
      if (!R_compute_identical(PROTECT(getAttrib(a,R_ClassSymbol)), PROTECT(getAttrib(na,R_ClassSymbol)), 0))
        error("'yes' has different class than 'na'. Please make sure that both arguments have the same class.");
      UNPROTECT(2);
      if (isFactor(a)) {
        if (!R_compute_identical(PROTECT(getAttrib(a,R_LevelsSymbol)), PROTECT(getAttrib(na,R_LevelsSymbol)), 0))
          error("'yes' and 'na' are both type factor but their levels are different.");
        UNPROTECT(2);
      }
    }
  }

  switch(ta) {
  case LGLSXP: {
    int *restrict pans = LOGICAL(ans);
    const int *restrict pa   = LOGICAL(a);
    const int *restrict pb   = LOGICAL(b);
    const int pna = nonna ? LOGICAL(na)[0] : NA_LOGICAL;
    #pragma omp parallel for num_threads(getDTthreads())
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : pna);
    }
  } break;
  case INTSXP: {
    int *restrict pans = INTEGER(ans);
    const int *restrict pa   = INTEGER(a);
    const int *restrict pb   = INTEGER(b);
    const int pna = nonna ? INTEGER(na)[0] : NA_INTEGER;
    #pragma omp parallel for num_threads(getDTthreads())
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : pna);
    }
  } break;
  case REALSXP: {
    double *restrict pans = REAL(ans);
    const double *restrict pa   = REAL(a);
    const double *restrict pb   = REAL(b);
    const double na_double = Rinherits(a, char_integer64) ? NA_INT64_D : NA_REAL; // Rinherits() is true for nanotime
    const double pna = nonna ? REAL(na)[0] : na_double;
    #pragma omp parallel for num_threads(getDTthreads())
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : pna);
    }
  } break;
  case STRSXP : {
    const SEXP *restrict pa = STRING_PTR(a);
    const SEXP *restrict pb = STRING_PTR(b);
    const SEXP pna = nonna ? STRING_PTR(na)[0] : NA_STRING;
    for (int64_t i=0; i<len0; ++i) {
      SET_STRING_ELT(ans, i, pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : pna));
    }
  } break;
  case CPLXSXP : {
    Rcomplex *restrict pans = COMPLEX(ans);
    const Rcomplex *restrict pa   = COMPLEX(a);
    const Rcomplex *restrict pb   = COMPLEX(b);
    const Rcomplex pna = nonna ? COMPLEX(na)[0] : NA_CPLX;
    #pragma omp parallel for num_threads(getDTthreads())
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : pna);
    }
  } break;
  case VECSXP : {
    const SEXP *restrict pa = VECTOR_PTR(a);
    const SEXP *restrict pb = VECTOR_PTR(b);
    const SEXP *restrict pna = VECTOR_PTR(na);
    for (int64_t i=0; i<len0; ++i) {
      if (pl[i]==NA_LOGICAL) {
        if (nonna)
          SET_VECTOR_ELT(ans, i, pna[0]);
        continue; // allocVector already initialized with R_NilValue
      }
      SET_VECTOR_ELT(ans, i, pl[i]==0 ? pb[i & bmask] : pa[i & amask]);
    }
  } break;
  default:
    error("Type %s is not supported.", type2char(ta));
  }

  SEXP l_names = PROTECT(getAttrib(l, R_NamesSymbol)); nprotect++;
  if (!isNull(l_names))
    setAttrib(ans, R_NamesSymbol, l_names);

  UNPROTECT(nprotect);
  return ans;
}
