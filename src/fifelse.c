#include "data.table.h"

SEXP fifelseR(SEXP l, SEXP a, SEXP b, SEXP na) {
  if (!isLogical(l)) {
    error(_("Argument 'test' must be logical."));
  }
  if (  (isS4(a) && !INHERITS(a, char_nanotime))
     || (isS4(b) && !INHERITS(b, char_nanotime)) ) {
    error("S4 class objects (except nanotime) are not supported.");
  }
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
      error(_("'yes' is of type %s but 'no' is of type %s. Please make sure that both arguments have the same type."), type2char(ta), type2char(tb));
    }
  }

  if (!R_compute_identical(PROTECT(getAttrib(a,R_ClassSymbol)), PROTECT(getAttrib(b,R_ClassSymbol)), 0))
    error(_("'yes' has different class than 'no'. Please make sure that both arguments have the same class."));
  UNPROTECT(2);

  if (isFactor(a)) {
    if (!R_compute_identical(PROTECT(getAttrib(a,R_LevelsSymbol)), PROTECT(getAttrib(b,R_LevelsSymbol)), 0))
      error(_("'yes' and 'no' are both type factor but their levels are different."));
    UNPROTECT(2);
  }

  if (len1!=1 && len1!=len0)
    error(_("Length of 'yes' is %"PRId64" but must be 1 or length of 'test' (%"PRId64")."), len1, len0);
  if (len2!=1 && len2!=len0)
    error(_("Length of 'no' is %"PRId64" but must be 1 or length of 'test' (%"PRId64")."), len2, len0);
  const int64_t amask = len1>1 ? INT64_MAX : 0; // for scalar 'a' bitwise AND will reset iterator to first element: pa[i & amask] -> pa[0]
  const int64_t bmask = len2>1 ? INT64_MAX : 0;

  const int *restrict pl = LOGICAL(l);
  SEXP ans = PROTECT(allocVector(ta, len0)); nprotect++;
  copyMostAttrib(a, ans);

  bool nonna = !isNull(na);
  if (nonna) {
    if (xlength(na) != 1)
      error(_("Length of 'na' is %"PRId64" but must be 1"), (int64_t)xlength(na));
    SEXPTYPE tn = TYPEOF(na);
    if (tn == LGLSXP && LOGICAL(na)[0]==NA_LOGICAL) {
      nonna = false;
    } else {
      if (tn != ta)
        error(_("'yes' is of type %s but 'na' is of type %s. Please make sure that both arguments have the same type."), type2char(ta), type2char(tn));
      if (!R_compute_identical(PROTECT(getAttrib(a,R_ClassSymbol)), PROTECT(getAttrib(na,R_ClassSymbol)), 0))
        error(_("'yes' has different class than 'na'. Please make sure that both arguments have the same class."));
      UNPROTECT(2);
      if (isFactor(a)) {
        if (!R_compute_identical(PROTECT(getAttrib(a,R_LevelsSymbol)), PROTECT(getAttrib(na,R_LevelsSymbol)), 0))
          error(_("'yes' and 'na' are both type factor but their levels are different."));
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
    #pragma omp parallel for num_threads(getDTthreads(len0, true))
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : pna);
    }
  } break;
  case INTSXP: {
    int *restrict pans = INTEGER(ans);
    const int *restrict pa   = INTEGER(a);
    const int *restrict pb   = INTEGER(b);
    const int pna = nonna ? INTEGER(na)[0] : NA_INTEGER;
    #pragma omp parallel for num_threads(getDTthreads(len0, true))
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
    #pragma omp parallel for num_threads(getDTthreads(len0, true))
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
    #pragma omp parallel for num_threads(getDTthreads(len0, true))
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ? pb[i & bmask] : (pl[i]==1 ? pa[i & amask] : pna);
    }
  } break;
  case VECSXP : {
    const SEXP *restrict pa = SEXPPTR_RO(a);
    const SEXP *restrict pb = SEXPPTR_RO(b);
    const SEXP *restrict pna = SEXPPTR_RO(na);
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
    error(_("Type %s is not supported."), type2char(ta));
  }

  SEXP l_names = PROTECT(getAttrib(l, R_NamesSymbol)); nprotect++;
  if (!isNull(l_names))
    setAttrib(ans, R_NamesSymbol, l_names);

  UNPROTECT(nprotect);
  return ans;
}

SEXP fcaseR(SEXP na, SEXP rho, SEXP args) {
  int n=length(args);
  if (n % 2) {
    error(_("Received %d inputs; please supply an even number of arguments in ..., "
            "consisting of logical condition, resulting value pairs (in that order). "
            "Note that the default argument must be named explicitly, e.g., default=0"), n);
  }
  int nprotect = 0, l = 0;
  int64_t len0=0, len1=0, len2=0, idx=0;
  SEXP ans = R_NilValue, value0 = R_NilValue, tracker = R_NilValue, cons = R_NilValue, outs = R_NilValue;
  PROTECT_INDEX Icons, Iouts;
  PROTECT_WITH_INDEX(cons, &Icons); nprotect++;
  PROTECT_WITH_INDEX(outs, &Iouts); nprotect++;
  SEXPTYPE type0;
  bool nonna = !isNull(na), imask = true;
  int *restrict p = NULL;
  n = n/2;
  for (int i=0; i<n; ++i) {
    REPROTECT(cons = eval(SEXPPTR_RO(args)[2*i], rho), Icons);
    REPROTECT(outs = eval(SEXPPTR_RO(args)[2*i+1], rho), Iouts);
    if (isS4(outs) && !INHERITS(outs, char_nanotime)) {
      error("S4 class objects (except nanotime) are not supported. Please see https://github.com/Rdatatable/data.table/issues/4131.");
    }
    if (!isLogical(cons)) {
      error("Argument #%d must be logical.", 2*i+1);
    }
    const int *restrict pcons = LOGICAL(cons);
    if (i == 0) {
      len0 = xlength(cons);
      len2 = len0;
      type0 = TYPEOF(outs);
      value0 = outs;
      if (nonna) {
        if (xlength(na) != 1) {
          error("Length of 'default' must be 1.");
        }
        SEXPTYPE tn = TYPEOF(na);
        if (tn == LGLSXP && LOGICAL(na)[0]==NA_LOGICAL) {
          nonna = false;
        } else {
          if (tn != type0) {
            error("Resulting value is of type %s but 'default' is of type %s. "
                     "Please make sure that both arguments have the same type.", type2char(type0), type2char(tn));
          }
          if (!R_compute_identical(PROTECT(getAttrib(outs,R_ClassSymbol)),  PROTECT(getAttrib(na,R_ClassSymbol)), 0)) {
            error("Resulting value has different class than 'default'. "
                     "Please make sure that both arguments have the same class.");
          }
          UNPROTECT(2);
          if (isFactor(outs)) {
            if (!R_compute_identical(PROTECT(getAttrib(outs,R_LevelsSymbol)),  PROTECT(getAttrib(na,R_LevelsSymbol)), 0)) {
              error("Resulting value and 'default' are both type factor but their levels are different.");
            }
            UNPROTECT(2);
          }
        }
      }
      ans = PROTECT(allocVector(type0, len0)); nprotect++;
      tracker = PROTECT(allocVector(INTSXP, len0)); nprotect++;
      p = INTEGER(tracker);
      copyMostAttrib(outs, ans);
    } else {
      imask = false;
      l = 0;
      if (xlength(cons) != len0) {
        error("Argument #%d has a different length than argument #1. "
                 "Please make sure all logical conditions have the same length.",
                 i*2+1);
      }
      if (TYPEOF(outs) != type0) {
        error("Argument #%d is of type %s, however argument #2 is of type %s. "
                 "Please make sure all output values have the same type.",
                 i*2+2, type2char(TYPEOF(outs)), type2char(type0));
      }
      if (!R_compute_identical(PROTECT(getAttrib(value0,R_ClassSymbol)),  PROTECT(getAttrib(outs,R_ClassSymbol)), 0)) {
        error("Argument #%d has different class than argument #2, "
                 "Please make sure all output values have the same class.", i*2+2);
      }
      UNPROTECT(2);
      if (isFactor(value0)) {
        if (!R_compute_identical(PROTECT(getAttrib(value0,R_LevelsSymbol)),  PROTECT(getAttrib(outs,R_LevelsSymbol)), 0)) {
          error("Argument #2 and argument #%d are both factor but their levels are different.", i*2+2);
        }
        UNPROTECT(2);
      }
    }
    len1 = xlength(outs);
    if (len1 != len0 && len1 != 1) {
      error("Length of output value #%d must either be 1 or length of logical condition.", i*2+2);
    }
    int64_t amask = len1>1 ? INT64_MAX : 0;
    switch(TYPEOF(outs)) {
    case LGLSXP: {
      const int *restrict pouts = LOGICAL(outs);
      int *restrict pans = LOGICAL(ans);
      const int pna = nonna ? LOGICAL(na)[0] : NA_LOGICAL;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          pans[idx] = pouts[idx & amask];
        } else {
          if (imask) {
            pans[j] = pna;
          }
          p[l++] = idx;
        }
      }
    } break;
    case INTSXP: {
      const int *restrict pouts = INTEGER(outs);
      int *restrict pans = INTEGER(ans);
      const int pna = nonna ? INTEGER(na)[0] : NA_INTEGER;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          pans[idx] = pouts[idx & amask];
        } else {
          if (imask) {
            pans[j] = pna;
          }
          p[l++] = idx;
        }
      }
    } break;
    case REALSXP: {
      const double *restrict pouts = REAL(outs);
      double *restrict pans = REAL(ans);
      const double na_double = Rinherits(outs, char_integer64) ? NA_INT64_D : NA_REAL;
      const double pna = nonna ? REAL(na)[0] : na_double;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          pans[idx] = pouts[idx & amask];
        } else {
          if (imask) {
            pans[j] = pna;
          }
          p[l++] = idx;
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict pouts = COMPLEX(outs);
      Rcomplex *restrict pans = COMPLEX(ans);
      const Rcomplex pna = nonna ? COMPLEX(na)[0] : NA_CPLX;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          pans[idx] = pouts[idx & amask];
        } else {
          if (imask) {
            pans[j] = pna;
          }
          p[l++] = idx;
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict pouts = STRING_PTR(outs);
      const SEXP pna = nonna ? STRING_PTR(na)[0] : NA_STRING;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          SET_STRING_ELT(ans, idx, pouts[idx & amask]);
        } else {
          if (imask) {
            SET_STRING_ELT(ans, idx, pna);
          }
          p[l++] = idx;
        }
      }
    } break;
    case VECSXP: {
      const SEXP *restrict pouts = SEXPPTR_RO(outs);
      const SEXP pna = SEXPPTR_RO(na)[0];
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx]==1) {
          SET_VECTOR_ELT(ans, idx, pouts[idx & amask]);
        } else {
          if (imask && nonna) {
            SET_VECTOR_ELT(ans, idx, pna);
          }
          p[l++] = idx;
        }
      }
    } break;
    default:
      error("Type %s is not supported.", type2char(TYPEOF(outs)));
    }
    if (l==0) {
      break;
    }
    len2 = l;
  }
  UNPROTECT(nprotect);
  return ans;
}
