#include "data.table.h"

SEXP fifelseR(SEXP l, SEXP a, SEXP b, SEXP na) {
  if (!isLogical(l)) {
    error(_("Argument 'test' must be logical."));
  }
  if (  (isS4(a) && !INHERITS(a, char_nanotime))
     || (isS4(b) && !INHERITS(b, char_nanotime)) ) {
    error(_("S4 class objects (except nanotime) are not supported."));
  }
  const int64_t len0 = xlength(l);
  const int64_t len1 = xlength(a);
  const int64_t len2 = xlength(b);
  const int64_t len3 = xlength(na);
  SEXPTYPE ta = TYPEOF(a);
  SEXPTYPE tb = TYPEOF(b);
  SEXPTYPE tn = TYPEOF(na);
  // na_a/b/n means a scalar NA (or NULL for the na argument), which is considered to be coerced into other types
  bool na_a = len1==1 && ta==LGLSXP && LOGICAL(a)[0]==NA_LOGICAL;
  bool na_b = len2==1 && tb==LGLSXP && LOGICAL(b)[0]==NA_LOGICAL;
  bool na_n = isNull(na) || (len3==1 && tn==LGLSXP && LOGICAL(na)[0]==NA_LOGICAL);

  if (!na_a && len1!=1 && len1!=len0)
    error(_("Length of 'yes' is %"PRId64" but must be 1 or length of 'test' (%"PRId64")."), len1, len0);
  if (!na_b && len2!=1 && len2!=len0)
    error(_("Length of 'no' is %"PRId64" but must be 1 or length of 'test' (%"PRId64")."), len2, len0);
  if (!na_n && len3!=1 && len3!=len0)
    error(_("Length of 'na' is %"PRId64" but must be 1 or length of 'test' (%"PRId64")."), len3, len0);
  
  int nprotect = 0;
  SEXPTYPE tans = !na_a ? ta : !na_b ? tb : !na_n ? tn : LGLSXP;
  if (!(na_a && na_b && na_n)) {
    SEXPTYPE ta0 = ta, tb0 = tb, tn0 = tn; // record the original type for error message use
    if (!na_b && tans==INTSXP && tb==REALSXP) tans = tb;
    if (!na_n && tans==INTSXP && tn==REALSXP) tans = tn;
    if (!na_a && tans==REALSXP && ta==INTSXP) {
      a = PROTECT(coerceVector(a, REALSXP)); nprotect++;
      ta = REALSXP;
    }
    // it's not possible that non-NA `yes`' type will be different from `tans`
    if (!na_b && tans==REALSXP && tb==INTSXP) {
      b = PROTECT(coerceVector(b, REALSXP)); nprotect++;
      tb = REALSXP;
    }
    if (!na_b && tans != tb) 
      error(_("'no' is of type %s but '%s' is %s. Please make all arguments have the same type."), type2char(tb0), tans==ta0 ? "yes" : "na", tans==ta0 ? type2char(ta0) : type2char(tn0));
    if (!na_n && tans==REALSXP && tn==INTSXP) {
      na = PROTECT(coerceVector(na, REALSXP)); nprotect++;
      tn = REALSXP;
    }
    if (!na_n && tans != tn) 
      error(_("'na' is of type %s but '%s' is %s. Please make all arguments have the same type."), type2char(tn0), tans==ta0 ? "yes" : "no", tans==ta0 ? type2char(ta0) : type2char(tb0));
  }
  
  if (!na_a && !na_b) {
    if (!R_compute_identical(PROTECT(getAttrib(a,R_ClassSymbol)), PROTECT(getAttrib(b,R_ClassSymbol)), 0))
      error(_("'yes' has different class than 'no'. Please make sure that both arguments have the same class."));
    UNPROTECT(2);
  }
  if (!na_a && !na_n) {
    if (!R_compute_identical(PROTECT(getAttrib(a,R_ClassSymbol)), PROTECT(getAttrib(na,R_ClassSymbol)), 0))
      error(_("'yes' has different class than 'na'. Please make sure that both arguments have the same class."));
    UNPROTECT(2);
  }
  if (!na_b && !na_n) {
    if (!R_compute_identical(PROTECT(getAttrib(b,R_ClassSymbol)), PROTECT(getAttrib(na,R_ClassSymbol)), 0))
      error(_("'no' has different class than 'na'. Please make sure that both arguments have the same class."));
    UNPROTECT(2);
  }
  
  if (isFactor(a) || isFactor(b)) {
    if (!na_a && !na_b) {
      if (!R_compute_identical(PROTECT(getAttrib(a,R_LevelsSymbol)), PROTECT(getAttrib(b,R_LevelsSymbol)), 0))
        error(_("'yes' and 'no' are both type factor but their levels are different."));
      UNPROTECT(2);  
    }
    if (!na_a && !na_n) {
      if (!R_compute_identical(PROTECT(getAttrib(a,R_LevelsSymbol)), PROTECT(getAttrib(na,R_LevelsSymbol)), 0))
        error(_("'yes' and 'na' are both type factor but their levels are different."));
      UNPROTECT(2);
    }
    if (!na_b && !na_n) {
      if (!R_compute_identical(PROTECT(getAttrib(b,R_LevelsSymbol)), PROTECT(getAttrib(na,R_LevelsSymbol)), 0))
        error(_("'no' and 'na' are both type factor but their levels are different."));
      UNPROTECT(2);
    }
  }

  const int64_t amask = len1>1 ? INT64_MAX : 0; // for scalar 'a' bitwise AND will reset iterator to first element: pa[i & amask] -> pa[0]
  const int64_t bmask = len2>1 ? INT64_MAX : 0;
  const int64_t nmask = len3>1 ? INT64_MAX : 0;

  const int *restrict pl = LOGICAL(l);
  SEXP ans = PROTECT(allocVector(tans, len0)); nprotect++;
  if (!na_a) 
    copyMostAttrib(a, ans);
  else if (!na_b)
    copyMostAttrib(b, ans);
  else if (!na_n)
    copyMostAttrib(na, ans);

  switch(tans) {
  case LGLSXP: {
    int *restrict pans = LOGICAL(ans);
    const int *restrict pa   = LOGICAL(a);
    const int *restrict pb   = LOGICAL(b);
    const int pna = nonna ? LOGICAL(na)[0] : NA_LOGICAL;
    #pragma omp parallel for num_threads(getDTthreads(len0, true))
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ?
                (na_b ? na : pb[i & bmask]) :
                pl[i]==1 ? 
                (na_a ? na : pa[i & amask]) : 
                (na_n ? na : pna[i & nmask]);
    }
  } break;
  case INTSXP: {
    int *restrict pans = INTEGER(ans);
    const int *restrict pa   = INTEGER(a);
    const int *restrict pb   = INTEGER(b);
    const int pna = nonna ? INTEGER(na)[0] : NA_INTEGER;
    #pragma omp parallel for num_threads(getDTthreads(len0, true))
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ?
                (na_b ? na : pb[i & bmask]) :
                pl[i]==1 ? 
                (na_a ? na : pa[i & amask]) : 
                (na_n ? na : pna[i & nmask]);
    }
  } break;
  case REALSXP: {
    double *restrict pans = REAL(ans);
    const double *restrict pa   = REAL(a);
    const double *restrict pb   = REAL(b);
    const double na_double = INHERITS(a, char_integer64) ? NA_INT64_D : NA_REAL; // INHERITS() is true for nanotime
    const double pna = nonna ? REAL(na)[0] : na_double;
    #pragma omp parallel for num_threads(getDTthreads(len0, true))
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ?
                (na_b ? na : pb[i & bmask]) :
                pl[i]==1 ? 
                (na_a ? na : pa[i & amask]) : 
                (na_n ? na : pna[i & nmask]);
    }
  } break;
  case STRSXP : {
    const SEXP *restrict pa = na_a ? NULL : STRING_PTR(a);
    const SEXP *restrict pb = na_b ? NULL : STRING_PTR(b);
    const SEXP *restrict pna = na_n ? NULL : STRING_PTR(na);
    const SEXP na = NA_STRING;
    for (int64_t i=0; i<len0; ++i) {
      SET_STRING_ELT(
        ans, i, pl[i]==0 ?
                (na_b ? na : pb[i & bmask]) :
                pl[i]==1 ? 
                (na_a ? na : pa[i & amask]) : 
                (na_n ? na : pna[i & nmask])
      );
    }
  } break;
  case CPLXSXP : {
    Rcomplex *restrict pans = COMPLEX(ans);
    const Rcomplex *restrict pa   = COMPLEX(a);
    const Rcomplex *restrict pb   = COMPLEX(b);
    const Rcomplex pna = nonna ? COMPLEX(na)[0] : NA_CPLX;
    #pragma omp parallel for num_threads(getDTthreads(len0, true))
    for (int64_t i=0; i<len0; ++i) {
      pans[i] = pl[i]==0 ?
                (na_b ? na : pb[i & bmask]) :
                pl[i]==1 ? 
                (na_a ? na : pa[i & amask]) : 
                (na_n ? na : pna[i & nmask]);
    }
  } break;
  case VECSXP : {
    const SEXP *restrict pa = na_a ? NULL : SEXPPTR_RO(a);
    const SEXP *restrict pb = na_b ? NULL : SEXPPTR_RO(b);
    const SEXP *restrict pna = na_n ? NULL : SEXPPTR_RO(na);
    for (int64_t i=0; i<len0; ++i) {
      if (pl[i] == NA_LOGICAL) {
        if (!na_n) SET_VECTOR_ELT(ans, i, pna[i & nmask]);
      } else if (pl[i]==0) {
        if (!na_b) SET_VECTOR_ELT(ans, i, pb[i & bmask]);
      } else if (pl[i]==1) {
        if (!na_a) SET_VECTOR_ELT(ans, i, pa[i & amask]);
      }
    }
  } break;
  default:
    error(_("Type '%s' is not supported"), type2char(ta));
  }

  SEXP l_names = PROTECT(getAttrib(l, R_NamesSymbol)); nprotect++;
  if (!isNull(l_names))
    setAttrib(ans, R_NamesSymbol, l_names);

  UNPROTECT(nprotect);
  return ans;
}

SEXP fcaseR(SEXP rho, SEXP args) {
  int n=length(args); // `default` will take the last two positions
  if (n % 2) {
    error(_("Received %d inputs; please supply an even number of arguments in ..., "
              "consisting of logical condition, resulting value pairs (in that order). "
              "Note that the default argument must be named explicitly, e.g., default=0"), n - 2);
  }
  int nprotect = 0, l = 0;
  int64_t len0=0, len1=0, len2=0, idx=0;
  SEXP ans = R_NilValue, value0 = R_NilValue, tracker = R_NilValue, cons = R_NilValue, outs = R_NilValue;
  PROTECT_INDEX Icons, Iouts;
  PROTECT_WITH_INDEX(cons, &Icons); nprotect++;
  PROTECT_WITH_INDEX(outs, &Iouts); nprotect++;
  SEXPTYPE type0;
  // naout means if the output is scalar logic na
  bool imask = true, naout = false, idefault = false;
  int *restrict p = NULL;
  n = n/2;
  for (int i=0; i<n; ++i) {
    idefault = i == (n - 1); // mark if the current eval is the `default` on R side
    REPROTECT(cons = eval(SEXPPTR_RO(args)[2*i], rho), Icons);
    REPROTECT(outs = eval(SEXPPTR_RO(args)[2*i+1], rho), Iouts);
    if (isS4(outs) && !INHERITS(outs, char_nanotime)) {
      error(_("S4 class objects (except nanotime) are not supported. Please see https://github.com/Rdatatable/data.table/issues/4131."));
    }
    if (!isLogical(cons)) {
      error(_("Argument #%d must be logical."), 2*i+1);
    }
    const int *restrict pcons = LOGICAL(cons);
    if (i == 0) {
      len0 = xlength(cons);
      len2 = len0;
      type0 = TYPEOF(outs);
      value0 = outs;
      ans = PROTECT(allocVector(type0, len0)); nprotect++;
      copyMostAttrib(outs, ans);
      tracker = PROTECT(allocVector(INTSXP, len0)); nprotect++;
      p = INTEGER(tracker);     
    } else {
      imask = false;
      l = 0;
      naout = xlength(outs) == 1 && TYPEOF(outs) == LGLSXP && LOGICAL(outs)[0]==NA_LOGICAL;
      if (xlength(cons) != len0 && xlength(cons) != 1) {
        // no need to check `idefault` here because the con for default is always `TRUE`
        error(_("Argument #%d has a different length than argument #1. "
                  "Please make sure all logical conditions have the same length or length 1."),
                  i*2+1);
      }
      if (!naout && TYPEOF(outs) != type0) {
        if (idefault) {
          error(_("Resulting value is of type %s but 'default' is of type %s. "
                    "Please make sure that both arguments have the same type."), type2char(type0), type2char(TYPEOF(outs)));
        } else {
          error(_("Argument #%d is of type %s, however argument #2 is of type %s. "
                    "Please make sure all output values have the same type."),
                    i*2+2, type2char(TYPEOF(outs)), type2char(type0));
        }
      }
      if (!naout && !R_compute_identical(PROTECT(getAttrib(value0,R_ClassSymbol)),  PROTECT(getAttrib(outs,R_ClassSymbol)), 0)) {
        if (idefault) {
          error(_("Resulting value has different class than 'default'. "
                    "Please make sure that both arguments have the same class."));
        } else {
          error(_("Argument #%d has different class than argument #2, "
                    "Please make sure all output values have the same class."), i*2+2);
        }
      }
      UNPROTECT(2);
      if (!naout && isFactor(value0)) {
        if (!R_compute_identical(PROTECT(getAttrib(value0,R_LevelsSymbol)),  PROTECT(getAttrib(outs,R_LevelsSymbol)), 0)) {
          if (idefault) {
            error(_("Resulting value and 'default' are both type factor but their levels are different."));
          } else {
            error(_("Argument #2 and argument #%d are both factor but their levels are different."), i*2+2);
          }
        }
        UNPROTECT(2);
      }
    }
    len1 = xlength(outs);
    if (len1 != len0 && len1 != 1) {
      if (idefault) {
        error(_("Length of 'default' must be 1 or %"PRId64"."), len0);
      } else {
        error(_("Length of output value #%d must either be 1 or length of logical condition."), i*2+2);        
      }
    }
    int64_t amask = len1>1 ? INT64_MAX : 0, conmask = xlength(cons)>1 ? INT64_MAX : 0;
    switch(TYPEOF(ans)) {
    case LGLSXP: {
      const int *restrict pouts;
      if (!naout) pouts = LOGICAL(outs); // the content is not useful if out is NA_LOGICAL scalar
      int *restrict pans = LOGICAL(ans);
      const int pna = NA_LOGICAL;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx & conmask]==1) {
          pans[idx] = naout ? pna : pouts[idx & amask];
        } else {
          if (imask) {
            pans[j] = pna;
          }
          p[l++] = idx;
        }
      }
    } break;
    case INTSXP: {
      const int *restrict pouts;
      if (!naout) pouts = INTEGER(outs); // the content is not useful if out is NA_LOGICAL scalar
      int *restrict pans = INTEGER(ans);
      const int pna = NA_INTEGER;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx & conmask]==1) {
          pans[idx] = naout ? pna : pouts[idx & amask];
        } else {
          if (imask) {
            pans[j] = pna;
          }
          p[l++] = idx;
        }
      }
    } break;
    case REALSXP: {
      const double *restrict pouts;
      if (!naout) pouts = REAL(outs); // the content is not useful if out is NA_LOGICAL scalar
      double *restrict pans = REAL(ans);
      const double na_double = INHERITS(ans, char_integer64) ? NA_INT64_D : NA_REAL;
      const double pna = na_double;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx & conmask]==1) {
          pans[idx] = naout ? pna : pouts[idx & amask];
        } else {
          if (imask) {
            pans[j] = pna;
          }
          p[l++] = idx;
        }
      }
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict pouts;
      if (!naout) pouts = COMPLEX(outs); // the content is not useful if out is NA_LOGICAL scalar
      Rcomplex *restrict pans = COMPLEX(ans);
      const Rcomplex pna = NA_CPLX;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx & conmask]==1) {
          pans[idx] = naout ? pna : pouts[idx & amask];
        } else {
          if (imask) {
            pans[j] = pna;
          }
          p[l++] = idx;
        }
      }
    } break;
    case STRSXP: {
      const SEXP *restrict pouts;
      if (!naout) pouts = STRING_PTR(outs); // the content is not useful if out is NA_LOGICAL scalar
      const SEXP pna = NA_STRING;
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx & conmask]==1) {
          SET_STRING_ELT(ans, idx, naout ? pna : pouts[idx & amask]);
        } else {
          if (imask) {
            SET_STRING_ELT(ans, idx, pna);
          }
          p[l++] = idx;
        }
      }
    } break;
    case VECSXP: {
      // the default value of VECSXP is `NULL` so we don't need to explicitly
      // assign the NA values as it does for other atomic types
      const SEXP *restrict pouts;
      if (!naout) pouts = SEXPPTR_RO(outs); // the content is not useful if out is NA_LOGICAL scalar
      for (int64_t j=0; j<len2; ++j) {
        idx = imask ? j : p[j];
        if (pcons[idx & conmask]==1) {
          if (!naout) SET_VECTOR_ELT(ans, idx, pouts[idx & amask]);
        } else {
          p[l++] = idx;
        }
      }
    } break;
    default:
      error(_("Type %s is not supported."), type2char(TYPEOF(ans)));
    }
    UNPROTECT(2); // this cons and outs
    if (l==0) {
      break;  // stop early as nothing left to do
    }
    len2 = l;
  }
  UNPROTECT(4); // cons0, value0, ans, tracker
  return ans;
}
