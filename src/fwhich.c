#include "data.table.h"

static SEXP ans;
static SEXP rho;
static int ny, nwhich;
static int *yp, *iwhich;
static bool alloc;

/*
 * fast sorted intersect
 * do not confuse with exported R fintersect
 * fwhich already has a built-in intersect via which_*_*(y, ny) args
 * used only during benchmarks, thus no extra checks in fsintersectR
 */
void fsintersect(int *x, int nx, int *y, int ny, int *out, int *nans) {
  if (nx && ny) { // otherwise segfault when fsintersect(integer(), integer())
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
SEXP fsintersectR(SEXP x, SEXP y) {
  if (!isInteger(x))
    error("fsintersect accept only integer class");
  if (!isInteger(y))
    error("fsintersect accept only integer class");
  // check no NAs
  // check no duplicates
  // check sorted
  R_len_t nx = xlength(x), ny = xlength(y);
  int *xp = INTEGER(x), *yp = INTEGER(y);
  SEXP ans = PROTECT(allocVector(INTSXP, MIN(nx, ny)));
  int *ansp = INTEGER(ans);
  int nans = 0;
  fsintersect(xp, nx, yp, ny, ansp, &nans);
  SETLENGTH(ans, nans);
  UNPROTECT(1);
  return ans;
}

/*
 * which equal, which in, which like
 * support negation
 * support short-circuit when calling which_* iteratively
 */

#define WHICH_OP(ITER) {                                       \
  if (negate) {                                                \
    for (int i=0; i<niter; ++i) {                              \
      if (x[ITER] != val) {                                    \
        out[n] = ITER;                                         \
        n++;                                                   \
      }                                                        \
    }                                                          \
  } else {                                                     \
    for (int i=0; i<niter; ++i) {                              \
      if (x[ITER] == val) {                                    \
        out[n] = ITER;                                         \
        n++;                                                   \
      }                                                        \
    }                                                          \
  }                                                            \
}
#define WHICH_FUN(ITER, FUN) {                                 \
  if (negate) {                                                \
    for (int i=0; i<niter; ++i) {                              \
      if (!FUN(x[ITER])) {                                     \
        out[n] = ITER;                                         \
        n++;                                                   \
      }                                                        \
    }                                                          \
  } else {                                                     \
    for (int i=0; i<niter; ++i) {                              \
      if (FUN(x[ITER])) {                                      \
        out[n] = ITER;                                         \
        n++;                                                   \
      }                                                        \
    }                                                          \
  }                                                            \
}
#define WHICH_CHAR(ITER) {                                     \
  if (negate) {                                                \
    for (int i=0; i<niter; ++i) {                              \
      if (StrCmp(xp[ITER], val)) {                             \
        out[n] = ITER;                                         \
        n++;                                                   \
      }                                                        \
    }                                                          \
  } else {                                                     \
    for (int i=0; i<niter; ++i) {                              \
      if (!StrCmp(xp[ITER], val)) {                            \
        out[n] = ITER;                                         \
        n++;                                                   \
      }                                                        \
    }                                                          \
  }                                                            \
}
static void which_eq_int(int *x, int nx, int *out, int *nout, int val, bool negate, int *y, int ny) {
  int n = 0, niter = ny<0 ? nx : ny;
  if (ny<0) {
    WHICH_OP(i);
  } else if (ny>0) { // short-circuit for intersect to y argument
    WHICH_OP(y[i]);
  }
  nout[0] = n;
}
static void which_eq_double(double *x, int nx, int *out, int *nout, double val, bool negate, int *y, int ny) {
  int n = 0, niter = ny<0 ? nx : ny;
  if (ny<0) {
    if (R_IsNaN(val)) { // base::is.nan
      WHICH_FUN(i, R_IsNaN);
    } else if (ISNAN(val)) { // base::is.na
      WHICH_FUN(i, ISNAN);
    } else {
      WHICH_OP(i);
    }
  } else if (ny>0) {
    if (R_IsNaN(val)) {
      WHICH_FUN(y[i], R_IsNaN);
    } else if (ISNAN(val)) {
      WHICH_FUN(y[i], ISNAN);
    } else {
      WHICH_OP(y[i]);
    }
  }
  nout[0] = n;
}
static void which_eq_char(SEXP x, int nx, int *out, int *nout, SEXP val, bool negate, int *y, int ny) {
  int n = 0, niter = ny<0 ? nx : ny;
  const SEXP *xp = STRING_PTR(x);
  if (ny<0) {
    WHICH_CHAR(i);
  } else if (ny>0) {
    WHICH_CHAR(y[i]);
  }
  nout[0] = n;
}
static void which_eq_int64(int64_t *x, int nx, int *out, int *nout, int64_t val, bool negate, int *y, int ny) {
  int n = 0, niter = ny<0 ? nx : ny;
  if (ny<0) {
    WHICH_OP(i);
  } else if (ny>0) {
    WHICH_OP(y[i]);
  }
  nout[0] = n;
}
void which_eq(SEXP x, int *iwhich, int *nwhich, SEXP val, bool negate, int *y, int ny) {
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  int nx = length(x);
  int nxy = ny<0 ? nx : ny;
  switch (TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    which_eq_int(INTEGER(x), nx, iwhich, nwhich, INTEGER(val)[0], negate, y, ny);
  } break;
  case REALSXP: {
    if (Rinherits(x, char_integer64)) {
      which_eq_int64((int64_t *)REAL(x), nx, iwhich, nwhich, ((int64_t *)REAL(val))[0], negate, y, ny);
    } else {
      which_eq_double(REAL(x), nx, iwhich, nwhich, REAL(val)[0], negate, y, ny);
    }
  } break;
  case STRSXP: {
    which_eq_char(x, nx, iwhich, nwhich, STRING_ELT(val, 0), negate, y, ny);
  } break;
  default: {
    error("%s: Incompatible type: %s", __func__, type2char(TYPEOF(x)));
  }
  }
  if (verbose)
    Rprintf("%s: in %d, out %d, took %.3fs\n", __func__, nxy, nwhich[0], omp_get_wtime()-tic);
}

#define WHICH_IN_OP(ITER) {                                    \
  if (negate) {                                                \
    for (int i=0; i<niter; i++) {                              \
      for (int j=0; j<nval; j++) {                             \
        if (x[ITER] != val[j]) {                               \
          out[n] = ITER;                                       \
          n++;                                                 \
          break;                                               \
        }                                                      \
      }                                                        \
    }                                                          \
  } else {                                                     \
    for (int i=0; i<niter; i++) {                              \
      for (int j=0; j<nval; j++) {                             \
        if (x[ITER] == val[j]) {                               \
          out[n] = ITER;                                       \
          n++;                                                 \
          break;                                               \
        }                                                      \
      }                                                        \
    }                                                          \
  }                                                            \
}
#define WHICH_IN_CHAR(ITER) {                                  \
  if (negate) {                                                \
    for (int i=0; i<niter; ++i) {                              \
      for (int j=0; j<nval; j++) {                             \
        if (StrCmp(xp[ITER], valp[j])) {                       \
          out[n] = ITER;                                       \
          n++;                                                 \
          break;                                               \
        }                                                      \
      }                                                        \
    }                                                          \
  } else {                                                     \
    for (int i=0; i<niter; ++i) {                              \
      for (int j=0; j<nval; j++) {                             \
        if (!StrCmp(xp[ITER], valp[j])) {                      \
          out[n] = ITER;                                       \
          n++;                                                 \
          break;                                               \
        }                                                      \
      }                                                        \
    }                                                          \
  }                                                            \
}
static void which_in_int(int *x, int nx, int *out, int *nout, int *val, int nval, bool negate, int *y, int ny) {
  int n = 0, niter = ny<0 ? nx : ny;
  if (ny<0) {
    WHICH_IN_OP(i);
  } else if (ny>0) { // short-circuit for intersect to y argument
    WHICH_IN_OP(y[i]);
  }
  nout[0] = n;
}
static void which_in_double(double *x, int nx, int *out, int *nout, double *val, int nval, bool negate, int *y, int ny) {
  int n = 0, niter = ny<0 ? nx : ny;
  if (ny!=0) {
    /*bool hasNA = false, hasNaN = false; // TODO: NA, NaN handling
    for (int j=0; j<nval; j++) {
      if (ISNA(val[j])) {
        hasNA = true;
        hasNaN = true;
        break;
      }
    }*/
    if (ny<0) {
      WHICH_IN_OP(i);
    } else if (ny>0) {
      WHICH_IN_OP(y[i]);
    }
  }
  nout[0] = n;
} // TODO: NA and NaN support
static void which_in_char(SEXP x, int nx, int *out, int *nout, SEXP val, int nval, bool negate, int *y, int ny) {
  int n = 0, niter = ny<0 ? nx : ny;
  const SEXP *xp = STRING_PTR(x);
  const SEXP *valp = STRING_PTR(val);
  if (ny<0) {
    WHICH_IN_CHAR(i);
  } else if (ny>0) {
    WHICH_IN_CHAR(y[i]);
  }
  nout[0] = n;
}
static void which_in_int64(int64_t *x, int nx, int *out, int *nout, int64_t *val, int nval, bool negate, int *y, int ny) {
  int n = 0, niter = ny<0 ? nx : ny;
  if (ny<0) {
    WHICH_IN_OP(i);
  } else if (ny>0) {
    WHICH_IN_OP(y[i]);
  }
  nout[0] = n;
}
void which_in(SEXP x, int *iwhich, int *nwhich, SEXP val, bool negate, int *y, int ny) {
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  int nx = length(x);
  int nxy = ny<0 ? nx : ny;
  switch (TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    which_in_int(INTEGER(x), nx, iwhich, nwhich, INTEGER(val), length(val), negate, y, ny);
  } break;
  case REALSXP: {
    if (Rinherits(x, char_integer64)) {
      which_in_int64((int64_t *)REAL(x), nx, iwhich, nwhich, (int64_t *)REAL(val), length(val), negate, y, ny);
    } else {
      which_in_double(REAL(x), nx, iwhich, nwhich, REAL(val), length(val), negate, y, ny);
    }
  } break;
  case STRSXP: {
    which_in_char(x, nx, iwhich, nwhich, val, length(val), negate, y, ny);
  } break;
  default: {
    error("%s: Incompatible type: %s", __func__, type2char(TYPEOF(x)));
  }
  }
  if (verbose)
    Rprintf("%s: in %d, out %d, took %.3fs\n", __func__, nxy, nwhich[0], omp_get_wtime()-tic);
}

static void which_like_any(SEXP x, int nx, int *out, int *nout, SEXP val, int nval, bool negate, int *y, int ny) {
  int n = 0;
  int protecti = 0;
  // if (isFactor(x)) {} // TODO: optimize same as in `like`
  if (ny>0) {
    SEXP idx = PROTECT(allocVector(INTSXP, ny)); protecti++;
    int *idxp = INTEGER(idx);
    for (int i=0; i<ny; i++) {
      idxp[i] = y[i]+1;
    }
    x = PROTECT(subsetVector(x, idx)); protecti++;
  } // short-circuit
  if (ny!=0) {
    SEXP grep = PROTECT(lang5(install("grep"), val, x, /*value=*/PROTECT(ScalarLogical(false)), /*invert=*/PROTECT(ScalarLogical(negate)))); protecti++; protecti++; protecti++;
    SET_TAG(CDR(grep), install("pattern")); SET_TAG(CDDR(grep), install("x")); SET_TAG(CDR(CDDR(grep)), install("value")); SET_TAG(CDDR(CDDR(grep)), install("invert"));
    SEXP ans = PROTECT(eval(grep, R_GlobalEnv)); protecti++;
    n = length(ans);
    int *ansp = INTEGER(ans);
    for (int i=0; i<n; i++) {
      out[i] = ansp[i]-1;
    }
  }
  UNPROTECT(protecti);
  nout[0] = n;
} // TODO: avoid R C eval and call C grep, avoid 1-0-1 index shifting
void which_like(SEXP x, int *iwhich, int *nwhich, SEXP val, bool negate, int *y, int ny) {
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  int nx = length(x);
  int nxy = ny<0 ? nx : ny;
  which_like_any(x, nx, iwhich, nwhich, val, length(val), negate, y, ny);
  if (verbose)
    Rprintf("%s: in %d, out %d, took %.3fs\n", __func__, nxy, nwhich[0], omp_get_wtime()-tic);
}

/*
 * which op
 * route for which_eq, which_in, which_like, etc.
 * used so we can optimize: DT[v1==s1 & v2%in%s2 & like(v3,s3)]
 */
#define AND_CALL(x) (CAR(x)==sym_and)
#define EQ_CALL(x) (CAR(x)==sym_equal)
#define NEQ_CALL(x) (CAR(x)==sym_nequal)
#define IN_CALL(x) (CAR(x)==sym_in)
#define BANG_CALL(x) (CAR(x)==sym_bang)
#define NIN_CALL(x) (CAR(x)==sym_nin || (BANG_CALL(x) && IN_CALL(CADR(x)))) // optimizes x%!in%y (FR #4152 not yet merged but will work here anyway) and !x%in%y
#define LIKE_CALL(x) (CAR(x)==sym_oplike || (CAR(x)==sym_like && length(CDR(x))==2)) // like(,, ignore.case, fixed) not optimized!
#define NLIKE_CALL(x) (BANG_CALL(x) && LIKE_CALL(CADR(x)))
#define OP_CALL(x) (EQ_CALL(x) || NEQ_CALL(x) || IN_CALL(x) || NIN_CALL(x) || LIKE_CALL(x) || NLIKE_CALL(x)) // all supported operators

void which_op(SEXP e, SEXP x, int *iwhich, int *nwhich, SEXP val, int *y, int ny) {
  if (EQ_CALL(e) || NEQ_CALL(e)) {
    which_eq(
      x,              // lhs
      iwhich, nwhich, // this iter result
      val,            // rhs
      NEQ_CALL(e),    // negate
      y, ny           // last iter result
    );
  } else if (IN_CALL(e) || NIN_CALL(e)) {
    which_in(x, iwhich, nwhich, val, NIN_CALL(e), y, ny);
  } else if (LIKE_CALL(e) || NLIKE_CALL(e)) {
    which_like(x, iwhich, nwhich, val, NLIKE_CALL(e), y, ny);
  } else {
    error("internal error: unsupported operator, should have been caught by now, please report to issue tracker");
  }
}

SEXP rwhich(SEXP expr) {
  //expr = PROTECT(LCONS(install("which"), LCONS(expr, R_NilValue))); protecti++; // add useNames=FALSE
  //ans = PROTECT(eval(expr, rho)); protecti++;
  //write ans but decode to 0 based index
  error("not yet implemented");
  return R_NilValue;
}

void fwhich1(SEXP expr, int d) {
  if (d > 10)
    error("recursion depth exceeded 10");
  if (nwhich == 0) {
    //Rprintf("fwhich1: nwhich already 0 escaping tree: "); Rf_PrintValue(expr);
    return;
  }
  if (AND_CALL(expr)) {
    //Rprintf("fwhich1: calling child fwhich1(expr, %d): ", d+1); Rf_PrintValue(CADR(expr));
    fwhich1(CADR(expr), d+1);
    //Rprintf("fwhich1: calling child fwhich1(expr, %d): ", d+1); Rf_PrintValue(CADDR(expr));
    fwhich1(CADDR(expr), d+1);
    return;
  }
  //Rprintf("fwhich1: computing leaf at %d, current nwhich=%d: ", d, nwhich); Rf_PrintValue(expr);
  SEXP x = R_NilValue, val = R_NilValue;
  //bool was_bang = false;
  //SEXP expr_backup = expr;
  if (OP_CALL(expr)) {
    if (BANG_CALL(expr)) {
      //was_bang = true; // if we need to redirect to R which
      expr = CADR(expr);
    }
    SEXP lhs = CADR(expr);
    SEXP rhs = CADDR(expr);
    bool sl=isSymbol(lhs) || isLanguage(lhs), sr=isSymbol(rhs) || isLanguage(rhs);
    if (!sl && !sr) { // at least one side of ==, != has to be a symbol/call
      Rprintf("%s: %d: none of LHS and RHS is symbol or call\n", __func__, d);
      error("not yet implemented");
      rwhich(expr);
      return;
    }
    if (!sl && !isVectorAtomic(lhs)) { // if not symbol/call then must be atomic
      Rprintf("%s: %d: LHS is not symbol, call or atomic vector\n", __func__, d);
      error("not yet implemented");
      rwhich(expr);
      return;
    }
    if (!sr && !isVectorAtomic(rhs)) { // same for rhs
      Rprintf("%s: %d: RHS is not symbol, call or atomic vector\n", __func__, d);
      error("not yet implemented");
      rwhich(expr);
      return;
    }
    if (sl) { // evaluate symbol/call, ensure it is atomic
      lhs = eval(lhs, rho);
      if (!isVectorAtomic(lhs)) {
        Rprintf("%s: %d: LHS evaluated to a non atomic vector\n", __func__, d);
        error("not yet implemented");
        //rwhich(expr); // this evaluates lhs second time!
        return;
      }
    }
    if (sr) {
      rhs = eval(rhs, rho);
      if (!isVectorAtomic(rhs)) {
        Rprintf("%s: %d: RHS evaluated to a non atomic vector\n", __func__, d);
        error("not yet implemented");
        //rwhich(expr); // this evaluates rhs second time!
        return;
      }
    }
    /*if (EQ_CALL(expr) || NEQ_CALL(expr)) {
      if (LENGTH(lhs)!=1) {
        if (LENGTH(rhs)!=1) {
          Rprintf("%s: %d: none of LHS and RHS has length 1\n", __func__, d);
          //rwhich(expr); // this evaluates lhs and rhs second time!
          error("not yet implemented");
          return;
        } else {
          x = lhs;
          val = rhs;
        }
      } else {
        if (LENGTH(rhs)!=1) {
          x = rhs;
          val = lhs;
        } else {
          x = lhs;
          val = rhs;
        }
      }
    } else if (IN_CALL(expr) || NIN_CALL(expr)) {
    } else if (LIKE_CALL(expr) || NLIKE_CALL(expr)) {
    }*/
    x = lhs;
    val = rhs;
  } else {
    error("not yet implemented");
  }
  //Rprintf("fwhich1: calling which_op(expr, ...): "); Rf_PrintValue(expr);
  R_len_t nx = LENGTH(x);
  if (!alloc) {
    //Rprintf("fwhich1: answer not yet allocated, allocate now\n");
    ans = PROTECT(allocVector(INTSXP, nx));
    alloc = true;
  } else {
    if (nx!=LENGTH(ans))
      error("vectors are of different length");
  }
  iwhich = INTEGER(ans);
  //Rprintf("ans before"); Rf_PrintValue(ans);
  which_op(
    expr,            // this iter expr
    x,               // lhs
    iwhich, &nwhich, // this iter result
    val,             // rhs
    yp, ny           // last iter result
  );
  //Rprintf("ans after"); Rf_PrintValue(ans);
  ny = nwhich;
  yp = INTEGER(ans);
  //Rprintf("fwhich1: leaf at %d computed, current nwhich=%d\n", d, nwhich);
}
SEXP fwhichR(SEXP expr, SEXP rhoArg) {
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  rho = rhoArg;
  ny=-1;
  nwhich=-1;
  alloc = false;
  //Rprintf("fwhichR: calling recursive fwhich1(expr, %d): ", 0); Rf_PrintValue(expr);
  fwhich1(expr, 0);
  //Rprintf("fwhichR: align ans to 1-based index:\n");
  for (int i=0; i<nwhich; i++) {
    iwhich[i]++; // shift 0-based index to 1-based index
  }
  SETLENGTH(ans, nwhich);
  if (verbose)
    Rprintf("%s: took %.3fs\n", __func__, omp_get_wtime()-tic);
  if (alloc)
    UNPROTECT(1);
  return ans;
}

/* dev space */
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
  int *yp = 0;
  if (ny>0) {
    intersect = duplicate(intersect);
    yp = INTEGER(intersect);
    for (int i=0; i<ny; i++) {
      yp[i]--; // shift 1-based index to 0-based index - this is for intersect argument, a short circuit
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, nans)); protecti++;
  int *iwhich = INTEGER(ans);
  int nwhich = 0;
  which_eq(x, iwhich, &nwhich, val, LOGICAL(negate)[0], yp, ny);
  for (int i=0; i<nwhich; i++) {
    iwhich[i]++; // shift 0-based index to 1-based index
  }
  SETLENGTH(ans, nwhich);
  UNPROTECT(protecti);
  return ans;
} // used only for benchmarks
