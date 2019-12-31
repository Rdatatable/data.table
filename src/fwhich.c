#include "data.table.h"

/*
 * fast sorted intersect only when x and y sorted
 * do not confuse with exported R fintersect
 * now used only during benchmarks, fwhich already has a built-in intersect via 'y' and 'ny' args
 */
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
SEXP fintersectR(SEXP x, SEXP y) {
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
  fintersect(xp, nx, yp, ny, ansp, &nans);
  SETLENGTH(ans, nans);
  UNPROTECT(1);
  return ans;
}

/*
 * which equal, which in
 * handle negation
 * handling short-circuit when calling which_eq iteratively
 */
static void which_eq_int(int *x, int nx, int *out, int *nout, int val, bool negate, int *y, int ny) {
  int n = 0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        if (x[i] != val) {
          out[n] = i;
          n++;
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        if (x[i] == val) {
          out[n] = i;
          n++;
        }
      }
    }
  } else if (ny>0) { // short-circuit for intersect to y argument
    if (negate) {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] != val) {
          out[n] = y[i];
          n++;
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] == val) {
          out[n] = y[i];
          n++;
        }
      }
    }
  }
  nout[0] = n;
}
static void which_eq_double(double *x, int nx, int *out, int *nout, double val, bool negate, int *y, int ny) {
  int n = 0;
  if (ny<0) {
    if (ISNA(val)) {
      if (negate) {
        for (int i=0; i<nx; i++) {
          if (!ISNA(x[i])) {
            out[n] = i;
            n++;
          }
        }
      } else {
        for (int i=0; i<nx; i++) {
          if (ISNA(x[i])) {
            out[n] = i;
            n++;
          }
        }
      }
    } else if (ISNAN(val)) {
      if (negate) {
        for (int i=0; i<nx; i++) {
          if (!ISNAN(x[i]) || ISNA(x[i])) {
            out[n] = i;
            n++;
          }
        }
      } else {
        for (int i=0; i<nx; i++) {
          if (ISNAN(x[i]) && !ISNA(x[i])) {
            out[n] = i;
            n++;
          }
        }
      }
    } else {
      if (negate) {
        for (int i=0; i<nx; i++) {
          if (x[i] != val) {
            out[n] = i;
            n++;
          }
        }
      } else {
        for (int i=0; i<nx; i++) {
          if (x[i] == val) {
            out[n] = i;
            n++;
          }
        }
      }
    }
  } else if (ny>0) {
    if (ISNA(val)) {
      if (negate) {
        for (int i=0; i<ny; i++) {
          if (!ISNA(x[y[i]])) {
            out[n] = y[i];
            n++;
          }
        }
      } else {
        for (int i=0; i<ny; i++) {
          if (ISNA(x[y[i]])) {
            out[n] = y[i];
            n++;
          }
        }
      }
    } else if (ISNAN(val)) {
      if (negate) {
        for (int i=0; i<ny; i++) {
          if (!ISNAN(x[y[i]]) || ISNA(x[y[i]])) {
            out[n] = y[i];
            n++;
          }
        }
      } else {
        for (int i=0; i<ny; i++) {
          if (ISNAN(x[y[i]]) && !ISNA(x[y[i]])) {
            out[n] = y[i];
            n++;
          }
        }
      }
    } else {
      if (negate) {
        for (int i=0; i<ny; i++) {
          if (x[y[i]] != val) {
            out[n] = y[i];
            n++;
          }
        }
      } else {
        for (int i=0; i<ny; i++) {
          if (x[y[i]] == val) {
            out[n] = y[i];
            n++;
          }
        }
      }
    }
  }
  nout[0] = n;
}
static void which_eq_char(SEXP x, int nx, int *out, int *nout, SEXP val, bool negate, int *y, int ny) {
  int n = 0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        if (StrCmp(STRING_ELT(x, i), val)!=0) {
          out[n] = i;
          n++;
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        if (StrCmp(STRING_ELT(x, i), val)==0) {
          out[n] = i;
          n++;
        }
      }
    }
  } else if (ny>0) {
    if (negate) {
      for (int i=0; i<ny; i++) {
        if (StrCmp(STRING_ELT(x, y[i]), val)!=0) {
          out[n] = y[i];
          n++;
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        if (StrCmp(STRING_ELT(x, y[i]), val)==0) {
          out[n] = y[i];
          n++;
        }
      }
    }
  }
  nout[0] = n;
}
static void which_eq_int64(int64_t *x, int nx, int *out, int *nout, int64_t val, bool negate, int *y, int ny) {
  int n = 0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        if (x[i] != val) {
          out[n] = i;
          n++;
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        if (x[i] == val) {
          out[n] = i;
          n++;
        }
      }
    }
  } else if (ny>0) {
    if (negate) {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] != val) {
          out[n] = y[i];
          n++;
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] == val) {
          out[n] = y[i];
          n++;
        }
      }
    }
  }
  nout[0] = n;
}
void which_eq(SEXP x, int *iwhich, int *nwhich, SEXP val, bool negate, int *y, int ny) {
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  switch (TYPEOF(x)) {
  case LGLSXP: {
    which_eq_int(LOGICAL(x), length(x), iwhich, nwhich, LOGICAL(val)[0], negate, y, ny);
  } break;
  case INTSXP: {
    which_eq_int(INTEGER(x), length(x), iwhich, nwhich, INTEGER(val)[0], negate, y, ny);
  } break;
  case REALSXP: {
    if (Rinherits(x, char_integer64)) {
      which_eq_int64((int64_t *)REAL(x), length(x), iwhich, nwhich, ((int64_t *)REAL(val))[0], negate, y, ny);
    } else {
      which_eq_double(REAL(x), length(x), iwhich, nwhich, REAL(val)[0], negate, y, ny);
    }
  } break;
  case STRSXP: {
    which_eq_char(x, length(x), iwhich, nwhich, STRING_ELT(val, 0), negate, y, ny);
  } break;
  default: {
    error("%s: Incompatible type: %s", __func__, type2char(TYPEOF(x)));
  }
  }
  if (verbose)
    Rprintf("which_eq: took %.3fs\n", omp_get_wtime()-tic);
}

static void which_in_int(int *x, int nx, int *out, int *nout, int *val, int nval, bool negate, int *y, int ny) {
  int n = 0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        for (int j=0; j<nval; j++) {
          if (x[i] != val[j]) {
            out[n] = i;
            n++;
            break;
          }
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        for (int j=0; j<nval; j++) {
          if (x[i] == val[j]) {
            out[n] = i;
            n++;
            break;
          }
        }
      }
    }
  } else if (ny>0) { // short-circuit for intersect to y argument
    if (negate) {
      for (int i=0; i<ny; i++) {
        for (int j=0; j<nval; j++) {
          if (x[y[i]] != val[j]) {
            out[n] = y[i];
            n++;
            break;
          }
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        for (int j=0; j<nval; j++) {
          if (x[y[i]] == val[j]) {
            out[n] = y[i];
            n++;
            break;
          }
        }
      }
    }
  }
  nout[0] = n;
}
static void which_in_double(double *x, int nx, int *out, int *nout, double *val, int nval, bool negate, int *y, int ny) {
  int n = 0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        for (int j=0; j<nval; j++) {
          if (x[i] != val[j]) {
            out[n] = i;
            n++;
            break;
          }
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        for (int j=0; j<nval; j++) {
          if (x[i] == val[j]) {
            out[n] = i;
            n++;
            break;
          }
        }
      }
    }
  } else if (ny>0) {
    if (negate) {
      for (int i=0; i<ny; i++) {
        for (int j=0; j<nval; j++) {
          if (x[y[i]] != val[j]) {
            out[n] = y[i];
            n++;
            break;
          }
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        for (int j=0; j<nval; j++) {
          if (x[y[i]] == val[j]) {
            out[n] = y[i];
            n++;
            break;
          }
        }
      }
    }
  }
  nout[0] = n;
} // TODO: NA and NaN support
static void which_in_char(SEXP x, int nx, int *out, int *nout, SEXP val, int nval, bool negate, int *y, int ny) {
  int n = 0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        for (int j=0; j<nval; j++) {
          if (StrCmp(STRING_ELT(x, i), STRING_ELT(val, j))!=0) {
            out[n] = i;
            n++;
            break;
          }
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        for (int j=0; j<nval; j++) {
          if (StrCmp(STRING_ELT(x, i), STRING_ELT(val, j))==0) {
            out[n] = i;
            n++;
            break;
          }
        }
      }
    }
  } else if (ny>0) {
    if (negate) {
      for (int i=0; i<ny; i++) {
        for (int j=0; j<nval; j++) {
          if (StrCmp(STRING_ELT(x, y[i]), STRING_ELT(val, j))!=0) {
            out[n] = y[i];
            n++;
            break;
          }
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        for (int j=0; j<nval; j++) {
          if (StrCmp(STRING_ELT(x, y[i]), STRING_ELT(val, j))==0) {
            out[n] = y[i];
            n++;
            break;
          }
        }
      }
    }
  }
  nout[0] = n;
}
static void which_in_int64(int64_t *x, int nx, int *out, int *nout, int64_t *val, int nval, bool negate, int *y, int ny) {
  int n = 0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        for (int j=0; j<nval; j++) {
          if (x[i] != val[j]) {
            out[n] = i;
            n++;
            break;
          }
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        for (int j=0; j<nval; j++) {
          if (x[i] == val[j]) {
            out[n] = i;
            n++;
            break;
          }
        }
      }
    }
  } else if (ny>0) {
    if (negate) {
      for (int i=0; i<ny; i++) {
        for (int j=0; j<nval; j++) {
          if (x[y[i]] != val[j]) {
            out[n] = y[i];
            n++;
            break;
          }
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        for (int j=0; j<nval; j++) {
          if (x[y[i]] == val[j]) {
            out[n] = y[i];
            n++;
            break;
          }
        }
      }
    }
  }
  nout[0] = n;
}
void which_in(SEXP x, int *iwhich, int *nwhich, SEXP val, bool negate, int *y, int ny) {
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  switch (TYPEOF(x)) {
  case LGLSXP: {
    which_in_int(LOGICAL(x), length(x), iwhich, nwhich, LOGICAL(val), length(val), negate, y, ny);
  } break;
  case INTSXP: {
    which_in_int(INTEGER(x), length(x), iwhich, nwhich, INTEGER(val), length(val), negate, y, ny);
  } break;
  case REALSXP: {
    if (Rinherits(x, char_integer64)) {
      which_in_int64((int64_t *)REAL(x), length(x), iwhich, nwhich, (int64_t *)REAL(val), length(val), negate, y, ny);
    } else {
      which_in_double(REAL(x), length(x), iwhich, nwhich, REAL(val), length(val), negate, y, ny);
    }
  } break;
  case STRSXP: {
    which_in_char(x, length(x), iwhich, nwhich, val, length(val), negate, y, ny);
  } break;
  default: {
    error("%s: Incompatible type: %s", __func__, type2char(TYPEOF(x)));
  }
  }
  if (verbose)
    Rprintf("which_in: took %.3fs\n", omp_get_wtime()-tic);
}

/*
 * which op
 * route for which_eq, which_in, which_like, etc.
 * used so we can optimize: DT[v1==s1 & v2%in%s2 & like(v3,s3)]
 */
#define AND_CALL(x) (CAR(x)==sym_and)
//#define OR_CALL(x) (CAR(x)==sym_or) // TODO: not yet used, would allow to support: DT[v1==s1 | v2==s2 | v3==s3] - has to use union instead of intersect short-circuit
#define EQ_CALL(x) (CAR(x)==sym_equal)
#define NEQ_CALL(x) (CAR(x)==sym_nequal)
#define IN_CALL(x) (CAR(x)==sym_in)
#define NIN_CALL(x) (CAR(x)==sym_nin)
#define OP_CALL(x) (EQ_CALL(x) || NEQ_CALL(x) || IN_CALL(x) || NIN_CALL(x))
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
  } else {
    error("internal error: unsupported OP, should have been caught by now, please report to issue tracker");
  }
}

/*
 * fast which optimization
 * helper function to process unevaluated expression from form of pairlist into a list of expressions
 * also investigate if optimization is possible for provided expression
 */
SEXP fwhichOpt(SEXP expr, bool *doOpt) {
  const bool verbose = GetVerbose();
  int protecti = 0;
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  bool escape = false;
  int nand = 0, nop = 0;
  SEXP node = expr;
  //bool debug = false;
  for (int i=0;; i++) { // examine if supported expression, and investigate number of AND and OP
    if (escape) {
      //if (debug) {Rprintf("id=%d, doOpt=0, skip", i);}
      continue;
    }
    if (OP_CALL(node)) {
      //if (debug) {Rprintf("i=%d, leaf node of supported OP\n", i);}
      nop++;
      break;
    } else if (AND_CALL(node)) {
      nand++;
      SEXP args = CDR(node);
      SEXP rhs = CADR(args);
      if (OP_CALL(rhs)) {
        nop++;
      } else {
        escape = true;
        break;
      }
      node = CAR(args);
    } else {
      //if (debug) {Rprintf("i=%d, unsupported OP, node: ", i); Rf_PrintValue(node);}
      escape = true;
      break;
    }
  }
  //if (debug) {Rprintf("fwhichOpt: nand=%d, nop=%d\n", nand, nop);}
  if (!escape && nand+1 != nop)
    error("internal error: number of & should be equal to number of OP-1, please report to issue tracker");
  SEXP ans = R_NilValue;
  if (!escape) {
    ans = PROTECT(allocVector(VECSXP, nop)); protecti++;
    for (int i=0; i<nop; i++) {
      node = expr;
      //if (debug) {Rprintf("i=%d, node: ", i); Rf_PrintValue(node);}
      for (int j=0; j<nand-i; j++) {
        node = CADR(node);
        //if (debug) {Rprintf("i=%d, j=%d, subsetting node: ", i, j);  Rf_PrintValue(node);}
      }
      if (i > 0)
        node = CADDR(node);
      //if (debug) {Rprintf("i=%d, final node: ", i);  Rf_PrintValue(node);}
      SET_VECTOR_ELT(ans, i, node);
    }
  }
  doOpt[0] = !escape;
  if (verbose)
    Rprintf("fwhichOpt: took %.3fs\n", omp_get_wtime()-tic);
  UNPROTECT(protecti);
  return ans;
}

/*
 * fast which
 * for a supported type of input: DT[v1==s1 & v2==s2 & v3==s3]
 * for now has to be wrapped with: DT[fwhich(v1==s1 & v2==s2 & v3==s3)]
 * redirects to which_op calls
 */
SEXP fwhichR(SEXP expr, SEXP rho) {
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  int protecti = 0;
  bool doOpt = false;
  SEXP ans = R_NilValue;
  SEXP exprs = fwhichOpt(expr, &doOpt);
  if (!doOpt) {
    if (verbose)
      Rprintf("fwhich: optimization not available for operations used in provided expression, fallback to R's which\n");
  } else {
    if (verbose)
      Rprintf("fwhich: optimization kicks-in\n");
    int ny=-1, nwhich=0;
    int *yp=0, *iwhich=0;
    int nexprs = length(exprs);
    for (int i=0; i<nexprs; i++) {
      if (i && !ny) { // already zero length answer
        //if (verbose) Rprintf("fwhich iter %d skip as already 0 length result\n", i+1);
        continue;
      }
      SEXP e = VECTOR_ELT(exprs, i);
      SEXP x = eval(CADR(e), rho);
      SEXP val = eval(CADDR(e), rho);
      //Rprintf("x: "); Rf_PrintValue(x); Rprintf("val: "); /Rf_PrintValue(val);
      if (!(isInteger(x) || isReal(x) || isString(x)) || !(isInteger(val) || isReal(val) || isString(val))) {
        if (verbose)
          Rprintf("fwhich optimization not available for argument classes used in provided expression, fallback to R's which\n");
        doOpt = false;
        break;
      }
      R_len_t nx = length(x);
      int nans = !i ? nx : MIN(nx, ny); // this MIN will have to be MAX for `|` operator, where we need union instead of intersect
      ans = PROTECT(allocVector(INTSXP, nans)); protecti++; // TODO: protect with index instead?
      iwhich = INTEGER(ans);
      which_op(
        e,               // this iter expr
        x,               // lhs
        iwhich, &nwhich, // this iter result
        val,             // rhs
        yp, ny           // last iter result
      );
      ny = nwhich;
      yp = INTEGER(ans);
    }
    if (doOpt) {
      for (int i=0; i<nwhich; i++) {
        iwhich[i]++; // shift 0-based index to 1-based index
      }
      SETLENGTH(ans, nwhich);
    }
  }
  if (!doOpt) {
    expr = PROTECT(LCONS(install("which"), LCONS(expr, R_NilValue))); protecti++;
    ans = PROTECT(eval(expr, rho)); protecti++;
  }
  if (verbose)
    Rprintf("fwhich: took %.3fs\n", omp_get_wtime()-tic);
  UNPROTECT(protecti);
  return ans;
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
  which_eq(x, iwhich, &nwhich, val, LOGICAL(negate)[0], yp, ny);
  for (int i=0; i<nwhich; i++) {
    iwhich[i]++; // shift 0-based index to 1-based index
  }
  SETLENGTH(ans, nwhich);
  UNPROTECT(protecti);
  return ans;
} // used only for benchmarks
/*SEXP fwhichOptR(SEXP expr) {
 bool doOpt = false;
 SEXP ans = fwhichOpt(expr, &doOpt);
 Rprintf("fwhichOptR: doOpt %d\n", doOpt);
 return ans;
} // only for testing dev*/
