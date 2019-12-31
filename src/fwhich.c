#include "data.table.h"

/*
 * fast intersect only when x and y sorted
 * now used only during benchmarks, which_eq_int already has a built-in intersect via 'y' and 'ny' args
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
    error("fintersect accept only integer class");
  if (!isInteger(y))
    error("fintersect accept only integer class");
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
 * which equals to
 * which_eq, !which_eq
 * handling short-circuit when calling which_eq iteratively
 */
static inline void which_eq_int(int *x, int nx, int *out, int *nout, int val, bool negate, int *y, int ny) {
  int j=0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        if (x[i] != val) {
          out[j] = i;
          j++;
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        if (x[i] == val) {
          out[j] = i;
          j++;
        }
      }
    }
  } else if (ny>0) { // short circuit when intersect/y arg provided
    if (negate) {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] != val) {
          out[j] = y[i];
          j++;
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] == val) {
          out[j] = y[i];
          j++;
        }
      }
    }
  }
  nout[0] = j;
}
static inline void which_eq_double(double *x, int nx, int *out, int *nout, double val, bool negate, int *y, int ny) {
  int j=0;
  if (ny<0) {
    if (ISNA(val)) {
      if (negate) {
        for (int i=0; i<nx; i++) {
          if (!ISNA(x[i])) {
            out[j] = i;
            j++;
          }
        }
      } else {
        for (int i=0; i<nx; i++) {
          if (ISNA(x[i])) {
            out[j] = i;
            j++;
          }
        }
      }
    } else if (ISNAN(val)) {
      if (negate) {
        for (int i=0; i<nx; i++) {
          if (!ISNAN(x[i]) || ISNA(x[i])) {
            out[j] = i;
            j++;
          }
        }
      } else {
        for (int i=0; i<nx; i++) {
          if (ISNAN(x[i]) && !ISNA(x[i])) {
            out[j] = i;
            j++;
          }
        }
      }
    } else {
      if (negate) {
        for (int i=0; i<nx; i++) {
          if (x[i] != val) {
            out[j] = i;
            j++;
          }
        }
      } else {
        for (int i=0; i<nx; i++) {
          if (x[i] == val) {
            out[j] = i;
            j++;
          }
        }
      }
    }
  } else if (ny>0) {
    if (ISNA(val)) {
      if (negate) {
        for (int i=0; i<ny; i++) {
          if (!ISNA(x[y[i]])) {
            out[j] = y[i];
            j++;
          }
        }
      } else {
        for (int i=0; i<ny; i++) {
          if (ISNA(x[y[i]])) {
            out[j] = y[i];
            j++;
          }
        }
      }
    } else if (ISNAN(val)) {
      if (negate) {
        for (int i=0; i<ny; i++) {
          if (!ISNAN(x[y[i]]) || ISNA(x[y[i]])) {
            out[j] = y[i];
            j++;
          }
        }
      } else {
        for (int i=0; i<ny; i++) {
          if (ISNAN(x[y[i]]) && !ISNA(x[y[i]])) {
            out[j] = y[i];
            j++;
          }
        }
      }
    } else {
      if (negate) {
        for (int i=0; i<ny; i++) {
          if (x[y[i]] != val) {
            out[j] = y[i];
            j++;
          }
        }
      } else {
        for (int i=0; i<ny; i++) {
          if (x[y[i]] == val) {
            out[j] = y[i];
            j++;
          }
        }
      }
    }
  }
  nout[0] = j;
}
static inline void which_eq_char(SEXP x, int nx, int *out, int *nout, SEXP val, bool negate, int *y, int ny) {
  int j=0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        if (STRING_ELT(x, i) != val) {
          out[j] = i;
          j++;
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        if (STRING_ELT(x, i) == val) {
          out[j] = i;
          j++;
        }
      }
    }
  } else if (ny>0) {
    if (negate) {
      for (int i=0; i<ny; i++) {
        if (STRING_ELT(x, y[i]) != val) {
          out[j] = y[i];
          j++;
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        if (STRING_ELT(x, y[i]) == val) {
          out[j] = y[i];
          j++;
        }
      }
    }
  }
  nout[0] = j;
}
static inline void which_eq_int64(int64_t *x, int nx, int *out, int *nout, int64_t val, bool negate, int *y, int ny) {
  int j=0;
  if (ny<0) {
    if (negate) {
      for (int i=0; i<nx; i++) {
        if (x[i] != val) {
          out[j] = i;
          j++;
        }
      }
    } else {
      for (int i=0; i<nx; i++) {
        if (x[i] == val) {
          out[j] = i;
          j++;
        }
      }
    }
  } else if (ny>0) {
    if (negate) {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] != val) {
          out[j] = y[i];
          j++;
        }
      }
    } else {
      for (int i=0; i<ny; i++) {
        if (x[y[i]] == val) {
          out[j] = y[i];
          j++;
        }
      }
    }
  }
  nout[0] = j;
}
void which_eq(SEXP x, int nx, int *iwhich, int *nwhich, SEXP val, bool negate, int *y, int ny) {
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  switch (TYPEOF(x)) {
  case LGLSXP: {
    which_eq_int(LOGICAL(x), nx, iwhich, nwhich, LOGICAL(val)[0], negate, y, ny);
  } break;
  case INTSXP: {
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
    Rprintf("which_eq: took %.3fs\n", omp_get_wtime()-tic);
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

  which_eq(x, nx, iwhich, &nwhich, val, LOGICAL(negate)[0], yp, ny);

  for (int i=0; i<nwhich; i++) {
    iwhich[i]++; // shift 0-based index to 1-based index
  }

  SETLENGTH(ans, nwhich);
  UNPROTECT(protecti);
  return ans;
} // used only for benchmarks

/*
 * TODO: which_in, which_like, etc.
 */

/*
 * TODO: which op
 * router for which_eq, which_in, which_like, etc.
 * used so we can optimize: DT[v1==s1 & v2%in%s2 & like(v3,s3)]
 */

/*
 * fast which
 * for a supported type of input: DT[v1==s1 & v2==s2 & v3==s3]
 * for now has to be wrapped with: DT[fwhich(v1==s1 & v2==s2 & v3==s3)]
 * redirects to which_eq calls (or which_op in future)
 */
#define AND_CALL(x) (CAR(x)==sym_and)
//#define OR_CALL(x) (CAR(x)==sym_or) // TODO: not yet used, would allow to support: DT[v1==s1 | v2==s2 | v3==s3] - has to use union instead of intersect short-circuit
#define EQ_CALL(x) (CAR(x)==sym_equal)
#define NEQ_CALL(x) (CAR(x)==sym_nequal)
//#define IN_CALL(x) (CAR(x)==sym_in) // TODO: %in% // requires all C which_in funs
//#define NIN_CALL(x) (CAR(x)==sym_nin) // TODO: %!in% // requires #4152
#define OP_CALL(x) (EQ_CALL(x) || NEQ_CALL(x))
SEXP fwhichOpt(SEXP expr, bool *doOpt) {
  const bool verbose = GetVerbose();
  int protecti = 0;
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  SEXP node = expr;
  bool escape = false;
  SEXP args = R_NilValue, rhs = R_NilValue;
  int nand = 0, nop = 0;
  bool debug = false;
  for (int i=0;; i++) { // examine if supported expression, and investigate number of AND and OP
    if (escape) {
      if (debug) {Rprintf("id=%d, doOpt=0, skip", i);}
      continue;
    }
    if (OP_CALL(node)) {
      if (debug) {Rprintf("i=%d, leaf node of supported OP\n", i);}
      nop++;
      break;
    } else if (AND_CALL(node)) {
      nand++;
      args = CDR(node);
      rhs = CAR(CDR(args));
      if (OP_CALL(rhs)) {
        nop++;
      } else {
        escape = true;
        break;
      }
      node = CAR(args);
    } else {
      escape = true;
      if (debug) {Rprintf("i=%d, unsupported OP, node: ", i); Rf_PrintValue(node);}
      break;
    }
  }
  if (debug) {Rprintf("fwhichOpt: nand=%d, nop=%d\n", nand, nop);}
  if (!escape && nand+1 != nop)
    error("internal error: number of & should be equal to number of OP-1");
  SEXP ans = R_NilValue;
  if (!escape) {
    ans = PROTECT(allocVector(VECSXP, nop)); protecti++;
    for (int i=0; i<nop; i++) {
      if (escape)
        break;
      node = expr;
      if (debug) {Rprintf("i=%d, node: ", i); Rf_PrintValue(node);}
      int limit = 0; // extra stop
      for (int j=0; j<nand-i; j++) {
        limit++;
        if (limit>1000) {
          warning("exceeded 1000 limit in fwitchOpt, please report to data.table issue tracker, together with query used, fallback to R's which");
          escape = true;
          break;
        }
        node = CAR(CDR(node));
        if (debug) {Rprintf("i=%d, j=%d, subsetting node: ", i, j);  Rf_PrintValue(node);}
      }
      if (i > 0)
        node = CAR(CDR(CDR(node)));
      if (debug) {Rprintf("i=%d, final node: ", i);  Rf_PrintValue(node);}
      SET_VECTOR_ELT(ans, i, node);
    }
  }
  doOpt[0] = !escape;
  if (verbose)
    Rprintf("fwhichOpt: took %.3fs\n", omp_get_wtime()-tic);
  UNPROTECT(protecti);
  return ans;
} // turn expression from form of pairlist into list of expressions
SEXP fwhichOptR(SEXP expr) {
  bool doOpt = false;
  SEXP ans = fwhichOpt(expr, &doOpt);
  Rprintf("fwhichOptR: doOpt %d\n", doOpt);
  return ans;
} // only for testing dev
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
      bool negate;
      if (EQ_CALL(e)) {
        negate = false;
      } else if (NEQ_CALL(e)) {
        negate = true;
      } else {
        Rprintf("fwhich: OP call: "); Rf_PrintValue(e);
        error("internal error: could not guess negate argument from OP call, there should be only supported OPs by now, please report to issue tracker");
      }
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
      //which_op(); // route for "equal", "in", "like", also "na"/"nan"->"equal"
      which_eq(
        x, nx, // lhs
        iwhich, &nwhich, // this iter result
        val, // rhs
        negate,
        yp, ny // last iter result
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
  UNPROTECT(protecti);
  if (verbose)
    Rprintf("fwhich: took %.3fs\n", omp_get_wtime()-tic);
  return ans;
}
