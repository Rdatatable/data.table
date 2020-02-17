#include "data.table.h"
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

// TO DO: margins
SEXP fcast(SEXP lhs, SEXP val, SEXP nrowArg, SEXP ncolArg, SEXP idxArg, SEXP fill, SEXP fill_d, SEXP is_agg) {
  int nrows=INTEGER(nrowArg)[0], ncols=INTEGER(ncolArg)[0];
  int nlhs=length(lhs), nval=length(val), *idx = INTEGER(idxArg);
  SEXP target;

  SEXP ans = PROTECT(allocVector(VECSXP, nlhs + (nval * ncols)));
  // set lhs cols
  for (int i=0; i<nlhs; ++i) {
    SET_VECTOR_ELT(ans, i, VECTOR_ELT(lhs, i));
  }
  // get val cols
  for (int i=0; i<nval; ++i) {
    SEXP thiscol = VECTOR_ELT(val, i);
    SEXP thisfill = fill;
    int nprotect = 0;
    if (isNull(fill)) {
      if (LOGICAL(is_agg)[0]) {
        thisfill = PROTECT(allocNAVector(TYPEOF(thiscol), 1)); nprotect++;
      } else thisfill = VECTOR_ELT(fill_d, i);
    }
    if (TYPEOF(thisfill) != TYPEOF(thiscol)) {
      thisfill = PROTECT(coerceVector(thisfill, TYPEOF(thiscol))); nprotect++;
    }
    switch (TYPEOF(thiscol)) {
    case INTSXP:
    case LGLSXP: {
      const int *ithiscol = INTEGER(thiscol);
      const int *ithisfill = INTEGER(thisfill);
      for (int j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target=allocVector(TYPEOF(thiscol), nrows) );
        int *itarget = INTEGER(target);
        copyMostAttrib(thiscol, target);
        for (int k=0; k<nrows; ++k) {
          int thisidx = idx[k*ncols + j];
          itarget[k] = (thisidx == NA_INTEGER) ? ithisfill[0] : ithiscol[thisidx-1];
        }
      }
    } break;
    case REALSXP: {
      const double *dthiscol = REAL(thiscol);
      const double *dthisfill = REAL(thisfill);
      for (int j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target=allocVector(TYPEOF(thiscol), nrows) );
        double *dtarget = REAL(target);
        copyMostAttrib(thiscol, target);
        for (int k=0; k<nrows; ++k) {
          int thisidx = idx[k*ncols + j];
          dtarget[k] = (thisidx == NA_INTEGER) ? dthisfill[0] : dthiscol[thisidx-1];
        }
      }
    } break;
    case STRSXP:
      for (int j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target=allocVector(TYPEOF(thiscol), nrows) );
        copyMostAttrib(thiscol, target);
        for (int k=0; k<nrows; ++k) {
          int thisidx = idx[k*ncols + j];
          SET_STRING_ELT(target, k, (thisidx == NA_INTEGER) ? STRING_ELT(thisfill, 0) : STRING_ELT(thiscol, thisidx-1));
        }
      }
      break;
    case VECSXP:
      for (int j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target=allocVector(TYPEOF(thiscol), nrows) );
        copyMostAttrib(thiscol, target);
        for (int k=0; k<nrows; ++k) {
          int thisidx = idx[k*ncols + j];
          SET_VECTOR_ELT(target, k, (thisidx == NA_INTEGER) ? VECTOR_ELT(thisfill, 0) : VECTOR_ELT(thiscol, thisidx-1));
        }
      }
      break;
    default: error(_("Unsupported column type in fcast val: '%s'"), type2char(TYPEOF(thiscol))); // #nocov
    }
    UNPROTECT(nprotect);
  }
  UNPROTECT(1);
  return(ans);
}

// commenting all unused functions, but not deleting it, just in case

// // internal functions that are not used anymore..

// // # nocov start
// // Note: all these functions below are internal functions and are designed specific to fcast.
// SEXP zero_init(R_len_t n) {
//   R_len_t i;
//   SEXP ans;
//   if (n < 0) error(_("Input argument 'n' to 'zero_init' must be >= 0"));
//   ans = PROTECT(allocVector(INTSXP, n));
//   for (i=0; i<n; i++) INTEGER(ans)[i] = 0;
//   UNPROTECT(1);
//   return(ans);
// }

// SEXP cast_order(SEXP v, SEXP env) {
//   R_len_t len;
//   SEXP call, ans;
//   if (TYPEOF(env) != ENVSXP) error(_("Argument 'env' to (data.table internals) 'cast_order' must be an environment"));
//   if (TYPEOF(v) == VECSXP) len = length(VECTOR_ELT(v, 0));
//   else len = length(v);
//   PROTECT(call = lang2(install("forder"), v)); // TODO: save the 'eval' by calling directly the C-function.
//   ans = PROTECT(eval(call, env));
//   if (length(ans) == 0) { // forder returns integer(0) if already sorted
//     UNPROTECT(1); // ans
//     ans = PROTECT(seq_int(len, 1));
//   }
//   UNPROTECT(2);
//   return(ans);
// }

// SEXP cross_join(SEXP s, SEXP env) {
//   // Calling CJ is faster and don't have to worry about sorting or setting key.
//   SEXP call, r;
//   if (!isNewList(s) || isNull(s)) error(_("Argument 's' to 'cross_join' must be a list of length > 0"));
//   PROTECT(call = lang3(install("do.call"), install("CJ"), s));
//   r = eval(call, env);
//   UNPROTECT(1);
//   return(r);
// }

// SEXP diff_int(SEXP x, R_len_t n) {
//   R_len_t i;
//   SEXP ans;
//   if (TYPEOF(x) != INTSXP) error(_("Argument 'x' to 'diff_int' must be an integer vector"));
//   ans = PROTECT(allocVector(INTSXP, length(x)));
//   for (i=1; i<length(x); i++)
//     INTEGER(ans)[i-1] = INTEGER(x)[i] - INTEGER(x)[i-1];
//   INTEGER(ans)[length(x)-1] = n - INTEGER(x)[length(x)-1] + 1;
//   UNPROTECT(1);
//   return(ans);
// }

// SEXP intrep(SEXP x, SEXP len) {
//   R_len_t i,j,l=0, k=0;
//   SEXP ans;
//   if (TYPEOF(x) != INTSXP || TYPEOF(len) != INTSXP) error(_("Arguments 'x' and 'len' to 'intrep' should both be integer vectors"));
//   if (length(x) != length(len)) error(_("'x' and 'len' must be of same length"));
//   // assuming both are of length >= 1
//   for (i=0; i<length(len); i++)
//     l += INTEGER(len)[i]; // assuming positive values for len. internal use - can't bother to check.
//   ans = PROTECT(allocVector(INTSXP, l));
//   for (i=0; i<length(len); i++) {
//     for (j=0; j<INTEGER(len)[i]; j++) {
//       INTEGER(ans)[k++] = INTEGER(x)[i];
//     }
//   }
//   UNPROTECT(1); // ans
//   return(ans);
// }

// // taken match_transform() from base:::unique.c and modified
// SEXP coerce_to_char(SEXP s, SEXP env)
// {
//   if (OBJECT(s)) {
//     if (inherits(s, "factor")) return asCharacterFactor(s);
//     else if(getAttrib(s, R_ClassSymbol) != R_NilValue) {
//       SEXP call, r;
//       PROTECT(call = lang2(install("as.character"), s));
//       r = eval(call, env);
//       UNPROTECT(1);
//       return r;
//     }
//   }
//   /* else */
//   return coerceVector(s, STRSXP);
// }

// // # nocov end
