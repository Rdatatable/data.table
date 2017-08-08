#include "data.table.h"
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

// TO DO: margins
SEXP fcast(SEXP lhs, SEXP val, SEXP nrowArg, SEXP ncolArg, SEXP idxArg, SEXP fill, SEXP fill_d, SEXP is_agg) {

    int nrows=INTEGER(nrowArg)[0], ncols=INTEGER(ncolArg)[0];
    int i,j,k, nlhs=length(lhs), nval=length(val), *idx = INTEGER(idxArg), thisidx;;
    SEXP thiscol, target, ans, thisfill;
    Rboolean isfill = TRUE, count;

    ans = PROTECT(allocVector(VECSXP, nlhs + (nval * ncols)));
    // set lhs cols
    for (i=0; i < nlhs; i++) {
        SET_VECTOR_ELT(ans, i, VECTOR_ELT(lhs, i));
    }
    // get val cols
    for (i=0; i<nval; i++) {
        thiscol = VECTOR_ELT(val, i);
        thisfill = fill;
        count = FALSE;
        if (isNull(fill)) {
            isfill = FALSE;
            if (LOGICAL(is_agg)[0]) {
                thisfill = PROTECT(allocNAVector(TYPEOF(thiscol), 1));
                count = TRUE;
            } else thisfill = VECTOR_ELT(fill_d, i);
        }
        if (isfill && TYPEOF(fill) != TYPEOF(thiscol)) {
            thisfill = PROTECT(coerceVector(fill, TYPEOF(thiscol)));
            count = TRUE;
        }
        switch (TYPEOF(thiscol)) {
            case INTSXP:
                for (j=0; j<ncols; j++) {
                    target = allocVector(TYPEOF(thiscol), nrows);
                    SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target);
                    copyMostAttrib(thiscol, target);
                    for (k=0; k<nrows; k++) {
                        thisidx = idx[k*ncols + j];
                        INTEGER(target)[k] = (thisidx == NA_INTEGER) ? INTEGER(thisfill)[0] : INTEGER(thiscol)[thisidx-1];
                    }
                }
            break;
            case REALSXP:
                for (j=0; j<ncols; j++) {
                    target = allocVector(TYPEOF(thiscol), nrows);
                    SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target);
                    copyMostAttrib(thiscol, target);
                    for (k=0; k<nrows; k++) {
                        thisidx = idx[k*ncols + j];
                        REAL(target)[k] = (thisidx == NA_INTEGER) ? REAL(thisfill)[0] : REAL(thiscol)[thisidx-1];
                    }
                }
            break;
            case LGLSXP:
                for (j=0; j<ncols; j++) {
                    target = allocVector(TYPEOF(thiscol), nrows);
                    SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target);
                    copyMostAttrib(thiscol, target);
                    for (k=0; k<nrows; k++) {
                        thisidx = idx[k*ncols + j];
                        LOGICAL(target)[k] = (thisidx == NA_INTEGER) ? LOGICAL(thisfill)[0] : LOGICAL(thiscol)[thisidx-1];
                    }
                }
            break;
            case STRSXP:
                for (j=0; j<ncols; j++) {
                    target = allocVector(TYPEOF(thiscol), nrows);
                    SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target);
                    copyMostAttrib(thiscol, target);
                    for (k=0; k<nrows; k++) {
                        thisidx = idx[k*ncols + j];
                        SET_STRING_ELT(target, k, (thisidx == NA_INTEGER) ? STRING_ELT(thisfill, 0) : STRING_ELT(thiscol, thisidx-1));
                    }
                }
            break;
            case VECSXP:
                for (j=0; j<ncols; j++) {
                    target = allocVector(TYPEOF(thiscol), nrows);
                    SET_VECTOR_ELT(ans, nlhs+j+i*ncols, target);
                    copyMostAttrib(thiscol, target);
                    for (k=0; k<nrows; k++) {
                        thisidx = idx[k*ncols + j];
                        SET_VECTOR_ELT(target, k, (thisidx == NA_INTEGER) ? VECTOR_ELT(thisfill, 0) : VECTOR_ELT(thiscol, thisidx-1));
                    }
                }
            break;
        }
        if (count) UNPROTECT(1);
    }
    UNPROTECT(1);
    return(ans);
}

// used in bmerge.c
SEXP int_vec_init(R_len_t n, int val) {
    // Matt removed the SEXP val input because the ScalarInteger() input wasn't PROTECTed in caller
    // and was potentially leaking/crashing.
    // This function only used once and only for int, so to remove untested lines (code-coverage)
    // decided to simplify it rather than add the PROTECT and UNPROTECT in caller.
    if (n < 0) error("Input argument 'n' to 'int_vec_init' must be >= 0");
    SEXP ans = PROTECT(allocVector(INTSXP, n));
    for (R_len_t i=0; i<n; i++) INTEGER(ans)[i] = val;
    UNPROTECT(1);
    return(ans);
}

// commenting all unused functions, but not deleting it, just in case

// // internal functions that are not used anymore..

// // # nocov start
// // Note: all these functions below are internal functions and are designed specific to fcast.
// SEXP zero_init(R_len_t n) {
//     R_len_t i;
//     SEXP ans;
//     if (n < 0) error("Input argument 'n' to 'zero_init' must be >= 0");
//     ans = PROTECT(allocVector(INTSXP, n));
//     for (i=0; i<n; i++) INTEGER(ans)[i] = 0;
//     UNPROTECT(1);
//     return(ans);
// }

// SEXP cast_order(SEXP v, SEXP env) {
//     R_len_t len;
//     SEXP call, ans;
//     if (TYPEOF(env) != ENVSXP) error("Argument 'env' to (data.table internals) 'cast_order' must be an environment");
//     if (TYPEOF(v) == VECSXP) len = length(VECTOR_ELT(v, 0));
//     else len = length(v);
//     PROTECT(call = lang2(install("forder"), v)); // TODO: save the 'eval' by calling directly the C-function.
//     ans = PROTECT(eval(call, env));
//     if (length(ans) == 0) { // forder returns integer(0) if already sorted
//         UNPROTECT(1); // ans
//         ans = PROTECT(seq_int(len, 1));
//     }
//     UNPROTECT(2);
//     return(ans);
// }

// SEXP cross_join(SEXP s, SEXP env) {
//     // Calling CJ is faster and don't have to worry about sorting or setting key.
//     SEXP call, r;
//     if (!isNewList(s) || isNull(s)) error("Argument 's' to 'cross_join' must be a list of length > 0");
//     PROTECT(call = lang3(install("do.call"), install("CJ"), s));
//     r = eval(call, env);
//     UNPROTECT(1);
//     return(r);
// }

// SEXP diff_int(SEXP x, R_len_t n) {

//     R_len_t i;
//     SEXP ans;
//     if (TYPEOF(x) != INTSXP) error("Argument 'x' to 'diff_int' must be an integer vector");
//     ans = PROTECT(allocVector(INTSXP, length(x)));
//     for (i=1; i<length(x); i++)
//         INTEGER(ans)[i-1] = INTEGER(x)[i] - INTEGER(x)[i-1];
//     INTEGER(ans)[length(x)-1] = n - INTEGER(x)[length(x)-1] + 1;
//     UNPROTECT(1);
//     return(ans);
// }

// SEXP intrep(SEXP x, SEXP len) {

//     R_len_t i,j,l=0, k=0;
//     SEXP ans;
//     if (TYPEOF(x) != INTSXP || TYPEOF(len) != INTSXP) error("Arguments 'x' and 'len' to 'intrep' should both be integer vectors");
//     if (length(x) != length(len)) error("'x' and 'len' must be of same length");
//     // assuming both are of length >= 1
//     for (i=0; i<length(len); i++)
//         l += INTEGER(len)[i]; // assuming positive values for len. internal use - can't bother to check.
//     ans = PROTECT(allocVector(INTSXP, l));
//     for (i=0; i<length(len); i++) {
//         for (j=0; j<INTEGER(len)[i]; j++) {
//             INTEGER(ans)[k++] = INTEGER(x)[i];
//         }
//     }
//     UNPROTECT(1); // ans
//     return(ans);
// }

// // taken match_transform() from base:::unique.c and modified
// SEXP coerce_to_char(SEXP s, SEXP env)
// {
//     if(OBJECT(s)) {
//     if(inherits(s, "factor")) return asCharacterFactor(s);
//     else if(getAttrib(s, R_ClassSymbol) != R_NilValue) {
//         SEXP call, r;
//         PROTECT(call = lang2(install("as.character"), s));
//         r = eval(call, env);
//         UNPROTECT(1);
//         return r;
//     }
//     }
//     /* else */
//     return coerceVector(s, STRSXP);
// }

// // # nocov end
