#include "data.table.h"
#include <Rdefines.h>

static double l=0.0, u=0.0;

Rboolean int_upper_closed(SEXP x, R_len_t i) {
    return (INTEGER(x)[i] == NA_INTEGER || (double)INTEGER(x)[i] <= u ? NA_LOGICAL : FALSE);
}

Rboolean int_upper_open(SEXP x, R_len_t i) {
    return (INTEGER(x)[i] == NA_INTEGER || (double)INTEGER(x)[i] < u ? NA_LOGICAL : FALSE);
}

Rboolean int_lower_closed(SEXP x, R_len_t i) {
    return (INTEGER(x)[i] == NA_INTEGER || (double)INTEGER(x)[i] >= l ? NA_LOGICAL : FALSE);
}

Rboolean int_lower_open(SEXP x, R_len_t i) {
    return (INTEGER(x)[i] == NA_INTEGER || (double)INTEGER(x)[i] > l ? NA_LOGICAL : FALSE);
}

Rboolean int_both_closed(SEXP x, R_len_t i) {
    return (INTEGER(x)[i] == NA_INTEGER ? NA_LOGICAL : ((double)INTEGER(x)[i] >= l && (double)INTEGER(x)[i] <= u));
}

Rboolean int_both_open(SEXP x, R_len_t i) {
    return (INTEGER(x)[i] == NA_INTEGER ? NA_LOGICAL : ((double)INTEGER(x)[i] > l && (double)INTEGER(x)[i] < u));
}

Rboolean double_upper_closed(SEXP x, R_len_t i) {
    return (ISNAN(REAL(x)[i]) || REAL(x)[i] <= u ? NA_LOGICAL : FALSE);
}

Rboolean double_upper_open(SEXP x, R_len_t i) {
    return (ISNAN(REAL(x)[i]) || REAL(x)[i] < u ? NA_LOGICAL : FALSE);
}

Rboolean double_lower_closed(SEXP x, R_len_t i) {
    return (ISNAN(REAL(x)[i]) || REAL(x)[i] >= l ? NA_LOGICAL : FALSE);
}

Rboolean double_lower_open(SEXP x, R_len_t i) {
    return (ISNAN(REAL(x)[i]) || REAL(x)[i] > l ? NA_LOGICAL : FALSE);
}

Rboolean double_both_closed(SEXP x, R_len_t i) {
    return (ISNAN(REAL(x)[i]) ? NA_LOGICAL : (REAL(x)[i] >= l && REAL(x)[i] <= u));
}

Rboolean double_both_open(SEXP x, R_len_t i) {
    return (ISNAN(REAL(x)[i]) ? NA_LOGICAL : (REAL(x)[i] > l && REAL(x)[i] < u));
}

SEXP between(SEXP x, SEXP lower, SEXP upper, SEXP bounds) {

    R_len_t i, nx = length(x), nl = length(lower), nu = length(upper);
    l = 0.0; u = 0.0;
    SEXP ans;
    Rboolean (*flower)(), (*fupper)(), (*fboth)();
    if (!nx || !nl || !nu)
        return (allocVector(LGLSXP, 0));
    if (nl != 1 && nl != nx)
        error("length(lower) (%d) must be either 1 or length(x) (%d)", nl, nx);
    if (nu != 1 && nu != nx)
        error("length(upper) (%d) must be either 1 or length(x) (%d)", nu, nx);
    if (!isLogical(bounds) || LOGICAL(bounds)[0] == NA_LOGICAL)
        error("incbounds must be logical TRUE/FALSE.");

    // no support for int64 yet (only handling most common cases)
    // coerce to also get NA values properly
    lower = PROTECT(coerceVector(lower, REALSXP)); l = REAL(lower)[0];
    upper = PROTECT(coerceVector(upper, REALSXP)); u = REAL(upper)[0];
    ans   = PROTECT(allocVector(LGLSXP, nx));

    if (LOGICAL(bounds)[0]) {
        fupper = isInteger(x) ? &int_upper_closed : &double_upper_closed;
        flower = isInteger(x) ? &int_lower_closed : &double_lower_closed;
        fboth  = isInteger(x) ? &int_both_closed  : &double_both_closed;
    } else {
        fupper = isInteger(x) ? &int_upper_open : &double_upper_open;
        flower = isInteger(x) ? &int_lower_open : &double_lower_open;
        fboth  = isInteger(x) ? &int_both_open  : &double_both_open;
    }

    if ( ISNAN(REAL(lower)[0]) ) {
        if ( ISNAN(REAL(upper)[0]) ) {
            #pragma omp parallel for num_threads(getDTthreads())
            for (i=0; i<nx; i++) LOGICAL(ans)[i] = NA_LOGICAL;
        } else {
            #pragma omp parallel for num_threads(getDTthreads())
            for (i=0; i<nx; i++) LOGICAL(ans)[i] = fupper(x, i);
        }
    } else {
        if ( ISNAN(REAL(upper)[0]) ) {
            #pragma omp parallel for num_threads(getDTthreads())
            for (i=0; i<nx; i++) LOGICAL(ans)[i] = flower(x, i);
        } else {
            #pragma omp parallel for num_threads(getDTthreads())
            for (i=0; i<nx; i++) LOGICAL(ans)[i] = fboth(x, i);
        }
    }
    UNPROTECT(3);
    return(ans);
}
