#include "data.table.h"

#define NA_INTEGER64 LLONG_MIN
#define MAX_INTEGER64 LLONG_MAX

bool isReallyRealC(SEXP x) {
  if (!isReal(x)) return(false);
  R_xlen_t n=xlength(x), i=0;
  double *dx = REAL(x);
  while (i<n &&
         ( ISNA(dx[i]) ||
         ( R_FINITE(dx[i]) && dx[i] == (int)(dx[i])))) {
    i++;
  }
  return(i<n);
}

SEXP between(SEXP x, SEXP lower, SEXP upper, SEXP bounds) {

  R_len_t nx = length(x), nl = length(lower), nu = length(upper);
  double tic=0.0;
  if (!nx || !nl || !nu)
    return (allocVector(LGLSXP, 0));
  const int longest = MAX(MAX(nx, nl), nu);
  if ((nl!=1 && nl!=longest) ||
      (nu!=1 && nu!=longest) ||
      (nx!=1 && nx!=longest)) {
    error("Incompatible vector lengths: length(x)==%d length(lower)==%d length(upper)==%d. Each should be either length 1 or the length of the longest.", nx, nl, nu);
  }
  if (!isLogical(bounds) || LOGICAL(bounds)[0] == NA_LOGICAL)
    error("incbounds must be logical TRUE/FALSE.");
  const bool verbose = GetVerbose();

  int nprotect = 0;
  bool integer=true;
  bool integer64=false;
  if (isInteger(x) &&         // #3517 coerce to num to int when possible
      (isInteger(lower) || (!isInteger(lower) && !isReallyRealC(lower))) &&
      (isInteger(upper) || (!isInteger(upper) && !isReallyRealC(upper)))) {
    if (!isInteger(lower)) {
      lower = PROTECT(coerceVector(lower, INTSXP)); nprotect++;
    }
    if (!isInteger(upper)) {
      upper = PROTECT(coerceVector(upper, INTSXP)); nprotect++;
    }
  } else if (inherits(x,"integer64")) {
    if (!inherits(lower,"integer64") || !inherits(upper,"integer64"))
      error("Internal error in between: 'x' is integer64 while 'lower' and/or 'upper' are not, should have been catched by now"); // # nocov
    integer=false;
    integer64=true;
  } else if (isReal(x) || isReal(lower) || isReal(upper)) {
    integer=false;
    if (!isReal(x)) {
      x = PROTECT(coerceVector(x, REALSXP)); nprotect++;
    }
    if (!isReal(lower)) {
      lower = PROTECT(coerceVector(lower, REALSXP)); nprotect++; // these coerces will convert NA appropriately
    }
    if (!isReal(upper)) {
      upper = PROTECT(coerceVector(upper, REALSXP)); nprotect++;
    }
  }
  // TODO: sweep through lower and upper ensuring lower<=upper (inc bounds) and no lower>upper or lower==INT_MAX

  const bool recycleX =   nx==1;
  const bool recycleLow = nl==1;
  const bool recycleUpp = nu==1;
  const bool open = !LOGICAL(bounds)[0];
  SEXP ans = PROTECT(allocVector(LGLSXP, longest)); nprotect++;
  int *restrict ansp = LOGICAL(ans);
  if (integer) {
    const int *lp = INTEGER(lower);
    const int *up = INTEGER(upper);
    const int *xp = INTEGER(x);
    if (!recycleX && recycleLow && recycleUpp) {
      const int l = lp[0] + open;  // +open so we can always use >= and <=.  NA_INTEGER+1 == -INT_MAX == INT_MIN+1 (so NA limit handled by this too)
      const int u = up[0]==NA_INTEGER ? INT_MAX : up[0] - open;
      if (verbose) tic = omp_get_wtime();
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<longest; i++) {
        int elem = xp[i];
        ansp[i] = elem==NA_INTEGER ? NA_LOGICAL : (l<=elem && elem<=u);
      }
      if (verbose) Rprintf("between parallel processing of integer with recycling took %8.3fs\n", omp_get_wtime()-tic);
    }
    else {
      const int xMask = recycleX ? 0 : INT_MAX;
      const int lowMask = recycleLow ? 0 : INT_MAX;
      const int uppMask = recycleUpp ? 0 : INT_MAX;
      if (verbose) tic = omp_get_wtime();
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<longest; i++) {
        int elem = xp[i & xMask];
        int l = lp[i & lowMask] +open;
        int u = up[i & uppMask];
        u = (u==NA_INTEGER) ? INT_MAX : u-open;
        ansp[i] = elem==NA_INTEGER ? NA_LOGICAL : (l<=elem && elem<=u);
      }
      if (verbose) Rprintf("between parallel processing of integer took %8.3fs\n", omp_get_wtime()-tic);
    }
  } else if (!integer64) {
    // type real
    const double *lp = REAL(lower);
    const double *up = REAL(upper);
    const double *xp = REAL(x);
    if (!recycleX && recycleLow && recycleUpp) {
      const double l = isnan(lp[0]) ? -INFINITY : lp[0];
      const double u = isnan(up[0]) ?  INFINITY : up[0];
      if (open) {
        if (verbose) tic = omp_get_wtime();
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i=0; i<longest; i++) {
          double elem = xp[i];
          ansp[i] = isnan(elem) ? NA_LOGICAL : (l<elem && elem<u);
        }
        if (verbose) Rprintf("between parallel processing of double using open bounds with recycling took %8.3fs\n", omp_get_wtime()-tic);
      } else {
        if (verbose) tic = omp_get_wtime();
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i=0; i<longest; i++) {
          double elem = xp[i];
          ansp[i] = isnan(elem) ? NA_LOGICAL : (l<=elem && elem<=u);
        }
        if (verbose) Rprintf("between parallel processing of double using closed bounds with recycling took %8.3fs\n", omp_get_wtime()-tic);
      }
    }
    else {
      const int xMask = recycleX ? 0 : INT_MAX;
      const int lowMask = recycleLow ? 0 : INT_MAX;
      const int uppMask = recycleUpp ? 0 : INT_MAX;
      if (verbose) tic = omp_get_wtime();
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<longest; i++) {
        double elem = xp[i & xMask];
        double l = lp[i & lowMask];
        double u = up[i & uppMask];
        if (isnan(l)) l=-INFINITY;
        if (isnan(u)) u= INFINITY;
        ansp[i] = isnan(elem) ? NA_LOGICAL : (open ? l<elem && elem<u : l<=elem && elem<=u);
      }
      if (verbose) Rprintf("between parallel processing of double took %8.3fs\n", omp_get_wtime()-tic);
    }
  } else {
    // type integer64
    const int64_t *lp = (int64_t *)REAL(lower);
    const int64_t *up = (int64_t *)REAL(upper);
    const int64_t *xp = (int64_t *)REAL(x);
    if (!recycleX && recycleLow && recycleUpp) {
      const int64_t l = lp[0] + open; // +open as for int32 branch
      const int64_t u = up[0]==NA_INTEGER64 ? MAX_INTEGER64 : up[0] - open;
      if (verbose) tic = omp_get_wtime();
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<longest; i++) {
        int64_t elem = xp[i];
        ansp[i] = elem==NA_INTEGER64 ? NA_LOGICAL : (l<=elem && elem<=u);
      }
      if (verbose) Rprintf("between parallel processing of integer64 with recycling took %8.3fs\n", omp_get_wtime()-tic);
    }
    else {
      const int xMask = recycleX ? 0 : INT_MAX;
      const int lowMask = recycleLow ? 0 : INT_MAX;
      const int uppMask = recycleUpp ? 0 : INT_MAX;
      if (verbose) tic = omp_get_wtime();
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<longest; i++) {
        int64_t elem = xp[i & xMask];
        int64_t l = lp[i & lowMask];
        int64_t u = up[i & uppMask];
        u = u==NA_INTEGER64 ? MAX_INTEGER64 : u-open;
        ansp[i] = elem==NA_INTEGER64 ? NA_LOGICAL : (l<=elem && elem<=u);
      }
      if (verbose) Rprintf("between parallel processing of integer64 took %8.3fs\n", omp_get_wtime()-tic);
    }
  }
  UNPROTECT(nprotect);
  return(ans);
}

