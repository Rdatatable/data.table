#include "data.table.h"

SEXP between(SEXP x, SEXP lower, SEXP upper, SEXP incbounds, SEXP NAbounds) {

  int nprotect = 0;
  R_len_t nx = length(x), nl = length(lower), nu = length(upper);
  if (!nx || !nl || !nu)
    return (allocVector(LGLSXP, 0));
  const int longest = MAX(MAX(nx, nl), nu);
  if ((nl!=1 && nl!=longest) ||
      (nu!=1 && nu!=longest) ||
      (nx!=1 && nx!=longest)) {
    error("Incompatible vector lengths: length(x)==%d length(lower)==%d length(upper)==%d. Each should be either length 1 or the length of the longest.", nx, nl, nu);
  }
  if (!isLogical(incbounds) || LOGICAL(incbounds)[0]==NA_LOGICAL)
    error("incbounds must be logical TRUE or FALSE");
  if (!isLogical(NAbounds) || LOGICAL(NAbounds)[0]==FALSE)
    error("NAbounds must be logical TRUE or NA");
  const bool verbose = GetVerbose();

  //bool integer=true;
  //bool integer64=false;
  if (isInteger(x)) {
    if ((isInteger(lower) || isRealReallyInt(lower)) &&
        (isInteger(upper) || isRealReallyInt(upper))) { // #3517 coerce to num to int when possible
      if (!isInteger(lower)) {
        lower = PROTECT(coerceVector(lower, INTSXP)); nprotect++;
      }
      if (!isInteger(upper)) {
        upper = PROTECT(coerceVector(upper, INTSXP)); nprotect++;
      }
    } else { // #3565
      x = PROTECT(coerceVector(x, REALSXP)); nprotect++;
    }
  } else if (isReal(x)) {
    if (INHERITS(x,char_integer64) || INHERITS(x,char_nanotime)) {
      if (INHERITS(x,char_integer64) && (!INHERITS(lower,char_integer64) || !INHERITS(upper,char_integer64)))
        error("Internal error in between: 'x' is integer64 while 'lower' and/or 'upper' are not, should have been caught by now"); // # nocov
      if (INHERITS(x,char_nanotime) && (!INHERITS(lower,char_nanotime) || !INHERITS(upper,char_nanotime)))
        error("Internal error in between: 'x' is nanotime while 'lower' and/or 'upper' are not, should have been caught by now"); // # nocov
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
  const int xMask   = recycleX   ? 0 : INT_MAX;
  const int lowMask = recycleLow ? 0 : INT_MAX;
  const int uppMask = recycleUpp ? 0 : INT_MAX;
  const bool open = !LOGICAL(incbounds)[0];
  const bool NAboundsNA = LOGICAL(NAbounds)[0]==NA_INTEGER;
  SEXP ans = PROTECT(allocVector(LGLSXP, longest)); nprotect++;
  int *restrict ansp = LOGICAL(ans);
  double tic=omp_get_wtime();
  switch (TYPEOF(x)) {
  case INTSXP: {
    const int *lp = INTEGER(lower);
    const int *up = INTEGER(upper);
    const int *xp = INTEGER(x);
    #pragma omp parallel for num_threads(getDTthreads())
    for (int i=0; i<longest; i++) {
      int elem = xp[i & xMask];
      int    l = lp[i & lowMask];
      int    u = up[i & uppMask];
      ansp[i] = (elem==NA_INTEGER || (NAboundsNA && (l==NA_INTEGER || u==NA_INTEGER))) ? NA_LOGICAL : l+open<=elem && (u==NA_INTEGER || elem<=u-open);
      // +open so we can always use >= and <=.  NA_INTEGER+1 == -INT_MAX == INT_MIN+1 (so NA limit handled by this too)
      // one relatively complex condition here (using one ?:, short-circuit '&&', and const bool NAboundsNA (predicted away)) should be
      //   as fast as specialized extra loops but with simplicity, correctness & coverage benefits.
    }
    if (verbose) Rprintf("between parallel processing of integer took %8.3fs\n", omp_get_wtime()-tic);
  } break;
  case REALSXP:
    if (INHERITS(x, char_integer64)) {
      const int64_t *lp = (int64_t *)REAL(lower);
      const int64_t *up = (int64_t *)REAL(upper);
      const int64_t *xp = (int64_t *)REAL(x);
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<longest; i++) {
        int64_t elem = xp[i & xMask];
        int64_t    l = lp[i & lowMask];
        int64_t    u = up[i & uppMask];
        ansp[i] = (elem==NA_INTEGER64 || (NAboundsNA && (l==NA_INTEGER64 || u==NA_INTEGER64))) ? NA_LOGICAL : l+open<=elem && (u==NA_INTEGER64 || elem<=u-open);
      }
      if (verbose) Rprintf("between parallel processing of integer64 took %8.3fs\n", omp_get_wtime()-tic);
    } else {
      const double *lp = REAL(lower);
      const double *up = REAL(upper);
      const double *xp = REAL(x);
      if (open) {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i=0; i<longest; i++) {
          double elem = xp[i & xMask];
          double    l = lp[i & lowMask];
          double    u = up[i & uppMask];
          ansp[i] = (isnan(elem) || (NAboundsNA && (isnan(l) || isnan(u)))) ? NA_LOGICAL : (isnan(l) || l<elem) && (isnan(u) || elem<u);
        }
        if (verbose) Rprintf("between parallel processing of double with open bounds took %8.3fs\n", omp_get_wtime()-tic);
      } else {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i=0; i<longest; i++) {
          double elem = xp[i & xMask];
          double    l = lp[i & lowMask];
          double    u = up[i & uppMask];
          ansp[i] = (isnan(elem) || (NAboundsNA && (isnan(l) || isnan(u)))) ? NA_LOGICAL : (isnan(l) || l<=elem) && (isnan(u) || elem<=u);
        }
        if (verbose) Rprintf("between parallel processing of double with closed bounds took %8.3fs\n", omp_get_wtime()-tic);
      }
    }
    break;
  case STRSXP: {
    const SEXP *lp = STRING_PTR(lower);
    const SEXP *up = STRING_PTR(upper);
    const SEXP *xp = STRING_PTR(x);
    for (int i=0; i<longest; i++) {
      const SEXP elem = xp[i & xMask];
      const SEXP    l = lp[i & lowMask];
      const SEXP    u = up[i & uppMask];
      ansp[i] = (elem==NA_STRING || (NAboundsNA && (l==NA_STRING || u==NA_STRING))) ? NA_LOGICAL :
                 (l==NA_STRING || strcmp(CHAR(ENC2UTF8(l)),CHAR(ENC2UTF8(elem)))<=-open) &&
                 (u==NA_STRING || strcmp(CHAR(ENC2UTF8(elem)),CHAR(ENC2UTF8(u)))<=-open);
      // ENC2UTF8 could allocate so cannot be parallel (TODO: if all ascii, can be parallel) TODO: needs protection
    }
    if (verbose) Rprintf("between non-parallel processing of character took %8.3fs\n", omp_get_wtime()-tic);
  } break;
  default:
    error("Unsupported type '%s'", type2char(TYPEOF(x)));
  }
  UNPROTECT(nprotect);
  return ans;
}
