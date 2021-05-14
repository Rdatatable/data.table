#include "data.table.h"

SEXP between(SEXP x, SEXP lower, SEXP upper, SEXP incbounds, SEXP NAboundsArg, SEXP checkArg) {
  int nprotect = 0;
  R_len_t nx = length(x), nl = length(lower), nu = length(upper);
  if (!nx || !nl || !nu)
    return (allocVector(LGLSXP, 0));
  const int longest = MAX(MAX(nx, nl), nu);
  if ((nl!=1 && nl!=longest) ||
      (nu!=1 && nu!=longest) ||
      (nx!=1 && nx!=longest)) {
    error(_("Incompatible vector lengths: length(x)==%d length(lower)==%d length(upper)==%d. Each should be either length 1 or the length of the longest."), nx, nl, nu);
  }
  const int longestBound = MAX(nl, nu);  // just for when check=TRUE
  if (!isLogical(incbounds) || LOGICAL(incbounds)[0]==NA_LOGICAL)
    error(_("incbounds must be TRUE or FALSE"));
  const bool open = !LOGICAL(incbounds)[0];
  if (!isLogical(NAboundsArg) || LOGICAL(NAboundsArg)[0]==FALSE)
    error(_("NAbounds must be TRUE or NA"));
  const bool NAbounds = LOGICAL(NAboundsArg)[0]==TRUE;
  if (!isLogical(checkArg) || LOGICAL(checkArg)[0]==NA_LOGICAL)
    error(_("check must be TRUE or FALSE"));
  const bool check = LOGICAL(checkArg)[0];
  const bool verbose = GetVerbose();

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
  }
  if (TYPEOF(lower) != TYPEOF(x)) {
    lower = PROTECT(coerceVector(lower, TYPEOF(x))); nprotect++;
  }
  if (TYPEOF(upper) != TYPEOF(x)) {
    upper = PROTECT(coerceVector(upper, TYPEOF(x))); nprotect++;
  }

  const bool recycleX =   nx==1;
  const bool recycleLow = nl==1;
  const bool recycleUpp = nu==1;
  const int xMask   = recycleX   ? 0 : INT_MAX;
  const int lowMask = recycleLow ? 0 : INT_MAX;
  const int uppMask = recycleUpp ? 0 : INT_MAX;
  SEXP ans = PROTECT(allocVector(LGLSXP, longest)); nprotect++;
  int *restrict ansp = LOGICAL(ans);
  double tic=omp_get_wtime();

  switch (TYPEOF(x)) {
  case INTSXP: {
    const int *lp = INTEGER(lower);
    const int *up = INTEGER(upper);
    const int *xp = INTEGER(x);
    if (check) for (int i=0; i<longestBound; ++i) {
      const int l=lp[i & lowMask], u=up[i & uppMask];
      if (l!=NA_INTEGER && u!=NA_INTEGER && l>u)
        error(_("Item %d of lower (%d) is greater than item %d of upper (%d)"), (i&lowMask)+1, l, (i&uppMask)+1, u);
    }
    if (NAbounds) {  // default NAbounds==TRUE => NA bound means TRUE; i.e. asif lower=-Inf or upper==Inf)
      #pragma omp parallel for num_threads(getDTthreads(longest, true))
      for (int i=0; i<longest; ++i) {
        const int elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
        ansp[i] = elem==NA_INTEGER ? NA_LOGICAL : (l==NA_INTEGER || l+open<=elem) && (u==NA_INTEGER || elem<=u-open);
        // +open so we can always use >= and <=.  NA_INTEGER+1 == -INT_MAX == INT_MIN+1 (so NA limit handled by this too)
      }
    } else {
      #pragma omp parallel for num_threads(getDTthreads(longest, true))
      for (int i=0; i<longest; ++i) {
        const int elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
        if (elem==NA_INTEGER) { ansp[i]=NA_LOGICAL; continue; }
        const bool lok = l!=NA_INTEGER, uok = u!=NA_INTEGER;
        ansp[i] = (lok && uok) ? l+open<=elem && elem<=u-open : ((uok && elem>u-open) || (lok && elem<l+open)) ? FALSE : NA_LOGICAL;
      }
    }
    if (verbose) Rprintf(_("between parallel processing of integer took %8.3fs\n"), omp_get_wtime()-tic);
  } break;

  case REALSXP:
    if (Rinherits(x, char_integer64)) {
      if (!Rinherits(lower, char_integer64) || !Rinherits(upper, char_integer64))
        error(_("x is integer64 but lower and/or upper are not.")); // e.g. between(int64, character, character)
      const int64_t *lp = (int64_t *)REAL(lower);
      const int64_t *up = (int64_t *)REAL(upper);
      const int64_t *xp = (int64_t *)REAL(x);
      if (check) for (int i=0; i<longestBound; ++i) {
        const int64_t l=lp[i & lowMask], u=up[i & uppMask];
        if (l!=NA_INTEGER64 && u!=NA_INTEGER64 && l>u)
          error(_("Item %d of lower (%"PRId64") is greater than item %d of upper (%"PRId64")"), (i&lowMask)+1, l, (i&uppMask)+1, u);
      }
      if (NAbounds) {
        #pragma omp parallel for num_threads(getDTthreads(longest, true))
        for (int i=0; i<longest; ++i) {
          const int64_t elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
          ansp[i] = elem==NA_INTEGER64 ? NA_LOGICAL : (l==NA_INTEGER64 || l+open<=elem) && (u==NA_INTEGER64 || elem<=u-open);
        }
      } else {
        #pragma omp parallel for num_threads(getDTthreads(longest, true))
        for (int i=0; i<longest; ++i) {
          const int64_t elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
          if (elem==NA_INTEGER64) { ansp[i]=NA_LOGICAL; continue; }
          const bool lok = l!=NA_INTEGER64, uok = u!=NA_INTEGER64;
          ansp[i] = (lok && uok) ? l+open<=elem && elem<=u-open : ((uok && elem>u-open) || (lok && elem<l+open)) ? FALSE : NA_LOGICAL;
        }
      }
      if (verbose) Rprintf(_("between parallel processing of integer64 took %8.3fs\n"), omp_get_wtime()-tic);
    } else {
      if (Rinherits(lower, char_integer64) || Rinherits(upper, char_integer64))
        error(_("x is not integer64 but lower and/or upper is integer64. Please align classes."));
      const double *lp = REAL(lower);
      const double *up = REAL(upper);
      const double *xp = REAL(x);
      if (check) for (int i=0; i<longestBound; ++i) {
        const double l=lp[i & lowMask], u=up[i & uppMask];
        if (!isnan(l) && !isnan(u) && l>u)
          error(_("Item %d of lower (%f) is greater than item %d of upper (%f)"), (i&lowMask)+1, l, (i&uppMask)+1, u);
      }
      if (open) {
        if (NAbounds) {
          #pragma omp parallel for num_threads(getDTthreads(longest, true))
          for (int i=0; i<longest; ++i) {
            const double elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
            ansp[i] = isnan(elem) ? NA_LOGICAL : (isnan(l) || l<elem) && (isnan(u) || elem<u);
          }
        } else {
          #pragma omp parallel for num_threads(getDTthreads(longest, true))
          for (int i=0; i<longest; ++i) {
            const double elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
            if (isnan(elem)) { ansp[i]=NA_LOGICAL; continue; }
            const bool lok = !isnan(l), uok = !isnan(u);
            ansp[i] = (lok && uok) ? l<elem && elem<u : ((uok && elem>=u) || (lok && elem<=l)) ? FALSE : NA_LOGICAL;
          }
        }
        if (verbose) Rprintf(_("between parallel processing of double with open bounds took %8.3fs\n"), omp_get_wtime()-tic);
      } else {
        if (NAbounds) {
          #pragma omp parallel for num_threads(getDTthreads(longest, true))
          for (int i=0; i<longest; ++i) {
            const double elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
            ansp[i] = isnan(elem) ? NA_LOGICAL : (isnan(l) || l<=elem) && (isnan(u) || elem<=u);
          }
        } else {
          #pragma omp parallel for num_threads(getDTthreads(longest, true))
          for (int i=0; i<longest; ++i) {
            const double elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
            if (isnan(elem)) { ansp[i]=NA_LOGICAL; continue; }
            const bool lok = !isnan(l), uok = !isnan(u);
            ansp[i] = (lok && uok) ? l<=elem && elem<=u : ((uok && elem>u) || (lok && elem<l)) ? FALSE : NA_LOGICAL;
          }
        }
        if (verbose) Rprintf(_("between parallel processing of double with closed bounds took %8.3fs\n"), omp_get_wtime()-tic);
      }
    }
    break;

  case STRSXP: {
    const SEXP *lp = STRING_PTR(lower);
    const SEXP *up = STRING_PTR(upper);
    const SEXP *xp = STRING_PTR(x);
    #define LCMP (strcmp(CHAR(ENC2UTF8(l)),CHAR(ENC2UTF8(elem)))<=-open)
    #define UCMP (strcmp(CHAR(ENC2UTF8(elem)),CHAR(ENC2UTF8(u)))<=-open)
    // TODO if all ascii can be parallel, otherwise ENC2UTF8 could allocate
    if (check) for (int i=0; i<longestBound; ++i) {
      const SEXP l=lp[i & lowMask], u=up[i & uppMask];
      if (l!=NA_STRING && u!=NA_STRING && l!=u && strcmp(CHAR(ENC2UTF8(l)), CHAR(ENC2UTF8(u)))>0)
        error(_("Item %d of lower ('%s') is greater than item %d of upper ('%s')"), (i&lowMask)+1, CHAR(l), (i&uppMask)+1, CHAR(u));
    }
    if (NAbounds) {
      for (int i=0; i<longest; ++i) {
        const SEXP elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
        ansp[i] = elem==NA_STRING ? NA_LOGICAL : (l==NA_STRING || LCMP) && (u==NA_STRING || UCMP);
      }
    } else {
      for (int i=0; i<longest; ++i) {
        const SEXP elem=xp[i & xMask], l=lp[i & lowMask], u=up[i & uppMask];
        if (elem==NA_STRING) { ansp[i] = NA_LOGICAL; continue; }
        const bool lok=(l!=NA_STRING), uok=(u!=NA_STRING);
        ansp[i] = (lok && uok) ? LCMP && UCMP : ((uok && !UCMP) || (lok && !LCMP)) ? FALSE : NA_LOGICAL;
      }
    }
    if (verbose) Rprintf(_("between non-parallel processing of character took %8.3fs\n"), omp_get_wtime()-tic);
  } break;
  default:
    error(_("Internal error: between.c unsupported type '%s' should have been caught at R level"), type2char(TYPEOF(x)));  // # nocov
  }
  UNPROTECT(nprotect);
  return ans;
}
