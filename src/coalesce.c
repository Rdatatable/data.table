#include "data.table.h"

/*
  OpenMP is used here to parallelize:
    - Row-wise coalescing
    - NA replacement
*/

SEXP coalesce(SEXP x, SEXP inplaceArg, SEXP nan_is_na_arg)
{
  if (TYPEOF(x) != VECSXP)
    internal_error(__func__, "input is list(...) at R level");
  if (!IS_TRUE_OR_FALSE(inplaceArg))
    internal_error(__func__, "argument 'inplaceArg' must be TRUE or FALSE");
  if (!IS_TRUE_OR_FALSE(nan_is_na_arg))
    internal_error(__func__, "argument 'nan_is_na_arg' must be TRUE or FALSE");

  const bool inplace   = LOGICAL(inplaceArg)[0];
  const bool nan_is_na = LOGICAL(nan_is_na_arg)[0];
  const bool verbose   = GetVerbose();
  int nprotect = 0;

  if (length(x) == 0 || isNull(VECTOR_ELT(x, 0)))
    return R_NilValue;

  SEXP first;
  int off = 1;

  if (TYPEOF(VECTOR_ELT(x, 0)) == VECSXP) {
    if (length(x) != 1)
      error(_("The first argument is a list/data.table/data.frame. No other args allowed."));
    x = VECTOR_ELT(x, 0);
    if (length(x) == 0)
      return R_NilValue;
    first = VECTOR_ELT(x, 0);
  } else {
    first = VECTOR_ELT(x, 0);
    if (length(x) > 1 && TYPEOF(VECTOR_ELT(x, 1)) == VECSXP) {
      x = VECTOR_ELT(x, 1);
      off = 0;
    }
  }

  const int nval = length(x) - off;
  if (nval == 0)
    return first;

  const int nrow = length(first);

  /* ------------------------------------------------------------
     DATE / IDATE NORMALIZATION
     ------------------------------------------------------------ */

  bool any_date_like =
    INHERITS(first, char_Date) || INHERITS(first, char_IDate);

  for (int i = 0; i < nval && !any_date_like; ++i) {
    SEXP item = VECTOR_ELT(x, i + off);
    if (INHERITS(item, char_Date) || INHERITS(item, char_IDate))
      any_date_like = true;
  }

  if (any_date_like) {
    /* Output contract: Date with REAL storage */
    if (!INHERITS(first, char_Date) || TYPEOF(first) != REALSXP) {
      first = PROTECT(coerceVector(first, REALSXP));
      setAttrib(first, R_ClassSymbol, char_Date);
      nprotect++;
    }

    /* Normalize all inputs to Date + REAL */
    for (int i = 0; i < nval; ++i) {
      SEXP item = VECTOR_ELT(x, i + off);
      if (!INHERITS(item, char_Date) || TYPEOF(item) != REALSXP) {
        SEXP tmp = PROTECT(coerceVector(item, REALSXP));
        setAttrib(tmp, R_ClassSymbol, char_Date);
        SET_VECTOR_ELT(x, i + off, tmp);
        nprotect++;
      }
    }
  }

  /* ------------------------------------------------------------
     TYPE / CLASS VALIDATION
     ------------------------------------------------------------ */

  for (int i = 0; i < nval; ++i) {
    SEXP item = VECTOR_ELT(x, i + off);

    if (TYPEOF(first) != TYPEOF(item)) {
      if ((TYPEOF(first) == INTSXP && TYPEOF(item) == REALSXP) ||
          (TYPEOF(first) == REALSXP && TYPEOF(item) == INTSXP)) {
        if (TYPEOF(first) == INTSXP) {
          first = PROTECT(coerceVector(first, REALSXP));
          nprotect++;
        }
      } else {
        error(_("Item %d is type %s but first item is type %s. Please coerce."),
              i + 2, type2char(TYPEOF(item)), type2char(TYPEOF(first)));
      }
    }

    if (!any_date_like) {
      if (!R_compute_identical(PROTECT(getAttrib(first, R_ClassSymbol)),
                               PROTECT(getAttrib(item, R_ClassSymbol)), 0))
        error(_("Item %d has a different class than item 1."), i + 2);
      UNPROTECT(2);
    }

    if (length(item) != 1 && length(item) != nrow)
      error(_("Item %d length mismatch."), i + 2);
  }

  if (!inplace) {
    first = PROTECT(copyAsPlain(first));
    nprotect++;
    if (verbose)
      Rprintf(_("coalesce copied first item (inplace=FALSE)\n"));
  }

  const void **valP = (const void **)R_alloc(nval, sizeof(*valP));

  /* ------------------------------------------------------------
     COALESCING (SCALAR-SAFE, NO UB)
     ------------------------------------------------------------ */

  switch (TYPEOF(first)) {

  case LGLSXP:
  case INTSXP: {
    int *xP = INTEGER(first);
    int finalVal = NA_INTEGER;
    bool hasFinal = false;
    int k = 0;

    for (int j = 0; j < nval; ++j) {
      SEXP item = VECTOR_ELT(x, j + off);
      if (length(item) == 1) {
        int v = INTEGER(item)[0];
        if (v != NA_INTEGER && !hasFinal) {
          finalVal = v;
          hasFinal = true;
        }
      } else {
        valP[k++] = INTEGER_RO(item);
      }
    }

#pragma omp parallel for num_threads(getDTthreads(nrow, true))
    for (int i = 0; i < nrow; ++i) {
      if (xP[i] != NA_INTEGER) continue;

      int v = NA_INTEGER;
      for (int j = 0; j < k && v == NA_INTEGER; ++j)
        v = ((int *)valP[j])[i];

      if (v != NA_INTEGER) xP[i] = v;
      else if (hasFinal)  xP[i] = finalVal;
    }
  } break;

  case REALSXP: {
    double *xP = REAL(first);
    double finalVal = NA_REAL;
    bool hasFinal = false;
    int k = 0;

    for (int j = 0; j < nval; ++j) {
      SEXP item = VECTOR_ELT(x, j + off);
      if (length(item) == 1) {
        double v = REAL(item)[0];
        if (!ISNA(v) && (!nan_is_na || !ISNAN(v)) && !hasFinal) {
          finalVal = v;
          hasFinal = true;
        }
      } else {
        valP[k++] = REAL_RO(item);
      }
    }

#pragma omp parallel for num_threads(getDTthreads(nrow, true))
    for (int i = 0; i < nrow; ++i) {
      double v = xP[i];
      if (!ISNA(v) && (!nan_is_na || !ISNAN(v))) continue;

      for (int j = 0; j < k && (ISNA(v) || (nan_is_na && ISNAN(v))); ++j)
        v = ((double *)valP[j])[i];

      if (!ISNA(v) && (!nan_is_na || !ISNAN(v))) xP[i] = v;
      else if (hasFinal) xP[i] = finalVal;
    }
  } break;

  case STRSXP: {
    const SEXP *xP = STRING_PTR_RO(first);
    SEXP finalVal = NA_STRING;
    bool hasFinal = false;
    int k = 0;

    for (int j = 0; j < nval; ++j) {
      SEXP item = VECTOR_ELT(x, j + off);
      if (length(item) == 1) {
        SEXP v = STRING_ELT(item, 0);
        if (v != NA_STRING && !hasFinal) {
          finalVal = v;
          hasFinal = true;
        }
      } else {
        valP[k++] = STRING_PTR_RO(item);
      }
    }

    for (int i = 0; i < nrow; ++i) {
      if (xP[i] != NA_STRING) continue;

      SEXP v = NA_STRING;
      for (int j = 0; j < k && v == NA_STRING; ++j)
        v = ((SEXP *)valP[j])[i];

      if (v != NA_STRING) SET_STRING_ELT(first, i, v);
      else if (hasFinal)  SET_STRING_ELT(first, i, finalVal);
    }
  } break;

  default:
    error(_("Type '%s' not supported."), type2char(TYPEOF(first)));
  }

  UNPROTECT(nprotect);
  return first;
}