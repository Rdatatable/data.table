#include "data.table.h"

SEXP coalesce(SEXP x, SEXP values, SEXP inplaceArg) {

  if (!isVectorAtomic(x)) error("argument 'x' must be an atomic vector");
  if (!IS_TRUE_OR_FALSE(inplaceArg)) error("argument '.inplace' must be TRUE or FALSE");
  const bool inplace = LOGICAL(inplaceArg)[0];
  const bool verbose = GetVerbose();
  const int nval = length(values);
  const bool factor=isFactor(x);
  const int nrow = length(x);
  for (int i=0; i<nval; ++i) {
    SEXP item = VECTOR_ELT(values, i);
    if (factor) {
      if (!isFactor(item))
        error("Item 1 is a factor but item %d is not a factor. When factors are involved, all items must be factor.", i+2);
      if (!R_compute_identical(getAttrib(x, R_LevelsSymbol), getAttrib(item, R_LevelsSymbol), 0))
        error("Item %d is a factor but its levels are not identical to the first item's levels.", i+2);
    } else {
      if (isFactor(VECTOR_ELT(values, i)))
        error("Item %d is a factor but item 1 is not a factor. When factors are involved, all items must be factor.", i+2);
    }
    if (TYPEOF(x) != TYPEOF(item))
      error("Item %d is type %s but the first item is type %s. Please coerce before coalescing.", i+2, type2char(TYPEOF(item)), type2char(TYPEOF(x)));
    if (!R_compute_identical(getAttrib(x, R_ClassSymbol), getAttrib(item, R_ClassSymbol), 0))
      error("Item %d has a different class than item 1.", i+2);
    if (length(item)!=1 && length(item)!=nrow)
      error("Item %d is length %d but the first item is length %d. Only singletons are recycled.", i+2, length(item), nrow);
  }
  if (!inplace) x = PROTECT(duplicate(x));
  if (verbose) Rprintf("coalesce copied x (inplace=FALSE)\n");
  void **valP = (void **)R_alloc(nval, sizeof(void *));
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP: {
    int *xP = INTEGER(x), k=0, finalVal=NA_INTEGER;
    for (int j=0; j<nval; ++j) {
      SEXP item = VECTOR_ELT(values, j);
      if (length(item)==1) {
        int tt = INTEGER(item)[0];
        if (tt==NA_INTEGER) continue;  // singleton NA can be skipped
        finalVal = tt;
        break;  // stop early on the first singleton that is not NA; minimizes deepest loop body below
      }
      valP[k++] = INTEGER(item);
    }
    const bool final=(finalVal!=NA_INTEGER);
    #pragma omp parallel for num_threads(getDTthreads())
    for (int i=0; i<nrow; ++i) {
      int val = xP[i];
      if (val!=NA_INTEGER) continue;
      int j=0; while (val==NA_INTEGER && j<k) val=((int *)valP[j++])[i];  // k is the number of non-singletons
      if (val!=NA_INTEGER) xP[i]=val; else if (final) xP[i]=finalVal;
    }
  } break;
  case REALSXP: {
    if (INHERITS(x,char_integer64) || INHERITS(x,char_nanotime)) { // integer64 and nanotime (it seem nanotime does not inherit from integer64)
      int64_t *xP=(int64_t *)REAL(x), finalVal=NA_INTEGER64;
      int k=0;
      for (int j=0; j<nval; ++j) {
        SEXP item = VECTOR_ELT(values, j);
        if (length(item)==1) {
          int64_t tt = ((int64_t *)REAL(item))[0];
          if (tt==NA_INTEGER64) continue;
          finalVal = tt;
          break;
        }
        valP[k++] = REAL(item);
      }
      const bool final = (finalVal!=NA_INTEGER64);
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<nrow; ++i) {
        int64_t val=xP[i];
        if (val!=NA_INTEGER64) continue;
        int j=0; while (val==NA_INTEGER64 && j<k) val=((int64_t *)valP[j++])[i];
        if (val!=NA_INTEGER64) xP[i]=val; else if (final) xP[i]=finalVal;
      }
    } else {
      double *xP = (double *)REAL(x), finalVal=NA_REAL;
      int k=0;
      for (int j=0; j<nval; ++j) {
        SEXP item = VECTOR_ELT(values, j);
        if (length(item)==1) {
          double tt = REAL(item)[0];
          if (ISNA(tt)) continue;
          finalVal = tt;
          break;
        }
        valP[k++] = REAL(item);
      }
      const bool final = !ISNA(finalVal);
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<nrow; ++i) {
        double val=xP[i];
        if (!ISNA(val)) continue;
        int j=0; while (ISNA(val) && j<k) val=((double *)valP[j++])[i];
        if (!ISNA(val)) xP[i]=val; else if (final) xP[i]=finalVal;
      }
    }
  } break;
  case STRSXP: {
    const SEXP *xP = STRING_PTR(x);
    SEXP finalVal=NA_STRING;
    int k=0;
    for (int j=0; j<nval; ++j) {
      SEXP item = VECTOR_ELT(values, j);
      if (length(item)==1) {
        SEXP tt = STRING_ELT(item,0);
        if (tt==NA_STRING) continue;
        finalVal = tt;
        break;
      }
      valP[k++] = STRING_PTR(item);
    }
    const bool final = (finalVal!=NA_STRING);
    for (int i=0; i<nrow; ++i) {
      SEXP val = xP[i];
      if (val!=NA_STRING) continue;
      int j=0; while (val==NA_STRING && j<k) val=((SEXP *)valP[j++])[i];
      if (val!=NA_STRING) SET_STRING_ELT(x, i, val); else if (final) SET_STRING_ELT(x, i, finalVal);
    }
  } break;
  default:
    error("Unsupported type: %s", type2char(TYPEOF(x))); // e.g. raw is tested
  }
  if (!inplace) UNPROTECT(1);
  return x;
}

