#include "data.table.h"

SEXP coalesce(SEXP x, SEXP values, SEXP inplaceArg) {

  if (!isVectorAtomic(x)) error("argument 'x' must be an atomic vector");
  if (!IS_TRUE_OR_FALSE(inplaceArg)) error("argument '.inplace' must be TRUE or FALSE");
  const bool inplace = LOGICAL(inplaceArg)[0];
  const bool verbose = GetVerbose();
  const int nval = length(values);

  const bool factor = isFactor(x);
  const int nrow = length(x);

  for (int i=0; i<nval; ++i) {
    SEXP item = VECTOR_ELT(values, i);
    if (factor) {
      if (!isFactor(item))
        error("Item 1 is a factor but item %d is not a factor. When factors are involved, all items must be factor.", i+2);
      if (!R_compute_identical(getAttrib(x, R_LevelsSymbol), getAttrib(item, R_LevelsSymbol), 0))
        error("Item %d is a factor but its levels are not identical to the first item's levels", i+2);
    } else {
      if (isFactor(VECTOR_ELT(values, i)))
        error("Item %d is a factor but item 1 is not a factor. When factors are involved, all items must be factor.", i+2);
      SEXP thisclass = getAttrib(item, R_ClassSymbol);
      if (length(thisclass) && !R_compute_identical(getAttrib(x, R_ClassSymbol), thisclass, 0))
        error("Item %d has a different class than item 1", i+2);
    }
    if (TYPEOF(x) != TYPEOF(item))
       error("Item %d has internal type %s but the first item has type %s. Please coerce before coalescing", i+2, type2char(TYPEOF(item)), type2char(TYPEOF(x)));
    if (/*length(item)!=1 &&*/ length(item)!=nrow)
      error("Item %d is length %d but the first item is length %d. Only singletons are recycled.", i+2, length(item));
  }
  if (!inplace) x = PROTECT(duplicate(x));
  if (verbose) Rprintf("coalesce copied x (inplace=FALSE)\n");
  void **valP = (void **)R_alloc(nval, sizeof(void *));
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP: {
    int *xP = INTEGER(x);
    for (int j=0; j<nval; ++j) valP[j] = INTEGER(VECTOR_ELT(values, j));
    #pragma omp parallel for num_threads(getDTthreads())
    for (int i=0; i<nrow; ++i) {
      int val=xP[i], j=0;
      if (val!=NA_INTEGER) continue;
      while (val==NA_INTEGER && j<nval) val=((int *)valP[j++])[i];
      xP[i] = val;
    }
  } break;
  case REALSXP: {
    for (int j=0; j<nval; ++j) valP[j] = REAL(VECTOR_ELT(values, j));
    if (INHERITS(x,char_integer64) || INHERITS(x,char_nanotime)) { // integer64 and nanotime (it seem nanotime does not inherit from integer64)
      int64_t *xP = (int64_t *)REAL(x);
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<nrow; ++i) {
        int64_t val=xP[i];
        if (val!=NA_INTEGER64) continue;
        int j=0; while (val==NA_INTEGER64 && j<nval) val=((int64_t *)valP[j++])[i];
        xP[i] = val;
      }
    } else {
      double *xP = (double *)REAL(x);
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<nrow; ++i) {
        double val=xP[i];
        if (!ISNA(val)) continue;
        int j=0; while (ISNA(val) && j<nval) val=((double *)valP[j++])[i];
        xP[i] = val;
      }
    }
  } break;
  case STRSXP: {
    const SEXP *xP = STRING_PTR(x);
    for (int j=0; j<nval; ++j) valP[j] = STRING_PTR(VECTOR_ELT(values, j));
    for (int i=0; i<nrow; ++i) {
      SEXP val=xP[i];
      if (val!=NA_STRING) continue;
      int j=0; while (val==NA_STRING && j<nval) val=((SEXP *)valP[j++])[i];
      if (val!=NA_STRING) SET_STRING_ELT(x, i, val);
    }
  } break;
  default:
    error("Unsupported type: %s", type2char(TYPEOF(x))); // e.g. raw is tested
  }
  if (!inplace) UNPROTECT(1);
  return x;
}

