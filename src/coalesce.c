#include "data.table.h"

SEXP coalesce(SEXP x, SEXP inplaceArg) {
  if (TYPEOF(x)!=VECSXP)
    error(_("Internal error in coalesce.c: input is list(...) at R level")); // # nocov
  if (!IS_TRUE_OR_FALSE(inplaceArg))
    error(_("Internal error in coalesce.c: argument 'inplaceArg' must be TRUE or FALSE")); // # nocov
  const bool inplace = LOGICAL(inplaceArg)[0];
  const bool verbose = GetVerbose();
  int nprotect = 0;
  if (length(x)==0 || isNull(VECTOR_ELT(x,0))) return R_NilValue;  // coalesce(NULL, "foo") return NULL even though character type mismatches type NULL
  SEXP first;  // the first vector (it might be the first argument, or it might be the first column of a data.table|frame
  int off = 1; // when x has been pointed to the list of replacement candidates, is the first candidate in position 0 or 1 in the list
  if (TYPEOF(VECTOR_ELT(x,0)) == VECSXP) {
    if (length(x)!=1)
      error(_("The first argument is a list, data.table or data.frame. In this case there should be no other arguments provided."));
    x = VECTOR_ELT(x,0);
    if (length(x)==0) return R_NilValue;
    first = VECTOR_ELT(x,0);
  } else {
    first = VECTOR_ELT(x,0);
    if (length(x)>1 && TYPEOF(VECTOR_ELT(x,1))==VECSXP) { x=VECTOR_ELT(x,1); off=0; }  // coalesce(x, list(y,z))
  }
  const int nval = length(x)-off;
  if (nval==0) return first;
  const bool factor = isFactor(first);
  const int nrow = length(first);
  for (int i=0; i<nval; ++i) {
    SEXP item = VECTOR_ELT(x, i+off);
    if (factor) {
      if (!isFactor(item))
        error(_("Item 1 is a factor but item %d is not a factor. When factors are involved, all items must be factor."), i+2);
      if (!R_compute_identical(PROTECT(getAttrib(first, R_LevelsSymbol)), PROTECT(getAttrib(item, R_LevelsSymbol)), 0))
        error(_("Item %d is a factor but its levels are not identical to the first item's levels."), i+2);
      UNPROTECT(2);
    } else {
      if (isFactor(item))
        error(_("Item %d is a factor but item 1 is not a factor. When factors are involved, all items must be factor."), i+2);
    }
    if (TYPEOF(first) != TYPEOF(item))
      error(_("Item %d is type %s but the first item is type %s. Please coerce before coalescing."), i+2, type2char(TYPEOF(item)), type2char(TYPEOF(first)));
    if (!R_compute_identical(PROTECT(getAttrib(first, R_ClassSymbol)), PROTECT(getAttrib(item, R_ClassSymbol)), 0))
      error(_("Item %d has a different class than item 1."), i+2);
    UNPROTECT(2);
    if (length(item)!=1 && length(item)!=nrow)
      error(_("Item %d is length %d but the first item is length %d. Only singletons are recycled."), i+2, length(item), nrow);
  }
  if (!inplace) {
    first = PROTECT(copyAsPlain(first)); nprotect++;
    if (verbose) Rprintf(_("coalesce copied first item (inplace=FALSE)\n"));
  }
  void **valP = (void **)R_alloc(nval, sizeof(void *));
  switch(TYPEOF(first)) {
  case LGLSXP:
  case INTSXP: {
    int *xP = INTEGER(first), k=0, finalVal=NA_INTEGER;
    for (int j=0; j<nval; ++j) {
      SEXP item = VECTOR_ELT(x, j+off);
      if (length(item)==1) {
        int tt = INTEGER(item)[0];
        if (tt==NA_INTEGER) continue;  // singleton NA can be skipped
        finalVal = tt;
        break;  // stop early on the first singleton that is not NA; minimizes deepest loop body below
      }
      valP[k++] = INTEGER(item);
    }
    const bool final=(finalVal!=NA_INTEGER);
    #pragma omp parallel for num_threads(getDTthreads(nrow, true))
    for (int i=0; i<nrow; ++i) {
      int val = xP[i];
      if (val!=NA_INTEGER) continue;
      int j=0; while (val==NA_INTEGER && j<k) val=((int *)valP[j++])[i];  // k is the number of non-singletons
      if (val!=NA_INTEGER) xP[i]=val; else if (final) xP[i]=finalVal;
    }
  } break;
  case REALSXP: {
    if (Rinherits(first, char_integer64)) { // Rinherits() is true for nanotime
      int64_t *xP=(int64_t *)REAL(first), finalVal=NA_INTEGER64;
      int k=0;
      for (int j=0; j<nval; ++j) {
        SEXP item = VECTOR_ELT(x, j+off);
        if (length(item)==1) {
          int64_t tt = ((int64_t *)REAL(item))[0];
          if (tt==NA_INTEGER64) continue;
          finalVal = tt;
          break;
        }
        valP[k++] = REAL(item);
      }
      const bool final = (finalVal!=NA_INTEGER64);
      #pragma omp parallel for num_threads(getDTthreads(nrow, true))
      for (int i=0; i<nrow; ++i) {
        int64_t val=xP[i];
        if (val!=NA_INTEGER64) continue;
        int j=0; while (val==NA_INTEGER64 && j<k) val=((int64_t *)valP[j++])[i];
        if (val!=NA_INTEGER64) xP[i]=val; else if (final) xP[i]=finalVal;
      }
    } else {
      double *xP = (double *)REAL(first), finalVal=NA_REAL;
      int k=0;
      for (int j=0; j<nval; ++j) {
        SEXP item = VECTOR_ELT(x, j+off);
        if (length(item)==1) {
          double tt = REAL(item)[0];
          if (ISNAN(tt)) continue;
          finalVal = tt;
          break;
        }
        valP[k++] = REAL(item);
      }
      const bool final = !ISNAN(finalVal);
      #pragma omp parallel for num_threads(getDTthreads(nrow, true))
      for (int i=0; i<nrow; ++i) {
        double val=xP[i];
        if (!ISNAN(val)) continue;
        int j=0; while (ISNAN(val) && j<k) val=((double *)valP[j++])[i];
        if (!ISNAN(val)) xP[i]=val; else if (final) xP[i]=finalVal;
      }
    }
  } break;
  case CPLXSXP: {
    Rcomplex *xP = COMPLEX(first), finalVal=NA_CPLX;
    int k=0;
    for (int j=0; j<nval; ++j) {
      SEXP item = VECTOR_ELT(x, j+off);
      if (length(item)==1) {
        Rcomplex tt = COMPLEX(item)[0];
        if (ISNAN(tt.r) && ISNAN(tt.i)) continue;
        finalVal = tt;
        break;
      }
      valP[k++] = COMPLEX(item);
    }
    const bool final = !ISNAN(finalVal.r) && !ISNAN(finalVal.i);
    #pragma omp parallel for num_threads(getDTthreads(nrow, true))
    for (int i=0; i<nrow; ++i) {
      Rcomplex val=xP[i];
      if (!ISNAN(val.r) && !ISNAN(val.i)) continue;
      int j=0; while (ISNAN(val.r) && ISNAN(val.i) && j<k) val=((Rcomplex *)valP[j++])[i];
      if (!ISNAN(val.r) || !ISNAN(val.i)) xP[i]=val; else if (final) xP[i]=finalVal;
    }
  } break;
  case STRSXP: {
    const SEXP *xP = STRING_PTR(first);
    SEXP finalVal=NA_STRING;
    int k=0;
    for (int j=0; j<nval; ++j) {
      SEXP item = VECTOR_ELT(x, j+off);
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
      if (val!=NA_STRING) SET_STRING_ELT(first, i, val); else if (final) SET_STRING_ELT(first, i, finalVal);
    }
  } break;
  default:
    error(_("Unsupported type: %s"), type2char(TYPEOF(first))); // e.g. raw is tested
  }
  UNPROTECT(nprotect);
  return first;
}

