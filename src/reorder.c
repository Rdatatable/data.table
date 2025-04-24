#include "data.table.h"

/*
  OpenMP is used here to reorder a vector or each column in a list of vectors
    (such as in a data.table) based on a given vector that dictates the new
    ordering of elements
*/
SEXP reorder(SEXP x, SEXP order)
{
  // For internal use only by setkey().
  // 'order' must be a strict permutation of 1:n; i.e. no repeats, zeros, NAs. Also known as a shuffle.
  // If only a small subset in the middle is reordered, the ends are moved in to avoid wasteful work.
  // x may be a vector, or a list of same-length vectors (typically a data.table).
  R_len_t nrow, ncol;
  size_t maxSize = 0;
  if (isNewList(x)) {
    nrow = length(VECTOR_ELT(x,0));
    ncol = length(x);
    for (int i=0; i<ncol; i++) {
      SEXP v = VECTOR_ELT(x,i);
      if (SIZEOF(v)!=4 && SIZEOF(v)!=8 && SIZEOF(v)!=16 && SIZEOF(v)!=1)
        error(_("Item %d of list is type '%s' which isn't yet supported (SIZEOF=%zu)"), i+1, type2char(TYPEOF(v)), SIZEOF(v));
      if (length(v)!=nrow)
        error(_("Column %d is length %d which differs from length of column 1 (%d). Invalid data.table."), i+1, length(v), nrow);
      if (SIZEOF(v) > maxSize)
        maxSize=SIZEOF(v);
      if (ALTREP(v)) SET_VECTOR_ELT(x, i, copyAsPlain(v));
    }
    copySharedColumns(x); // otherwise two columns which point to the same vector would be reordered and then re-reordered, issues linked in PR#3768
  } else {
    if (SIZEOF(x)!=4 && SIZEOF(x)!=8 && SIZEOF(x)!=16 && SIZEOF(x)!=1)
      error(_("reorder accepts vectors but this non-VECSXP is type '%s' which isn't yet supported (SIZEOF=%zu)"), type2char(TYPEOF(x)), SIZEOF(x));
    if (ALTREP(x)) internal_error(__func__, "cannot reorder an ALTREP vector. Please see NEWS item 2 in v1.11.4"); // # nocov
    maxSize = SIZEOF(x);
    nrow = length(x);
    ncol = 1;
  }
  if (!isInteger(order))
    error(_("order must be an integer vector"));
  if (length(order) != nrow)
    error("nrow(x)[%d]!=length(order)[%d]", nrow, length(order)); // # notranslate
  int nprotect = 0;
  if (ALTREP(order)) { order=PROTECT(copyAsPlain(order)); nprotect++; }  // TODO: if it's an ALTREP sequence some optimizations are possible rather than expand

  const int *restrict idx = INTEGER(order);
  int i=0;
  while (i<nrow && idx[i] == i+1) ++i;
  const int start=i;
  if (start==nrow) { UNPROTECT(nprotect); return R_NilValue; }  // input is 1:n, nothing to do
  i = nrow-1;
  while (idx[i] == i+1) --i;
  const int end=i, nmid=end-start+1;

  uint8_t *seen = (uint8_t *)R_alloc(nmid, sizeof(uint8_t)); // detect duplicates
  memset(seen, 0, nmid*sizeof(uint8_t));
  for (int i=start; i<=end; ++i) {
    if (idx[i]==NA_INTEGER || idx[i]-1<start || idx[i]-1>end || seen[idx[i]-1-start]++)
      error(_("Item %d of order (%d) is either NA, out of range [1,%d], or is duplicated. The new order must be a strict permutation of 1:n"),
              i+1, idx[i], length(order));
    // This should run in reasonable time because although 'seen' is random write, it is writing to just 1 byte * nrow
    // which is relatively small and has a good chance of fitting in cache.
    // A worry mitigated by this check is a user passing their own incorrect ordering using ::: to reach this internal.
    // This check is once up front, and then idx is applied to all the columns which is where the most time is spent.
  }

  char *TMP = R_alloc(nmid, maxSize);

  for (int i=0; i<ncol; ++i) {
    const SEXP v = isNewList(x) ? VECTOR_ELT(x,i) : x;
    const size_t size = SIZEOF(v);    // size_t, otherwise #61 (integer overflow in memcpy)
    if (size==4) {
      const int *restrict vd = DATAPTR_RO(v);
      int *restrict tmp = (int *)TMP;
      #pragma omp parallel for num_threads(getDTthreads(end, true))
      for (int i=start; i<=end; ++i) {
        tmp[i-start] = vd[idx[i]-1];  // copies 4 bytes; e.g. INTSXP and also SEXP pointers on 32bit (STRSXP and VECSXP)
      }
      // Theory:
      // The write to TMP is contiguous, so sync between cpus of written-cache-lines should not be an issue.
      // The read from vd is random, but at least the column has a good chance of being all in cache as parallelism is within column.
      // As idx approaches being ordered (e.g. moving blocks around) then this should approach read cache-efficiency too.
    } else if (size==8) {
      const double *restrict vd = DATAPTR_RO(v);
      double *restrict tmp = (double *)TMP;
      #pragma omp parallel for num_threads(getDTthreads(end, true))
      for (int i=start; i<=end; ++i) {
        tmp[i-start] = vd[idx[i]-1];  // copies 8 bytes; e.g. REALSXP and also SEXP pointers on 64bit (STRSXP and VECSXP)
      }
    } else if (size==16) {
      const Rcomplex *restrict vd = DATAPTR_RO(v);
      Rcomplex *restrict tmp = (Rcomplex *)TMP;
      #pragma omp parallel for num_threads(getDTthreads(end, true))
      for (int i=start; i<=end; ++i) {
        tmp[i-start] = vd[idx[i]-1];
      }
    } else { // size 1; checked up front // support raw as column #5100
      const Rbyte *restrict vd = DATAPTR_RO(v);
      Rbyte *restrict tmp = (Rbyte *)TMP;
      #pragma omp parallel for num_threads(getDTthreads(end, true))
      for (int i=start; i<=end; ++i) {
        tmp[i-start] = vd[idx[i]-1];  // copies 1 bytes; e.g. RAWSXP
      }
    }
    // Unique and somber line. Not done lightly. Please read all comments in this file.
    memcpy(((char *)DATAPTR_RO(v)) + size*start, TMP, size*nmid);
    // The one and only place in data.table where we write behind the write-barrier. Fundamental to setkey and data.table.
    // This file is unique and special w.r.t. the write-barrier: an utterly strict in-place shuffle.
    // This shuffle operation does not inc or dec named/refcnt, or anything similar in R: past, present or future.
  }
  UNPROTECT(nprotect);
  return R_NilValue;
}

SEXP setcolorder(SEXP x, SEXP o)
{
  SEXP names = getAttrib(x, R_NamesSymbol);
  const int ncol=LENGTH(x);
  if (isNull(names)) error(_("dt passed to setcolorder has no names"));
  if (ncol != LENGTH(names))
    internal_error(__func__, "dt passed to setcolorder has %d columns but %d names", ncol, LENGTH(names));  // # nocov
  SEXP tt = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(tt, 0, names);
  SET_VECTOR_ELT(tt, 1, x);
  reorder(tt, o);
  UNPROTECT(1);
  return R_NilValue;
}

