#include "data.table.h"

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
      if (SIZEOF(v)!=4 && SIZEOF(v)!=8 && SIZEOF(v)!=16)
        error(_("Item %d of list is type '%s' which isn't yet supported (SIZEOF=%d)"), i+1, type2char(TYPEOF(v)), SIZEOF(v));
      if (length(v)!=nrow)
        error(_("Column %d is length %d which differs from length of column 1 (%d). Invalid data.table."), i+1, length(v), nrow);
      if (SIZEOF(v) > maxSize)
        maxSize=SIZEOF(v);
      if (ALTREP(v)) SET_VECTOR_ELT(x, i, copyAsPlain(v));
    }
    copySharedColumns(x); // otherwise two columns which point to the same vector would be reordered and then re-reordered, issues linked in PR#3768
  } else {
    if (SIZEOF(x)!=4 && SIZEOF(x)!=8 && SIZEOF(x)!=16)
      error(_("reorder accepts vectors but this non-VECSXP is type '%s' which isn't yet supported (SIZEOF=%d)"), type2char(TYPEOF(x)), SIZEOF(x));
    if (ALTREP(x)) error(_("Internal error in reorder.c: cannot reorder an ALTREP vector. Please see NEWS item 2 in v1.11.4 and report this as a bug.")); // # nocov
    maxSize = SIZEOF(x);
    nrow = length(x);
    ncol = 1;
  }
  if (!isInteger(order)) error(_("order must be an integer vector"));
  if (length(order) != nrow) error(_("nrow(x)[%d]!=length(order)[%d]"),nrow,length(order));
  int nprotect = 0;
  if (ALTREP(order)) { order=PROTECT(copyAsPlain(order)); nprotect++; }  // TODO: if it's an ALTREP sequence some optimizations are possible rather than expand

  const int *restrict idx = INTEGER(order);
  int i=0;
  while (i<nrow && idx[i] == i+1) ++i;
  const int start = i;
  if (start==nrow) { UNPROTECT(nprotect); return(R_NilValue); }  // input is 1:n, nothing to do
  i = nrow-1;
  while (idx[i] == i+1) --i;
  const int end = i;
  for (int i=start; i<=end; ++i) {
    int itmp = idx[i]-1;
    if (itmp<start || itmp>end) error(_("order is not a permutation of 1:nrow[%d]"), nrow);
    // This file is for internal use (so we should get the input right), but this check seems sensible to still perform since it should
    // run in negligible time (sequential with prefetch). It is not sufficient though; e.g. a non-permutation such as a duplicate will not be caught.
    // But checking for dups would incur too much cost given that i) this is for internal use only and we only use this function internally
    // when we're sure we're passing in a strict permutation, and ii) this operation is fundamental to setkey and data.table.
  }

  char *TMP = malloc((end-start+1)*maxSize);
  if (!TMP) error(_("Unable to allocate %d * %d bytes of working memory for reordering data.table"), end-start+1, maxSize);

  for (int i=0; i<ncol; ++i) {
    const SEXP v = isNewList(x) ? VECTOR_ELT(x,i) : x;
    const size_t size = SIZEOF(v);    // size_t, otherwise #5305 (integer overflow in memcpy)
    if (size==4) {
      const int *restrict vd = DATAPTR_RO(v);
      int *restrict tmp = (int *)TMP;
      #pragma omp parallel for num_threads(getDTthreads())
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
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=start; i<=end; ++i) {
        tmp[i-start] = vd[idx[i]-1];  // copies 8 bytes; e.g. REALSXP and also SEXP pointers on 64bit (STRSXP and VECSXP)
      }
    } else { // size 16; checked up front
      const Rcomplex *restrict vd = DATAPTR_RO(v);
      Rcomplex *restrict tmp = (Rcomplex *)TMP;
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=start; i<=end; ++i) {
        tmp[i-start] = vd[idx[i]-1];
      }
    }

    // Unique and somber line. Not done lightly. Please read all comments in this file.
    memcpy(((char *)DATAPTR_RO(v)) + size*start, TMP, size*(end-start+1));
    // The one and only place in data.table where we write behind the write-barrier. Fundamental to setkey and data.table.
    // This file is unique and special w.r.t. the write-barrier: an utterly strict in-place shuffle.
    // This shuffle operation does not inc or dec named/refcnt, or anything similar in R: past, present or future.
  }

  free(TMP);
  UNPROTECT(nprotect);
  return(R_NilValue);
}

