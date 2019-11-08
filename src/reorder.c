#include "data.table.h"

SEXP reorder(SEXP x, SEXP order)
{
  // For internal use only by setkey().
  // 'order' must strictly be a permutation of 1:n (i.e. no repeats, zeros or NAs)
  // If only a small subset in the middle is reordered the ends are moved in: [start,end].
  // x may be a vector, or a list of same-length vectors such as data.table
  R_len_t nrow, ncol;
  size_t maxSize = 0;
  if (isNewList(x)) {
    nrow = length(VECTOR_ELT(x,0));
    ncol = length(x);
    for (int i=0; i<ncol; i++) {
      SEXP v = VECTOR_ELT(x,i);
      if (SIZEOF(v)!=4 && SIZEOF(v)!=8 && SIZEOF(v)!=16)
        error("Item %d of list is type '%s' which isn't yet supported (SIZEOF=%d)", i+1, type2char(TYPEOF(v)), SIZEOF(v));
      if (length(v)!=nrow)
        error("Column %d is length %d which differs from length of column 1 (%d). Invalid data.table.", i+1, length(v), nrow);
      if (SIZEOF(v) > maxSize)
        maxSize=SIZEOF(v);
      if (ALTREP(v)) SET_VECTOR_ELT(x, i, copyAsPlain(v));
    }
    copySharedColumns(x); // otherwise two columns which point to the same vector would be reordered and then re-reordered, issues linked in PR#3768
  } else {
    if (SIZEOF(x)!=4 && SIZEOF(x)!=8 && SIZEOF(x)!=16)
      error("reorder accepts vectors but this non-VECSXP is type '%s' which isn't yet supported (SIZEOF=%d)", type2char(TYPEOF(x)), SIZEOF(x));
    if (ALTREP(x)) error("Internal error in reorder.c: cannot reorder an ALTREP vector. Please see NEWS item 2 in v1.11.4 and report this as a bug."); // # nocov
    maxSize = SIZEOF(x);
    nrow = length(x);
    ncol = 1;
  }
  if (!isInteger(order)) error("order must be an integer vector");
  if (length(order) != nrow) error("nrow(x)[%d]!=length(order)[%d]",nrow,length(order));
  int nprotect = 0;
  if (ALTREP(order)) { order=PROTECT(copyAsPlain(order)); nprotect++; }  // TODO: if it's an ALTREP sequence some optimizations are possible rather than expand

  const int *restrict idx = INTEGER(order);
  int i=0;
  while (i<nrow && idx[i] == i+1) i++;
  const int start = i;
  if (start==nrow) { UNPROTECT(nprotect); return(R_NilValue); }  // input is 1:n, nothing to do
  i = nrow-1;
  while (idx[i] == i+1) i--;
  const int end = i;
  for (int i=start; i<=end; i++) {
    int itmp = idx[i]-1;
    if (itmp<start || itmp>end) error("order is not a permutation of 1:nrow[%d]", nrow);
  }
  // Creorder is for internal use (so we should get the input right!), but the check above seems sensible. The for loop above should run
  // in neglible time (sequential with prefetch). It will catch NAs anywhere but won't catch duplicates. But doing so would be going too
  // far given this is for internal use only and we only use this function internally when we're sure it's a permutation.

  char *TMP = malloc(nrow * maxSize);
  // enough RAM for a copy of one column (of largest type). Writes into the [start,end] subset. Outside [start,end] is wasted in that rarer case
  // to save a "-start" in the deep loop below in all cases.
  if (!TMP) error("Unable to allocate %d * %d bytes of working memory for reordering data.table", end-start+1, maxSize);

  for (int i=0; i<ncol; i++) {
    const SEXP v = isNewList(x) ? VECTOR_ELT(x,i) : x;
    const size_t size = SIZEOF(v);    // size_t, otherwise #5305 (integer overflow in memcpy)
    if (size==4) {
      int *vd = (int *)DATAPTR(v);
      const int *restrict cvd = vd;
      int *restrict tmp = (int *)TMP;
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=start; i<=end; i++) {
        tmp[i] = cvd[idx[i]-1];  // copies 4 bytes; including pointers on 32bit (STRSXP and VECSXP)
      }
      memcpy(vd+start, tmp+start, (end-start+1)*size);
    } else if (size==8) {
      double *vd = (double *)DATAPTR(v);
      const double *restrict cvd = vd;
      double *restrict tmp = (double *)TMP;
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=start; i<=end; i++) {
        tmp[i] = cvd[idx[i]-1];  // copies 8 bytes; including pointers on 64bit (STRSXP and VECSXP)
      }
      memcpy(vd+start, tmp+start, (end-start+1)*size);
    } else {
      // #1444 -- support for copying CPLXSXP (which have size 16)
      Rcomplex *vd = (Rcomplex *)DATAPTR(v);
      const Rcomplex *restrict cvd = vd;
      Rcomplex *restrict tmp = (Rcomplex *)TMP;
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=start; i<=end; i++) {
        tmp[i] = cvd[idx[i]-1];  // copies 16 bytes
      }
      memcpy(vd+start, tmp+start, (end-start+1)*size);
    }
  }
  // It's ok to ignore write barrier, and in parallel too, only because this reorder() function accepts and checks
  // a unique permutation of 1:nrow. It performs an in-place shuffle. This operation in the end does not change gcgen, mark or
  // named/refcnt. They all stay the same even for STRSXP and VECSXP because it's just a data shuffle.
  //
  // Theory:
  // The write to TMP is contiguous, so sync between cpus of written-cache-lines should not be an issue.
  // The read from vd is random, but at least the column has a good chance of being all in cache as parallelism is within column.
  // As idx approaches being ordered (e.g. moving blocks around) then it should automatically be more read cache efficient.
  free(TMP);
  UNPROTECT(nprotect);
  return(R_NilValue);
}

