#include "data.table.h"

SEXP vecseq(SEXP x, SEXP len, SEXP clamp) {
  // Name taken from bit::vecseq, but,
  //   * implemented in C rather than R
  //   * takes len rather than y endpoints
  //   * no concat=FALSE or eval arguments as not needed.
  //   * Adds clamp to prevent very large vectors being created (unless allow.cartesian=TRUE)
  // Specially for use by [.data.table after binary search. Now so specialized that for general use
  // bit::vecseq is recommended (Jens has coded it in C now).

  if (!isInteger(x))
    error(_("x must be an integer vector")); // # nocov
  if (!isInteger(len))
    error(_("len must be an integer vector")); // # nocov
  if (LENGTH(x) != LENGTH(len))
    error(_("x and len must be the same length")); // # nocov
  const int *ix = INTEGER(x);
  const int *ilen = INTEGER(len), nlen=LENGTH(len);
  int reslen = 0;
  for (int i=0; i<nlen; ++i) {
    if (INT_MAX-reslen < ilen[i])
      error(_("Join results in more than 2^31 rows (internal vecseq reached physical limit). Very likely misspecified join. Check for duplicate key values in i each of which join to the same group in x over and over again. If that's ok, try by=.EACHI to run j for each group to avoid the large allocation. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and data.table issue tracker for advice."));
    reslen += ilen[i];
  }
  if (!isNull(clamp)) {
    if (!isNumeric(clamp) || LENGTH(clamp)!=1)
      error(_("clamp must be a double vector length 1")); // # nocov
    double limit = REAL(clamp)[0];
    if (limit<0)
      error(_("clamp must be positive")); // # nocov
    if (reslen>limit)
      error(_("Join results in %d rows; more than %d = nrow(x)+nrow(i). Check for duplicate key values in i each of which join to the same group in x over and over again. If that's ok, try by=.EACHI to run j for each group to avoid the large allocation. If you are sure you wish to proceed, rerun with allow.cartesian=TRUE. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and data.table issue tracker for advice."), reslen, (int)limit);
  }
  SEXP ans = PROTECT(allocVector(INTSXP, reslen));
  int *ians = INTEGER(ans);
  int k = 0;
  for (int i=0; i<nlen; ++i) {
    int thisx = ix[i];
    for (int j=0; j<ilen[i]; ++j) {
      ians[k++] = thisx++;
    }
  }
  UNPROTECT(1);
  return(ans);
}

SEXP seqexp(SEXP x) {
  // function(x) unlist(lapply(seq_along(x), function(i) rep(i, x[[i]])))
  // used to expand bmerge()$lens, when $starts does not have NAs (or duplicates?)
  // replaces rep.int(indices__, len__) where indices__ was seq_along(x)
  // input:  1,1,2,  1,3,    1,0,1,2
  // output: 1,2,3,3,4,5,5,5,6,  8,9,9
  // all1 returns NULL, this is now commented out to reduce overhead becase seqexp is called only when !(ans$allLen1 && (!inner || len.x==length(ans$starts))) if seqexp would be used in another place we could enable that
  if (!isInteger(x))
    error("internal error: 'x' must be an integer"); // # nocov
  const int *xp = INTEGER(x), nx = LENGTH(x);
  int nans = 0;
  for (int i=0; i<nx; ++i)
    nans += xp[i];
  /*bool hasZero=false;
  for (int i=0; i<nx; ++i) {
    int thisx = xp[i];
    if (!thisx)
      hasZero = true;
    nans += thisx;
  }
  if (!hasZero && nans==nx) // short-circuit: if !hasZeros && nans=length(x), then all must be 1
    return R_NilValue;*/
  SEXP ans = PROTECT(allocVector(INTSXP, nans));
  int *ansp = INTEGER(ans);
  for (int i=0, ians=0; i<nx; ++i) {
    int thisx = xp[i];
    int thisi = i+1; // R's 1-based index
    for (int j=0; j<thisx; ++j)
      ansp[ians++] = thisi;
  }
  UNPROTECT(1);
  return ans;
}
