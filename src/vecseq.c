#include "data.table.h"

SEXP vecseq(SEXP x, SEXP len, SEXP clamp)
{
  // Name taken from bit::vecseq, but,
  //   * implemented in C rather than R
  //   * takes len rather than y endpoints
  //   * no concat=FALSE or eval arguments as not needed.
  //   * Adds clamp to prevent very large vectors being created (unless allow.cartesian=TRUE)
  // Specially for use by [.data.table after binary search. Now so specialized that for general use
  // bit::vecseq is recommended (Jens has coded it in C now).

  if (!isInteger(x)) error(_("x must be an integer vector"));
  if (!isInteger(len)) error(_("len must be an integer vector"));
  if (LENGTH(x) != LENGTH(len)) error(_("x and len must be the same length"));
  const int *ix = INTEGER(x);
  const int *ilen = INTEGER(len), nlen=LENGTH(len);
  int reslen = 0;
  for (int i=0; i<nlen; ++i) {
    if (INT_MAX-reslen < ilen[i])
      error(_("Join results in more than 2^31 rows (internal vecseq reached physical limit). Very likely misspecified join. Check for duplicate key values in i each of which join to the same group in x over and over again. If that's ok, try by=.EACHI to run j for each group to avoid the large allocation. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and data.table issue tracker for advice."));
    reslen += ilen[i];
  }
  if (!isNull(clamp)) {
    if (!isNumeric(clamp) || LENGTH(clamp)!=1) error(_("clamp must be a double vector length 1"));
    double limit = REAL(clamp)[0];
    if (limit<0) error(_("clamp must be positive"));
    if (reslen>limit) error(_("Join results in %d rows; more than %d = nrow(x)+nrow(i). Check for duplicate key values in i each of which join to the same group in x over and over again. If that's ok, try by=.EACHI to run j for each group to avoid the large allocation. If you are sure you wish to proceed, rerun with allow.cartesian=TRUE. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and data.table issue tracker for advice."), reslen, (int)limit);
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

