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

SEXP vecseq_having(SEXP x, SEXP len, SEXP having, SEXP retGrpArg, SEXP o__) {

  if (!isInteger(x)) error(_("x must be an integer vector"));
  if (!isInteger(len)) error(_("len must be an integer vector"));
  if (!isLogical(having)) error(_("having must be a logical vector"));
  if (!isInteger(o__)) error(_("o__ must be an integer vector"));
  //if (!isInteger(irows)) error(_("irows must be an integer vector"));
  //if (!isLogical(isGforce)) error(_("isGforce must be a logical vector"));
  if (!isLogical(retGrpArg)) error(_("retGrpArg must be a logical vector"));
  
  if (LENGTH(x) != LENGTH(len)) error(_("x and len must be the same length"));
  if (LENGTH(x) != LENGTH(having)) error(_("x and having must be the same length"));
  if (LENGTH(retGrpArg) > 1) error(_("retGrpArg must be a logical vector of length 1"));
  
  const int *ihaving = INTEGER(having); 
  const int *ix = INTEGER(x);
  const int *ilen = INTEGER(len), nlen=LENGTH(len);
  
  int reslen = 0;
  int total_true = 0;
  
  for (int i = 0; i < nlen; ++i) {
    if (ihaving[i]) {
      reslen += ilen[i];
      total_true++;
    }
  }
  
  if (reslen == 0) return(allocVector(INTSXP, 0));
  
  bool retGrp = LOGICAL(retGrpArg)[0] == TRUE;
  if (total_true == nlen && retGrp) return(R_NilValue); //no need to recalculate if all groups are returned from having
  
  SEXP ans = PROTECT(allocVector(INTSXP, reslen));
  int *ians = INTEGER(ans);

  if (retGrp) {
    // should return a list to minimize allocations
    // We have the following allocations
    //   - f__      DONE
    //   - len__    DONE
    //   - max_grp  DONE
    
    // TODO
    //   - vec_seq_result_o = order(ans)
    //   - irows = if (is.null(irows)) vec_seq_result_o else sort(irows[ans])
    //   - byval = lapply(byval, `[`, ans[vec_seq_result_o])
    //   - o__ = order(vec_seq_result_o)
    
    SEXP tt, vv;
    
    setAttrib(ans, sym_starts, tt = allocVector(INTSXP, total_true));
    setAttrib(ans, sym_grplen, vv = allocVector(INTSXP, total_true));

    int *ss = INTEGER(tt);
    int *ww = INTEGER(vv);
        
    int k = 0;
    int l = 0;
    int total = 1;
    int maxgrpn = 0;
    if (LENGTH(o__)) {
      const int *io__ = INTEGER(o__); 
      for (int i=0; i<nlen; ++i) {
        if (!ihaving[i]) continue;
        int thisx = ix[i] - 1;
        int elem = ilen[i];
        ss[l] = total;
        total += elem;
        ww[l] = elem;
        l++;
        if (elem > maxgrpn) maxgrpn = elem;
        for (int j = thisx; j < (thisx + ilen[i]); j++) {
          ians[k++] = io__[j];
        }
      }
    } else {
      for (int i=0; i<nlen; ++i) {
        if (!ihaving[i]) continue;
        int thisx = ix[i];
        int elem = ilen[i];
        ss[l] = total;
        total += elem;
        ww[l] = elem;
        l++;
        if (elem > maxgrpn) maxgrpn = elem;
        for (int j = thisx; j < (thisx + ilen[i]); j++) {
          ians[k++] = j;
        }
      }
    }
    //R_orderVector1(ians, total_true, ans, true, false);
    setAttrib(ans, sym_maxgrpn, ScalarInteger(maxgrpn));
  } else {
    int k = 0;
    if (LENGTH(o__)) {
      const int *io__ = INTEGER(o__); 
      for (int i=0; i<nlen; ++i) {
        if (!ihaving[i]) continue;
        int thisx = ix[i] - 1;
        for (int j = thisx; j < (thisx + ilen[i]); j++) {
          ians[k++] = io__[j];
        }
      }
    } else {
      for (int i=0; i<nlen; ++i) {
        if (!ihaving[i]) continue;
        int thisx = ix[i];
        for (int j = thisx; j < (thisx + ilen[i]); j++) {
          ians[k++] = j;
        }
      }
    }
  }
  
  UNPROTECT(1);
  return(ans);
}
