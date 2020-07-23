#include "data.table.h"

// DONE: return 'uniqlist' as a vector (same as duplist) and write a separate function to get group sizes
// Also improvements for numeric type with a hack of checking unsigned int (to overcome NA/NaN/Inf/-Inf comparisons) (> 2x speed-up)
SEXP uniqlist(SEXP l, SEXP order)
{
  // This works like UNIX uniq as referred to by ?base::unique; i.e., it
  // drops immediately repeated rows but doesn't drop duplicates of any
  // previous row. Unless, order is provided, then it also drops any previous
  // row. l must be a list of same length vectors ans is allocated first
  // (maximum length the number of rows) and the length returned in anslen.
  // No NA in order which is guaranteed since internal-only. Used at R level internally (Cuniqlist) but is not and should not be exported.
  // DONE: ans is now grown
  if (!isNewList(l)) error(_("Internal error: uniqlist has not been passed a list of columns")); // # nocov
  R_len_t ncol = length(l);
  R_len_t nrow = length(VECTOR_ELT(l,0));
  if (!isInteger(order)) error(_("Internal error: uniqlist has been passed a non-integer order")); // # nocov
  if (LENGTH(order)<1) error(_("Internal error: uniqlist has been passed a length-0 order")); // # nocov
  if (LENGTH(order)>1 && LENGTH(order)!=nrow) error(_("Internal error: uniqlist has been passed length(order)==%d but nrow==%d"), LENGTH(order), nrow); // # nocov
  bool via_order = INTEGER(order)[0] != -1;  // has an ordering vector been passed in that we have to hop via? Don't use MISSING() here as it appears unstable on Windows

  unsigned long long *ulv; // for numeric check speed-up
  SEXP v, ans;
  R_len_t len, thisi, previ, isize=1000;
  int *iidx = Calloc(isize, int); // for 'idx'
  len = 1;
  iidx[0] = 1; // first row is always the first of the first group

  if (ncol==1) {

#define COMPARE1                                                                 \
      prev = *vd;                                                                \
      for (int i=1; i<nrow; i++) {                                               \
        elem = *++vd;                                                            \
        if (elem!=prev

#define COMPARE1_VIA_ORDER                                                       \
      prev = vd[*o -1];                                                          \
      for (int i=1; i<nrow; i++) {                                               \
        elem = vd[*++o -1];                                                      \
        if (elem!=prev

#define COMPARE2                                                                 \
                        ) {                                                      \
          iidx[len++] = i+1;                                                     \
          if (len>=isize) {                                                      \
            isize = MIN(nrow, (size_t)(1.1*(double)isize*((double)nrow/i)));     \
            iidx = Realloc(iidx, isize, int);                                    \
          }                                                                      \
        }                                                                        \
        prev = elem;                                                             \
      }

    SEXP v = VECTOR_ELT(l,0);
    int *o = INTEGER(order);  // only used when via_order is true
    switch(TYPEOF(v)) {
    case INTSXP : case LGLSXP : {
      const int *vd=INTEGER(v);
      int prev, elem;
      if (via_order) {
        // ad hoc by (order passed in)
        COMPARE1_VIA_ORDER COMPARE2
      } else {
        // e.g. by=key(DT)[1]
        COMPARE1           COMPARE2
      }
    } break;
    case STRSXP : {
      const SEXP *vd=STRING_PTR(v);
      SEXP prev, elem;
      if (via_order) {
        COMPARE1_VIA_ORDER && ENC2UTF8(elem)!=ENC2UTF8(prev) COMPARE2   // but most of the time they are equal, so ENC2UTF8 doesn't need to be called
      } else {
        COMPARE1           && ENC2UTF8(elem)!=ENC2UTF8(prev) COMPARE2
      }
    } break;
    case REALSXP : {
      const uint64_t *vd=(const uint64_t *)REAL(v);
      uint64_t prev, elem;
      // grouping by integer64 makes sense (ids). grouping by float supported but a good use-case for that is harder to imagine
      if (getNumericRounding_C()==0 /*default*/ || inherits(v, "integer64")) {
        if (via_order) {
          COMPARE1_VIA_ORDER COMPARE2
        } else {
          COMPARE1           COMPARE2
        }
      } else {
        if (via_order) {
          COMPARE1_VIA_ORDER && dtwiddle(&elem, 0)!=dtwiddle(&prev, 0) COMPARE2
        } else {
          COMPARE1           && dtwiddle(&elem, 0)!=dtwiddle(&prev, 0) COMPARE2
        }
      }
    } break;
    default :
      error(_("Type '%s' not supported"), type2char(TYPEOF(v)));  // # nocov
    }
  } else {
    // ncol>1
    thisi = via_order ? INTEGER(order)[0]-1 : 0;
    bool *i64 = (bool *)R_alloc(ncol, sizeof(bool));
    for (int i=0; i<ncol; i++) i64[i] = INHERITS(VECTOR_ELT(l,i), char_integer64);
    for (int i=1; i<nrow; i++) {
      previ = thisi;
      thisi = via_order ? INTEGER(order)[i]-1 : i;
      int j = ncol;  // the last column varies the most frequently so check that first and work backwards
      bool b = true;
      while (--j>=0 && b) {
        v=VECTOR_ELT(l,j);
        switch (TYPEOF(v)) {
        case INTSXP : case LGLSXP :  // NA_INTEGER==NA_LOGICAL checked in init.c
          b=INTEGER(v)[thisi]==INTEGER(v)[previ]; break;
        case STRSXP :
          // fix for #469, when key is set, duplicated calls uniqlist, where encoding
          // needs to be taken care of.
          b=ENC2UTF8(STRING_ELT(v,thisi))==ENC2UTF8(STRING_ELT(v,previ)); break;  // marked non-utf8 encodings are converted to utf8 so as to match properly when inputs are of different encodings.
          // TODO: surely faster way than this two deep STRING_ELT()
        case REALSXP :
          ulv = (unsigned long long *)REAL(v);
          b = ulv[thisi] == ulv[previ]; // (gives >=2x speedup)
          if (!b && !i64[j]) {
            b = dtwiddle(ulv, thisi) == dtwiddle(ulv, previ);
            // could store LHS for use next time as RHS (to save calling dtwiddle twice). However: i) there could be multiple double columns so vector of RHS would need
            // to be stored, ii) many short-circuit early before the if (!b) anyway (negating benefit) and iii) we may not have needed LHS this time so logic would be complex.
          }
          break;
        default :
          error(_("Type '%s' not supported"), type2char(TYPEOF(v)));  // # nocov
        }
      }
      if (!b) {
        iidx[len++] = i+1;
        if (len >= isize) {
          isize = MIN(nrow, (size_t)(1.1*(double)isize*((double)nrow/i)));
          iidx = Realloc(iidx, isize, int);
        }
      }
    }
  }
  PROTECT(ans = allocVector(INTSXP, len));
  memcpy(INTEGER(ans), iidx, sizeof(int)*len); // sizeof is of type size_t - no integer overflow issues
  Free(iidx);
  UNPROTECT(1);
  return(ans);
}

SEXP uniqlengths(SEXP x, SEXP n) {
  // seems very similar to rbindlist.c:uniq_lengths. TODO: centralize into common function
  if (TYPEOF(x) != INTSXP) error(_("Input argument 'x' to 'uniqlengths' must be an integer vector"));
  if (TYPEOF(n) != INTSXP || length(n) != 1) error(_("Input argument 'n' to 'uniqlengths' must be an integer vector of length 1"));
  R_len_t len = length(x);
  SEXP ans = PROTECT(allocVector(INTSXP, len));
  for (R_len_t i=1; i<len; i++) {
    INTEGER(ans)[i-1] = INTEGER(x)[i] - INTEGER(x)[i-1];
  }
  if (len>0) INTEGER(ans)[len-1] = INTEGER(n)[0] - INTEGER(x)[len-1] + 1;
  UNPROTECT(1);
  return(ans);
}

// we could compute `uniqlist` and `uniqlengths` and then construct the result
// but that seems unnecessary waste of memory and roundabout..
// so, we'll do it directly here..
SEXP rleid(SEXP l, SEXP cols) {
  R_xlen_t nrow = xlength(VECTOR_ELT(l, 0));
  R_len_t ncol = length(l), lencols = length(cols);
  if (!nrow || !ncol) return(allocVector(INTSXP, 0));
  if (!isInteger(cols) || lencols==0) error(_("cols must be an integer vector with length >= 1"));
  int *icols = INTEGER(cols);
  for (int i=0; i<lencols; i++) {
    int elem = icols[i];
    if (elem<1 || elem>ncol) error(_("Item %d of cols is %d which is outside range of l [1,length(l)=%d]"), i+1, elem, ncol);
  }
  for (int i=1; i<ncol; i++) {
    if (xlength(VECTOR_ELT(l,i)) != nrow) error(_("All elements to input list must be of same length. Element [%d] has length %"PRIu64" != length of first element = %"PRIu64"."), i+1, (uint64_t)xlength(VECTOR_ELT(l,i)), (uint64_t)nrow);
  }
  SEXP ans = PROTECT(allocVector(INTSXP, nrow));
  int *ians = INTEGER(ans);
  int grp = 1;
  ians[0] = grp; // first row is always the first of first group
  if (ncol > 1) {
    for (R_xlen_t i=1; i<nrow; i++) {
      bool same = true;
      int j = lencols;
      // the last column varies the most frequently so check that first and work backwards
      while (--j>=0 && same) {
        SEXP jcol = VECTOR_ELT(l, icols[j]-1);
        switch (TYPEOF(jcol)) {
        case INTSXP : case LGLSXP :
          same = INTEGER(jcol)[i]==INTEGER(jcol)[i-1];
          break;
        case STRSXP :
          same = STRING_ELT(jcol,i)==STRING_ELT(jcol,i-1);
          // TODO: do we want to check encodings here now that forder seems to?
          // Old comment : forder checks no non-ascii unknown, and either UTF-8 or Latin1 but not both.
          //               So == pointers is ok given that check
          break;
        case REALSXP : {
          long long *ll = (long long *)REAL(jcol);
          same = ll[i]==ll[i-1];
          // 8 bytes of bits are identical. For real (no rounding currently) and integer64
          // long long == 8 bytes checked in init.c
        } break;
        case CPLXSXP: {
          Rcomplex *pz = COMPLEX(jcol);
          same = memcmp(&pz[i], &pz[i-1], sizeof(Rcomplex))==0; // compiler optimization should replace library call with best 16-byte fixed method
        } break;
        default :
          error(_("Type '%s' not supported"), type2char(TYPEOF(jcol)));  // # nocov
        }
      }
      ians[i] = (grp+=!same);
    }
  } else { // we are checking only one column so we can easily takes inline R functions out of the loops
    SEXP jcol = VECTOR_ELT(l, icols[0]-1);
    switch (TYPEOF(jcol)) {
    case INTSXP : case LGLSXP : {
      int *ijcol = INTEGER(jcol);
      for (R_xlen_t i=1; i<nrow; i++) {
        bool same = ijcol[i]==ijcol[i-1];
        ians[i] = (grp+=!same);
      }
    } break;
    case STRSXP : {
      const SEXP *jd = STRING_PTR(jcol);
      for (R_xlen_t i=1; i<nrow; i++) {
        bool same = jd[i]==jd[i-1];
        ians[i] = (grp+=!same);
      }
    } break;
    case REALSXP : {
      long long *lljcol = (long long *)REAL(jcol);
      for (R_xlen_t i=1; i<nrow; i++) {
        bool same = lljcol[i]==lljcol[i-1];
        ians[i] = (grp+=!same);
      }
    } break;
    case CPLXSXP: {
      Rcomplex *pzjcol = COMPLEX(jcol);
      for (R_xlen_t i=1; i<nrow; i++) {
        bool same = memcmp(&pzjcol[i], &pzjcol[i-1], sizeof(Rcomplex))==0;
        ians[i] = (grp += !same);
      }
    } break;
    default :
      error(_("Type '%s' not supported"), type2char(TYPEOF(jcol)));
    }
  }
  UNPROTECT(1);
  return(ans);
}

SEXP nestedid(SEXP l, SEXP cols, SEXP order, SEXP grps, SEXP resetvals, SEXP multArg) {
  Rboolean byorder = (length(order)>0);
  SEXP v, ans;
  if (!isNewList(l) || length(l) < 1) error(_("Internal error: nestedid was not passed a list length 1 or more")); // # nocov
  R_len_t nrows = length(VECTOR_ELT(l,0)), ncols = length(cols);
  if (nrows==0) return(allocVector(INTSXP, 0));
  R_len_t thisi, previ, ansgrpsize=1000, nansgrp=0;
  R_len_t *ansgrp = Calloc(ansgrpsize, R_len_t), starts, grplen; // #3401 fix. Needs to be Calloc due to Realloc below .. else segfaults.
  R_len_t ngrps = length(grps);
  bool *i64 = (bool *)R_alloc(ncols, sizeof(bool));
  if (ngrps==0) error(_("Internal error: nrows[%d]>0 but ngrps==0"), nrows); // # nocov
  R_len_t resetctr=0, rlen = length(resetvals) ? INTEGER(resetvals)[0] : 0;
  if (!isInteger(cols) || ncols == 0) error(_("cols must be an integer vector of positive length"));
  // mult arg
  enum {ALL, FIRST, LAST} mult = ALL;
  if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "all")) mult = ALL;
  else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "first")) mult = FIRST;
  else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "last")) mult = LAST;
  else error(_("Internal error: invalid value for 'mult'. please report to data.table issue tracker")); // # nocov
  // integer64
  for (int j=0; j<ncols; j++) {
    i64[j] = INHERITS(VECTOR_ELT(l, INTEGER(cols)[j]-1), char_integer64);
  }
  ans  = PROTECT(allocVector(INTSXP, nrows));
  int *ians = INTEGER(ans), *igrps = INTEGER(grps);
  grplen = (ngrps == 1) ? nrows : igrps[1]-igrps[0];
  starts = igrps[0]-1 + (mult != LAST ? 0 : grplen-1);
  ansgrp[0] = byorder ? INTEGER(order)[starts]-1 : starts;
  for (int j=0; j<grplen; j++) {
    ians[byorder ? INTEGER(order)[igrps[0]-1+j]-1 : igrps[0]-1+j] = 1;
  }
  nansgrp = 1;
  for (int i=1; i<ngrps; i++) {
    // "first"=add next grp to current grp iff min(next) >= min(current)
    // "last"=add next grp to current grp iff max(next) >= max(current)
    // in addition to this thisi >= previ should be satisfied
    // could result in more groups.. so done only for first/last cases
    // as it allows to extract indices directly in bmerge.
    grplen = (i+1 < ngrps) ? igrps[i+1]-igrps[i] : nrows-igrps[i]+1;
    starts = igrps[i]-1 + (mult != LAST ? 0 : grplen-1);
    thisi = byorder ? INTEGER(order)[starts]-1 : starts;
    Rboolean b = TRUE;
    int k = 0;
    for (; k<nansgrp; k++) {
      int j = ncols;
      previ = ansgrp[k];
      // b=TRUE is ideal for mult=ALL, results in lesser groups
      b = mult == ALL || (thisi >= previ);
      // >= 0 is not necessary as first col will always be in
      // increasing order. NOTE: all "==" cols are already skipped for
      // computing nestedid during R-side call, for efficiency.
      while(b && --j>0) {
        v = VECTOR_ELT(l,INTEGER(cols)[j]-1);
        switch(TYPEOF(v)) {
        case INTSXP: case LGLSXP:
          b = INTEGER(v)[thisi] >= INTEGER(v)[previ];
          break;
        case STRSXP :
          b = ENC2UTF8(STRING_ELT(v,thisi)) == ENC2UTF8(STRING_ELT(v,previ));
          break;
        case REALSXP: {
          double *xd = REAL(v);
          b = i64[j] ? ((int64_t *)xd)[thisi] >= ((int64_t *)xd)[previ] :
                       dtwiddle(xd, thisi) >= dtwiddle(xd, previ);
        } break;
        default:
          error(_("Type '%s' not supported"), type2char(TYPEOF(v)));  // # nocov
        }
      }
      if (b) break;
    }
    // TODO: move this as the outer for-loop and parallelise..
    // but preferably wait to see if problems with that big non-equi
    // group sizes do occur that commonly before to invest time here.
    int tmp=0;
    if (rlen != starts) {
      tmp = b ? k : nansgrp++;
    } else { // we're wrapping up this group, reset nansgrp
      tmp = 0; nansgrp = 1;
      rlen += INTEGER(resetvals)[++resetctr];
    }
    if (nansgrp >= ansgrpsize) {
      ansgrpsize = MIN(nrows, (size_t)(1.1*(double)ansgrpsize*((double)nrows/i)));
      ansgrp = Realloc(ansgrp, ansgrpsize, int);
    }
    for (int j=0; j<grplen; j++) {
      ians[byorder ? INTEGER(order)[igrps[i]-1+j]-1 : igrps[i]-1+j] = tmp+1;
    }
    ansgrp[tmp] = thisi;
  }
  Free(ansgrp);
  UNPROTECT(1);
  return(ans);
}

SEXP uniqueNlogical(SEXP x, SEXP narmArg) {
  // single pass; short-circuit and return as soon as all 3 values are found
  if (!isLogical(x)) error(_("x is not a logical vector"));
  if (!isLogical(narmArg) || length(narmArg)!=1 || INTEGER(narmArg)[0]==NA_INTEGER) error(_("na.rm must be TRUE or FALSE"));
  bool narm = LOGICAL(narmArg)[0]==1;
  const R_xlen_t n = xlength(x);
  if (n==0)
    return ScalarInteger(0);  // empty vector
  Rboolean first = LOGICAL(x)[0];
  R_xlen_t i=0;
  const int *ix = LOGICAL(x);
  while (++i<n && ix[i]==first);
  if (i==n)
    return ScalarInteger(first==NA_INTEGER && narm ? 0 : 1); // all one value
  Rboolean second = ix[i];
  // we've found 2 different values (first and second). Which one didn't we find? Then just look for that.
  // NA_LOGICAL == INT_MIN checked in init.c
  const int third = (first+second == 1) ? NA_LOGICAL : ( first+second == INT_MIN ? TRUE : FALSE );
  if (third==NA_LOGICAL && narm)
    return ScalarInteger(2);  // TRUE and FALSE found before any NA, but na.rm=TRUE so we're done
  while (++i<n) if (ix[i]==third)
    return ScalarInteger(3-narm);
  return ScalarInteger(2-(narm && third!=NA_LOGICAL));
}

