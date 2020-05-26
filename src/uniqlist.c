#include "data.table.h"

/* uniq macros */

#define COMPARE1                                                              \
prev = *vd;                                                                   \
for (int i=1; i<nrow; i++) {                                                  \
  elem = *++vd;                                                               \
  if (elem!=prev

#define COMPARE1_VIA_ORDER                                                    \
prev = vd[*o -1];                                                             \
for (int i=1; i<nrow; i++) {                                                  \
  elem = vd[*++o -1];                                                         \
  if (elem!=prev

#define COMPARE1_VIA_ORDER_SAFE                                               \
prev = vd[*o -1];                                                             \
for (int i=1; i<nrow; i++) {                                                  \
  int oi = *++o;                                                              \
  if (oi < 1 || oi > nrow)                                                    \
    error("'order' must be in range 1:nrow(x)");                              \
  elem = vd[oi -1];                                                           \
  if (elem!=prev

#define COMPARE2                                                              \
  ) {                                                                         \
    iidx[len++] = i+1;                                                        \
    if (len>=isize) {                                                         \
      isize = MIN(nrow, (size_t)(1.1*(double)isize*((double)nrow/i)));        \
      iidx = Realloc(iidx, isize, int);                                       \
    }                                                                         \
  }                                                                           \
  prev = elem;                                                                \
}

SEXP uniq(SEXP x, SEXP order, SEXP safe) {
  // safe=true ensure no segfault using COMPARE1_VIA_ORDER_SAFE
  // safe=false is not available in public API
  if (!isNewList(x))
    error(_("internal error: 'x' must be a data.table type object"));
  if (!isInteger(order))
    error(_("internal error: 'order' must be an integer"));
  if (!IS_TRUE_OR_FALSE(safe))
    error(_("internal error: 'safe' must be TRUE or FALSE"));
  const bool verbose = GetVerbose();
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  R_len_t ncol = length(x);
  R_len_t nrow = length(VECTOR_ELT(x,0));
  if (!ncol || !nrow) {
    SEXP ans = PROTECT(allocVector(INTSXP, 0));
    if (verbose)
      Rprintf(_("uniq: took %.3fs\n"), omp_get_wtime()-tic);
    UNPROTECT(1);
    return(ans);
  }
  bool via_order = LENGTH(order) > 0;
  bool via_order_safe = via_order && LOGICAL(safe)[0];
  if (via_order && LENGTH(order)!=nrow)
    error(_("internal error: uniq has been passed length(order)==%d but nrow==%d"), LENGTH(order), nrow);
  const int *o = INTEGER(order);  // only used when via_order[_safe] is true
  if (nrow==1) {
    if (via_order && o[0]!=1)
      error("'order' must be in range 1:nrow(x)");
    SEXP ans = PROTECT(allocVector(INTSXP, 1));
    INTEGER(ans)[0] = 1;
    if (verbose)
      Rprintf(_("uniq: took %.3fs\n"), omp_get_wtime()-tic);
    UNPROTECT(1);
    return(ans);
  }
  
  unsigned long long *ulv; // for numeric check speed-up (to overcome NA/NaN/Inf/-Inf comparisons) (> 2x speed-up)
  R_len_t isize=1000, len=1;
  int *iidx = Calloc(isize, int); // for 'idx'
  iidx[0] = 1; // first row is always the first of the first group
  if (ncol==1) {
    SEXP v = VECTOR_ELT(x,0);
    switch(TYPEOF(v)) {
    case INTSXP : case LGLSXP : {
      const int *vd=INTEGER(v);
      int prev, elem;
      if (via_order_safe) {
        COMPARE1_VIA_ORDER_SAFE COMPARE2
      } else if (via_order) {
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
      if (via_order_safe) {
        COMPARE1_VIA_ORDER_SAFE && ENC2UTF8(elem)!=ENC2UTF8(prev) COMPARE2
      } else if (via_order) {
        COMPARE1_VIA_ORDER && ENC2UTF8(elem)!=ENC2UTF8(prev) COMPARE2   // but most of the time they are equal, so ENC2UTF8 doesn't need to be called
      } else {
        COMPARE1           && ENC2UTF8(elem)!=ENC2UTF8(prev) COMPARE2
      }
    } break;
    case REALSXP : {
      const uint64_t *vd=(const uint64_t *)REAL(v);
      uint64_t prev, elem;
      // grouping by integer64 makes sense (ids). grouping by float supported but a good use-case for that is harder to imagine
      if (getNumericRounding_C()==0 /*default*/ ||
          Rinherits(v,char_integer64)) {
        if (via_order_safe) {
          COMPARE1_VIA_ORDER_SAFE COMPARE2
        } else if (via_order) {
          COMPARE1_VIA_ORDER COMPARE2
        } else {
          COMPARE1           COMPARE2
        }
      } else {
        if (via_order_safe) {
          COMPARE1_VIA_ORDER_SAFE && dtwiddle(&elem, 0)!=dtwiddle(&prev, 0) COMPARE2
        } else if (via_order) {
          COMPARE1_VIA_ORDER && dtwiddle(&elem, 0)!=dtwiddle(&prev, 0) COMPARE2
        } else {
          COMPARE1           && dtwiddle(&elem, 0)!=dtwiddle(&prev, 0) COMPARE2
        }
      }
    } break;
    default : {
      error(_("Type '%s' not supported"), type2char(TYPEOF(v)));
    }
    }
  } else { // ncol>1
    R_len_t previ, thisi = via_order ? o[0]-1 : 0;
    bool *i64 = (bool *)R_alloc(ncol, sizeof(bool));
    for (int i=0; i<ncol; i++)
      i64[i] = Rinherits(VECTOR_ELT(x,i),char_integer64);
    for (int i=1; i<nrow; i++) {
      previ = thisi;
      if (!via_order) {
        thisi = i;
      } else {
        int oi = o[i];
        if (via_order_safe && (oi < 1 || oi > nrow))
          error("'order' must be in range 1:nrow(x)");
        thisi = oi-1;
      }
      int j = ncol;  // the last column varies the most frequently so check that first and work backwards
      bool same = true; // flag to indicate if the values in a row are same as the previous row, if flag false then we can move on to next group
      while (--j>=0 && same) {
        SEXP v = VECTOR_ELT(x,j);
        switch (TYPEOF(v)) {
        case INTSXP : case LGLSXP : { // NA_INTEGER==NA_LOGICAL checked in init.c
          same = INTEGER(v)[thisi]==INTEGER(v)[previ];
        } break;
        case STRSXP : {
          // fix for #469, when key is set, duplicated calls uniqlist, where encoding
          // needs to be taken care of.
          same = ENC2UTF8(STRING_ELT(v,thisi))==ENC2UTF8(STRING_ELT(v,previ));
        } break;  // marked non-utf8 encodings are converted to utf8 so as to match properly when inputs are of different encodings.
        case REALSXP : {
          ulv = (unsigned long long *)REAL(v);
          same = ulv[thisi] == ulv[previ]; // (gives >=2x speedup)
          if (!same && !i64[j]) {
            same = dtwiddle(ulv, thisi) == dtwiddle(ulv, previ);
            // could store LHS for use next time as RHS (to save calling dtwiddle twice). However: i) there could be multiple double columns so vector of RHS would need
            // to be stored, ii) many short-circuit early before the if (!b) anyway (negating benefit) and iii) we may not have needed LHS this time so logic would be complex.
          }
        } break;
        default : {
          error(_("Type '%s' not supported"), type2char(TYPEOF(v)));
        }
        }
      }
      if (!same) {
        iidx[len++] = i+1;
        if (len >= isize) {
          isize = MIN(nrow, (size_t)(1.1*(double)isize*((double)nrow/i)));
          iidx = Realloc(iidx, isize, int);
        }
      } // almost like COMPARE2 but no last line: prev = elem
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, len));
  memcpy(INTEGER(ans), iidx, sizeof(int)*len); // sizeof is of type size_t - no integer overflow issues
  Free(iidx);
  if (verbose)
    Rprintf(_("uniq: took %.3fs\n"), omp_get_wtime()-tic);
  UNPROTECT(1);
  return(ans);
}

SEXP uniqlengths(SEXP x, SEXP n) {
  if (!isInteger(x))
    error(_("internal error: Input argument 'x' to 'uniqlengths' must be an integer vector")); // # nocov
  if (!isInteger(x) || length(n)!=1 || INTEGER(n)[0]==NA_INTEGER)
    error(_("internal error: Input argument 'n' to 'uniqlengths' must be an integer vector of length 1 non NA")); // # nocov
  R_len_t len = length(x);
  SEXP ans = PROTECT(allocVector(INTSXP, len));
  const int *xp = INTEGER(x);
  int *ansp = INTEGER(ans);
  for (R_len_t i=1; i<len; ++i) {
    ansp[i-1] = xp[i] - xp[i-1];
  }
  if (len>0)
    ansp[len-1] = INTEGER(n)[0] - xp[len-1] + 1;
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
  if (!isLogical(x))
    error(_("internal error: x is not a logical vector")); // # nocov
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("na.rm must be TRUE or FALSE"));
  bool narm = LOGICAL(narmArg)[0]==1;
  const R_xlen_t n = xlength(x);
  if (n==0)
    return ScalarInteger(0);  // empty vector
  Rboolean first = LOGICAL(x)[0];
  R_xlen_t i=0;
  const int *ix = LOGICAL(x);
  while (++i<n && ix[i]==first);
  if (i==n) { // all values the same
    if (first==NA_INTEGER && narm) {
      return ScalarInteger(0);
    } else {
      return ScalarInteger(1);
    }
  }
  Rboolean second = ix[i];
  // we've found 2 different values (first and second). Which one didn't we find? Then just look for that.
  // NA_LOGICAL == INT_MIN checked in init.c
  int third;
  if (first+second == 1) {
    third = NA_LOGICAL;
  } else {
    third = first+second == INT_MIN;
  }
  if (third==NA_LOGICAL && narm)
    return ScalarInteger(2); // TRUE and FALSE found before any NA, but na.rm=TRUE so we're done
  while (++i<n) {
    if (ix[i]==third)
      return ScalarInteger(3-narm);
  }
  return ScalarInteger(2-(narm && third!=NA_LOGICAL));
}
