#include "data.table.h"

void subsetVectorRaw(SEXP ans, SEXP source, SEXP idx, const bool anyNA)
// Used here by subsetDT() and by dogroups.c
{
  const int n = length(idx);
  if (length(ans)!=n) error(_("Internal error: subsetVectorRaw length(ans)==%d n=%d"), length(ans), n);

  const int *restrict idxp = INTEGER(idx);
  // anyNA refers to NA _in idx_; if there's NA in the data (source) that's just regular data to be copied
  // negatives, zeros and out-of-bounds have already been dealt with in convertNegAndZero so we can rely
  // here on idx in range [1,length(ans)].

  const int nth = getDTthreads(n, /*throttle=*/true);
  // For small n such as 2,3,4 etc we had hoped OpenMP would be sensible inside it and not create a team
  // with each thread doing just one item. Otherwise, call overhead would be too high for highly iterated
  // calls on very small subsets. Timings were tested in #3175. However, the overhead does seem to add up
  // significantly. Hence the throttle was introduced, #4484. And not having the OpenMP region at all here
  // when nth==1 (the ifs below in PARLOOP) seems to help too, #4200.
  // To stress test the code for correctness by forcing multi-threading on for small data, the throttle can
  // be turned off using setDThreads() or R_DATATABLE_THROTTLE environment variable.

  #define PARLOOP(_NAVAL_)                                        \
  if (anyNA) {                                                    \
    if (nth>1) {                                                  \
      _Pragma("omp parallel for num_threads(nth)")                \
      for (int i=0; i<n; ++i) {                                   \
        int elem = idxp[i];                                       \
        ap[i] = elem==NA_INTEGER ? _NAVAL_ : sp[elem-1];          \
      }                                                           \
    } else {                                                      \
      for (int i=0; i<n; ++i) {                                   \
        int elem = idxp[i];                                       \
        ap[i] = elem==NA_INTEGER ? _NAVAL_ : sp[elem-1];          \
      }                                                           \
    }                                                             \
  } else {                                                        \
    if (nth>1) {                                                  \
      _Pragma("omp parallel for num_threads(nth)")                \
      for (int i=0; i<n; ++i) {                                   \
        ap[i] = sp[idxp[i]-1];                                    \
      }                                                           \
    } else {                                                      \
      for (int i=0; i<n; ++i) {                                   \
        ap[i] = sp[idxp[i]-1];                                    \
      }                                                           \
    }                                                             \
  }

  switch(TYPEOF(source)) {
  case INTSXP: case LGLSXP: {
    int *sp = INTEGER(source);
    int *ap = INTEGER(ans);
    PARLOOP(NA_INTEGER)
  } break;
  case REALSXP : {
    if (INHERITS(source, char_integer64)) {
      int64_t *sp = (int64_t *)REAL(source);
      int64_t *ap = (int64_t *)REAL(ans);
      PARLOOP(INT64_MIN)
    } else {
      double *sp = REAL(source);
      double *ap = REAL(ans);
      PARLOOP(NA_REAL)
    }
  } break;
  case STRSXP : {
    // write barrier (assigning strings/lists) is not thread safe. Hence single threaded.
    // To go parallel here would need access to NODE_IS_OLDER, at least. Given gcgen, mark and named
    // are upper bounded and max 3, REFCNT==REFCNTMAX could be checked first and then critical SET_ if not.
    // Inside that critical just before SET_ it could check REFCNT<REFCNTMAX still held. Similarly for gcgen.
    // TODO - discuss with Luke Tierney. Produce benchmarks on integer/double to see if it's worth making a safe
    //        API interface for package use for STRSXP.
    // Aside: setkey() is a separate special case (a permutation) and does do this in parallel without using SET_*.
    const SEXP *sp = SEXPPTR_RO(source);
    if (anyNA) {
      for (int i=0; i<n; i++) { int elem = idxp[i]; SET_STRING_ELT(ans, i, elem==NA_INTEGER ? NA_STRING : sp[elem-1]); }
    } else {
      for (int i=0; i<n; i++) {                     SET_STRING_ELT(ans, i, sp[idxp[i]-1]); }
    }
  } break;
  case VECSXP : {
    const SEXP *sp = SEXPPTR_RO(source);
    if (anyNA) {
      for (int i=0; i<n; i++) { int elem = idxp[i]; SET_VECTOR_ELT(ans, i, elem==NA_INTEGER ? R_NilValue : sp[elem-1]); }
    } else {
      for (int i=0; i<n; i++) {                     SET_VECTOR_ELT(ans, i, sp[idxp[i]-1]); }
    }
  } break;
  case CPLXSXP : {
    Rcomplex *sp = COMPLEX(source);
    Rcomplex *ap = COMPLEX(ans);
    PARLOOP(NA_CPLX)
  } break;
  case RAWSXP : {
    Rbyte *sp = RAW(source);
    Rbyte *ap = RAW(ans);
    PARLOOP(0)
  } break;
  default :
    error(_("Internal error: column type '%s' not supported by data.table subset. All known types are supported so please report as bug."), type2char(TYPEOF(source)));  // # nocov
  }
}

static const char *check_idx(SEXP idx, int max, bool *anyNA_out, bool *orderedSubset_out)
// set anyNA for branchless subsetVectorRaw
// error if any negatives, zeros or >max since they should have been dealt with by convertNegAndZeroIdx() called ealier at R level.
// single cache efficient sweep with prefetch, so very low priority to go parallel
{
  if (!isInteger(idx)) error(_("Internal error. 'idx' is type '%s' not 'integer'"), type2char(TYPEOF(idx))); // # nocov
  bool anyLess=false, anyNA=false;
  int last = INT32_MIN;
  int *idxp = INTEGER(idx), n=LENGTH(idx);
  for (int i=0; i<n; i++) {
    int elem = idxp[i];
    if (elem<=0 && elem!=NA_INTEGER) return "Internal inefficiency: idx contains negatives or zeros. Should have been dealt with earlier.";  // e.g. test 762  (TODO-fix)
    if (elem>max) return "Internal inefficiency: idx contains an item out-of-range. Should have been dealt with earlier.";                   // e.g. test 1639.64
    anyNA |= elem==NA_INTEGER;
    anyLess |= elem<last;
    last = elem;
  }
  *anyNA_out = anyNA;
  *orderedSubset_out = !anyLess; // for the purpose of ordered keys elem==last is allowed
  return NULL;
}

SEXP convertNegAndZeroIdx(SEXP idx, SEXP maxArg, SEXP allowOverMax)
{
  // called from [.data.table to massage user input, creating a new strictly positive idx if there are any negatives or zeros
  // + more precise and helpful error messages telling user exactly where the problem is (saving user debugging time)
  // + a little more efficient than negativeSubscript in src/main/subscript.c (it's private to R so we can't call it anyway)
  // allowOverMaxArg is false when := (test 1024), otherwise true for selecting

  if (!isInteger(idx)) error(_("Internal error. 'idx' is type '%s' not 'integer'"), type2char(TYPEOF(idx))); // # nocov
  if (!isInteger(maxArg) || length(maxArg)!=1) error(_("Internal error. 'maxArg' is type '%s' and length %d, should be an integer singleton"), type2char(TYPEOF(maxArg)), length(maxArg)); // # nocov
  if (!isLogical(allowOverMax) || LENGTH(allowOverMax)!=1 || LOGICAL(allowOverMax)[0]==NA_LOGICAL) error(_("Internal error: allowOverMax must be TRUE/FALSE"));  // # nocov
  int max = INTEGER(maxArg)[0], n=LENGTH(idx);
  if (max<0) error(_("Internal error. max is %d, must be >= 0."), max); // # nocov    includes NA which will print as INT_MIN
  int *idxp = INTEGER(idx);

  bool stop = false;
  #pragma omp parallel for num_threads(getDTthreads(n, true))
  for (int i=0; i<n; i++) {
    if (stop) continue;
    int elem = idxp[i];
    if ((elem<1 && elem!=NA_INTEGER) || elem>max) stop=true;
  }
  if (!stop) return(idx); // most common case to return early: no 0, no negative; all idx either NA or in range [1-max]

  // ---------
  // else massage the input to a standard idx where all items are either NA or in range [1,max] ...

  int countNeg=0, countZero=0, countNA=0, firstOverMax=0;
  for (int i=0; i<n; i++) {
    int elem = idxp[i];
    if (elem==NA_INTEGER) countNA++;
    else if (elem<0) countNeg++;
    else if (elem==0) countZero++;
    else if (elem>max && firstOverMax==0) firstOverMax=i+1;
  }
  if (firstOverMax && LOGICAL(allowOverMax)[0]==FALSE) {
    error(_("i[%d] is %d which is out of range [1,nrow=%d]"), firstOverMax, idxp[firstOverMax-1], max);
  }

  int countPos = n-countNeg-countZero-countNA;
  if (countPos && countNeg) {
    int i=0, firstNeg=0, firstPos=0;
    while (i<n && (firstNeg==0 || firstPos==0)) {
      int elem = idxp[i];
      if (firstPos==0 && elem>0) firstPos=i+1;
      if (firstNeg==0 && elem<0 && elem!=NA_INTEGER) firstNeg=i+1;
      i++;
    }
    error(_("Item %d of i is %d and item %d is %d. Cannot mix positives and negatives."), firstNeg, idxp[firstNeg-1], firstPos, idxp[firstPos-1]);
  }
  if (countNeg && countNA) {
    int i=0, firstNeg=0, firstNA=0;
    while (i<n && (firstNeg==0 || firstNA==0)) {
      int elem = idxp[i];
      if (firstNeg==0 && elem<0 && elem!=NA_INTEGER) firstNeg=i+1;
      if (firstNA==0 && elem==NA_INTEGER) firstNA=i+1;
      i++;
    }
    error(_("Item %d of i is %d and item %d is NA. Cannot mix negatives and NA."), firstNeg, idxp[firstNeg-1], firstNA);
  }

  SEXP ans;
  if (countNeg==0) {
    // just zeros to remove, or >max to convert to NA
    ans = PROTECT(allocVector(INTSXP, n - countZero));
    int *ansp = INTEGER(ans);
    for (int i=0, ansi=0; i<n; i++) {
      int elem = idxp[i];
      if (elem==0) continue;
      ansp[ansi++] = elem>max ? NA_INTEGER : elem;
    }
  } else {
    // idx is all negative without any NA but perhaps some zeros
    bool *keep = (bool *)R_alloc(max, sizeof(bool));    // 4 times less memory that INTSXP in src/main/subscript.c
    for (int i=0; i<max; i++) keep[i] = true;
    int countRemoved=0, countDup=0, countBeyond=0;   // idx=c(-10,-5,-10) removing row 10 twice
    int firstBeyond=0, firstDup=0;
    for (int i=0; i<n; i++) {
      int elem = -idxp[i];
      if (elem==0) continue;
      if (elem>max) {
        countBeyond++;
        if (firstBeyond==0) firstBeyond=i+1;
        continue;
      }
      if (!keep[elem-1]) {
        countDup++;
        if (firstDup==0) firstDup=i+1;
      } else {
        keep[elem-1] = false;
        countRemoved++;
      }
    }
    if (countBeyond)
      warning(_("Item %d of i is %d but there are only %d rows. Ignoring this and %d more like it out of %d."), firstBeyond, idxp[firstBeyond-1], max, countBeyond-1, n);
    if (countDup)
      warning(_("Item %d of i is %d which removes that item but that has occurred before. Ignoring this dup and %d other dups."), firstDup, idxp[firstDup-1], countDup-1);
    int ansn = max-countRemoved;
    ans = PROTECT(allocVector(INTSXP, ansn));
    int *ansp = INTEGER(ans);
    for (int i=0, ansi=0; i<max; i++) {
      if (keep[i]) ansp[ansi++] = i+1;
    }
  }
  UNPROTECT(1);
  return ans;
}

static void checkCol(SEXP col, int colNum, int nrow, SEXP x)
{
  if (isNull(col)) error(_("Column %d is NULL; malformed data.table."), colNum);
  if (isNewList(col) && INHERITS(col, char_dataframe)) {
    SEXP names = getAttrib(x, R_NamesSymbol);
    error(_("Column %d ['%s'] is a data.frame or data.table; malformed data.table."),
          colNum, isNull(names)?"":CHAR(STRING_ELT(names,colNum-1)));
  }
  if (length(col)!=nrow) {
    SEXP names = getAttrib(x, R_NamesSymbol);
    error(_("Column %d ['%s'] is length %d but column 1 is length %d; malformed data.table."),
          colNum, isNull(names)?"":CHAR(STRING_ELT(names,colNum-1)), length(col), nrow);
  }
}

/*
* subsetDT - Subsets a data.table
* NOTE:
*   1) 'rows' and 'cols' are 1-based, passed from R level
*   2) Originally for subsetting vectors in fcast and now the beginnings of [.data.table ported to C
*   3) Immediate need is for R 3.1 as lglVec[1] now returns R's global TRUE and we don't want := to change that global [think 1 row data.tables]
*   4) Could do it other ways but may as well go to C now as we were going to do that anyway
*/

SEXP subsetDT(SEXP x, SEXP rows, SEXP cols) { // API change needs update NEWS.md and man/cdt.Rd
  int nprotect=0;
  if (!isNewList(x)) error(_("Internal error. Argument 'x' to CsubsetDT is type '%s' not 'list'"), type2char(TYPEOF(rows))); // # nocov
  if (!length(x)) return(x);  // return empty list

  const int nrow = length(VECTOR_ELT(x,0));
  // check index once up front for 0 or NA, for branchless subsetVectorRaw which is repeated for each column
  bool anyNA=false, orderedSubset=true;   // true for when rows==null (meaning all rows)
  if (!isNull(rows) && check_idx(rows, nrow, &anyNA, &orderedSubset)!=NULL) {
    SEXP max = PROTECT(ScalarInteger(nrow)); nprotect++;
    rows = PROTECT(convertNegAndZeroIdx(rows, max, ScalarLogical(TRUE))); nprotect++;
    const char *err = check_idx(rows, nrow, &anyNA, &orderedSubset);
    if (err!=NULL) error(err);
  }

  if (!isInteger(cols)) error(_("Internal error. Argument 'cols' to Csubset is type '%s' not 'integer'"), type2char(TYPEOF(cols))); // # nocov
  for (int i=0; i<LENGTH(cols); i++) {
    int this = INTEGER(cols)[i];
    if (this<1 || this>LENGTH(x)) error(_("Item %d of 'cols' is %d which is outside 1-based range [1,ncol(x)=%d]"), i+1, this, LENGTH(x));
  }

  int overAlloc = checkOverAlloc(GetOption(install("datatable.alloccol"), R_NilValue));
  SEXP ans = PROTECT(allocVector(VECSXP, LENGTH(cols)+overAlloc)); nprotect++;  // doing alloc.col directly here; eventually alloc.col can be deprecated.

  // user-defined and superclass attributes get copied as from v1.12.0
  copyMostAttrib(x, ans);
  // most means all except R_NamesSymbol, R_DimSymbol and R_DimNamesSymbol
  // includes row.names (oddly, given other dims aren't) and "sorted" dealt with below
  // class is also copied here which retains superclass name in class vector as has been the case for many years; e.g. tests 1228.* for #64

  SET_TRUELENGTH(ans, LENGTH(ans));
  SETLENGTH(ans, LENGTH(cols));
  int ansn;
  if (isNull(rows)) {
    ansn = nrow;
    const int *colD = INTEGER(cols);
    for (int i=0; i<LENGTH(cols); i++) {
      SEXP thisCol = VECTOR_ELT(x, colD[i]-1);
      checkCol(thisCol, colD[i], nrow, x);
      SET_VECTOR_ELT(ans, i, copyAsPlain(thisCol));
      // materialize the column subset as we have always done for now, until REFCNT is on by default in R (TODO)
    }
  } else {
    ansn = LENGTH(rows);  // has been checked not to contain zeros or negatives, so this length is the length of result
    const int *colD = INTEGER(cols);
    for (int i=0; i<LENGTH(cols); i++) {
      SEXP source = VECTOR_ELT(x, colD[i]-1);
      checkCol(source, colD[i], nrow, x);
      SEXP target;
      SET_VECTOR_ELT(ans, i, target=allocVector(TYPEOF(source), ansn));
      copyMostAttrib(source, target);
      subsetVectorRaw(target, source, rows, anyNA);  // parallel within column
    }
  }
  SEXP tmp = PROTECT(allocVector(STRSXP, LENGTH(cols)+overAlloc)); nprotect++;
  SET_TRUELENGTH(tmp, LENGTH(tmp));
  SETLENGTH(tmp, LENGTH(cols));
  setAttrib(ans, R_NamesSymbol, tmp);
  subsetVectorRaw(tmp, getAttrib(x, R_NamesSymbol), cols, /*anyNA=*/false);

  tmp = PROTECT(allocVector(INTSXP, 2)); nprotect++;
  INTEGER(tmp)[0] = NA_INTEGER;
  INTEGER(tmp)[1] = -ansn;
  setAttrib(ans, R_RowNamesSymbol, tmp);  // The contents of tmp must be set before being passed to setAttrib(). setAttrib looks at tmp value and copies it in the case of R_RowNamesSymbol. Caused hard to track bug around 28 Sep 2014.

  // clear any index that was copied over by copyMostAttrib() above, e.g. #1760 and #1734 (test 1678)
  setAttrib(ans, sym_index, R_NilValue);
  // but maintain key if ordered subset
  SEXP key = getAttrib(x, sym_sorted);
  if (length(key)) {
    SEXP in = PROTECT(chin(key, getAttrib(ans,R_NamesSymbol))); nprotect++;
    int i = 0;  while(i<LENGTH(key) && LOGICAL(in)[i]) i++;
    // i is now the keylen that can be kept. 2 lines above much easier in C than R
    if (i==0 || !orderedSubset) {
      // clear key that was copied over by copyMostAttrib() above
      setAttrib(ans, sym_sorted, R_NilValue);
    } else {
      // make a new key attribute; shorter if i<LENGTH(key) or same length copied so this key is safe to change by ref (setnames)
      setAttrib(ans, sym_sorted, tmp=allocVector(STRSXP, i));
      for (int j=0; j<i; j++) SET_STRING_ELT(tmp, j, STRING_ELT(key, j));
    }
  }
  unlock(ans);
  setselfref(ans);
  UNPROTECT(nprotect);
  return ans;
}

SEXP subsetVector(SEXP x, SEXP idx) { // idx is 1-based passed from R level
  bool anyNA=false, orderedSubset=false;
  int nprotect=0;
  if (isNull(x))
    error(_("Internal error: NULL can not be subset. It is invalid for a data.table to contain a NULL column."));      // # nocov
  if (check_idx(idx, length(x), &anyNA, &orderedSubset) != NULL)
    error(_("Internal error: CsubsetVector is internal-use-only but has received negatives, zeros or out-of-range"));  // # nocov
  SEXP ans = PROTECT(allocVector(TYPEOF(x), length(idx))); nprotect++;
  copyMostAttrib(x, ans);
  subsetVectorRaw(ans, x, idx, anyNA);
  UNPROTECT(nprotect);
  return ans;
}

