#include "data.table.h"
#include <Rdefines.h>
#ifdef _OPENMP
   #include <omp.h>
#endif

static union {
    double d;
    long long ll;
} naval;

static SEXP subsetVectorRaw(SEXP x, SEXP idx, int l, int tl)
// Only for use by subsetDT() or subsetVector() below, hence static
// l is the count of non-zero (including NAs) in idx i.e. the length of the result
// tl is the amount to be allocated,  tl>=l
// TO DO: if no 0 or NA detected up front in subsetDT() below, could switch to a faster subsetVectorRawNo0orNA()
{
    int i, this, ansi=0, max=length(x), n=LENGTH(idx), *pidx=INTEGER(idx);
    if (tl<l) error("Internal error: tl<n passed to subsetVectorRaw");
    SEXP class = getAttrib(x, R_ClassSymbol), ans = PROTECT(allocVector(TYPEOF(x), tl));
    Rboolean i64 = isString(class) && STRING_ELT(class, 0) == char_integer64;
    if (i64) naval.ll = NAINT64; else naval.d = NA_REAL;
    SETLENGTH(ans, l);
    SET_TRUELENGTH(ans, tl);
    // Rprintf("l=%d, tl=%d, LENGTH(idx)=%d\n", l, tl, LENGTH(idx));
#ifdef _OPENMP
    int *ctr = (int *)calloc(omp_get_max_threads()+1, sizeof(int));
#endif

    switch(TYPEOF(x)) {
    case INTSXP :
#ifdef _OPENMP
    #pragma omp parallel
    {
        int tmp=0, ithread = omp_get_thread_num(), nthreads = omp_get_num_threads();    // local
        // computing count indices correctly is tricky when there are 0-indices.
        // 1. count number of non-0 'idx' for each thread
        #pragma omp for
        for (i=0; i<n; i++) tmp += (pidx[i] != 0);  // don't use ctr[ithread+1] here -- false sharing
        ctr[ithread+1] = tmp;                       // ctr[0]=0, rest contains count where iidx!=0,
                                                    // within each thread's range
        #pragma omp barrier                         // wait for all threads, important
        // 2. using that, set the starting index for each thread appropriately
        #pragma omp single
        for (i=0; i<nthreads; i++)
            ctr[i+1] += ctr[i];                     // for each thread, compute the right starting point, by
                                                    // taking (non)0-count into account, computed above.
        tmp = ctr[ithread];                         // copy back from shared to thread's local var. All set.
        #pragma omp barrier                         // wait for all threads, important
        // 3. use old code, but with thread's local var with right start index as counter
        #pragma omp for private(this) reduction(+:ansi)
        for (i=0; i<n; i++) {
            this = pidx[i];
            if (this==0) continue;
            // have to use 'tmp' here, and not ctr[ithread++] -- false sharing
            INTEGER(ans)[tmp++] = (this==NA_INTEGER || this>max) ? NA_INTEGER : INTEGER(x)[this-1];
            ansi++;                                 // not required, but just to be sure
        }
    }
#else
    for (i=0; i<n; i++) {
        this = pidx[i];
        if (this==0) continue;
        INTEGER(ans)[ansi++] = (this==NA_INTEGER || this>max) ? NA_INTEGER : INTEGER(x)[this-1];
    }
#endif
    break;
    case REALSXP :
#ifdef _OPENMP
    #pragma omp parallel
    {
        int tmp=0, ithread = omp_get_thread_num(), nthreads = omp_get_num_threads();
        #pragma omp for
        for (i=0; i<n; i++) tmp += (pidx[i] != 0);
        ctr[ithread+1] = tmp;
        #pragma omp barrier
        #pragma omp single
        for (i=0; i<nthreads; i++)
            ctr[i+1] += ctr[i];
        tmp = ctr[ithread];
        #pragma omp barrier
        #pragma omp for private(this) reduction(+:ansi)
        for (i=0; i<n; i++) {
            this = pidx[i];
            if (this==0) continue;
            REAL(ans)[tmp++] = (this==NA_INTEGER || this>max) ? naval.d : REAL(x)[this-1];
            ansi++;
        }
    }
#else
    for (i=0; i<n; i++) {
        this = pidx[i];
        if (this==0) continue;
        REAL(ans)[ans++] = (this==NA_INTEGER || this>max) ? naval.d : REAL(x)[this-1];
    }
#endif
    break;
    case LGLSXP :
#ifdef _OPENMP
    #pragma omp parallel
    {
        int tmp=0, ithread = omp_get_thread_num(), nthreads = omp_get_num_threads();
        #pragma omp for
        for (i=0; i<n; i++) tmp += (pidx[i] != 0);
        ctr[ithread+1] = tmp;
        #pragma omp barrier
        #pragma omp single
        for (i=0; i<nthreads; i++)
            ctr[i+1] += ctr[i];
        tmp = ctr[ithread];
        #pragma omp barrier
        #pragma omp for private(this) reduction(+:ansi)
        for (i=0; i<n; i++) {
            this = pidx[i];
            if (this==0) continue;
            LOGICAL(ans)[tmp++] = (this==NA_INTEGER || this>max) ? NA_LOGICAL : LOGICAL(x)[this-1];
            ansi++;
        }
    }
#else
    for (i=0; i<n; i++) {
        this = pidx[i];
        if (this==0) continue;
        LOGICAL(ans)[ansi++] = (this==NA_INTEGER || this>max) ? NA_LOGICAL : LOGICAL(x)[this-1];
    }
#endif
    break;
    case STRSXP :
    for (i=0; i<n; i++) {
        this = pidx[i];
        if (this==0) continue;
        SET_STRING_ELT(ans, ansi++, (this==NA_INTEGER || this>max) ? NA_STRING : STRING_ELT(x, this-1));
    }
    break;
    case VECSXP :
    for (i=0; i<n; i++) {
        this = pidx[i];
        if (this==0) continue;
        SET_VECTOR_ELT(ans, ansi++, (this==NA_INTEGER || this>max) ? R_NilValue : VECTOR_ELT(x, this-1));
    }
    break;
    // Fix for #982
    // source: https://github.com/wch/r-source/blob/fbf5cdf29d923395b537a9893f46af1aa75e38f3/src/main/subset.c
    case CPLXSXP :
#ifdef _OPENMP
    #pragma omp parallel
    {
        int tmp=0, ithread = omp_get_thread_num(), nthreads = omp_get_num_threads();
        #pragma omp for
        for (i=0; i<n; i++) tmp += (pidx[i] != 0);
        ctr[ithread+1] = tmp;
        #pragma omp barrier
        #pragma omp single
        for (i=0; i<nthreads; i++)
            ctr[i+1] += ctr[i];
        tmp = ctr[ithread];
        #pragma omp barrier
        #pragma omp for private(this) reduction(+:ansi)
        for (i=0; i<n; i++) {
            this = pidx[i];
            if (this==0) continue;
            if (this == NA_INTEGER || this>max) {
                COMPLEX(ans)[tmp].r = NA_REAL;
                COMPLEX(ans)[tmp++].i = NA_REAL;
            } else COMPLEX(ans)[tmp++] = COMPLEX(x)[this-1];
            ansi++;
        }
    }
#else
    for (i=0; i<n; i++) {
        this = pidx[i];
        if (this == 0) continue;
        if (this == NA_INTEGER || this>max) {
            COMPLEX(ans)[ansi].r = NA_REAL;
            COMPLEX(ans)[ansi].i = NA_REAL;
        } else COMPLEX(ans)[ansi] = COMPLEX(x)[this-1];
        ansi++;
    }
#endif
    break;
    case RAWSXP :
#ifdef _OPENMP
    #pragma omp parallel
    {
        int tmp=0, ithread = omp_get_thread_num(), nthreads = omp_get_num_threads();
        #pragma omp for
        for (i=0; i<n; i++) tmp += (pidx[i] != 0);
        ctr[ithread+1] = tmp;
        #pragma omp barrier
        #pragma omp single
        for (i=0; i<nthreads; i++)
            ctr[i+1] += ctr[i];
        tmp = ctr[ithread];
        #pragma omp barrier
        #pragma omp for private(this) reduction(+:ansi)
        for (i=0; i<n; i++) {
            this = pidx[i];
            if (this==0) continue;
            RAW(ans)[tmp++] = (this == NA_INTEGER || this>max) ? (Rbyte) 0 : RAW(x)[this-1];
            ansi++;
        }
    }
#else
    for (i=0; i<n; i++) {
        this = pidx[i];
        if (this == 0) continue;
        RAW(ans)[ansi++] = (this == NA_INTEGER || this>max) ? (Rbyte) 0 : RAW(x)[this-1];
    }
#endif
    break;
    default :
    error("Unknown column type '%s'", type2char(TYPEOF(x)));
    }
#ifdef _OPENMP
    free(ctr);
#endif
    if (ansi != l) error("Internal error: ansi [%d] != l [%d] at the end of subsetVector", ansi, l);
    copyMostAttrib(x, ans);
    UNPROTECT(1);
    return(ans);
}

static int check_idx(SEXP idx, int n)
{
    int i, this, ans=0;
    if (!isInteger(idx)) error("Internal error. 'idx' is type '%s' not 'integer'", type2char(TYPEOF(idx)));
    for (i=0; i<LENGTH(idx); i++) {  // check idx once up front and count the non-0 so we know how long the answer will be
    this = INTEGER(idx)[i];
    if (this==0) continue;
    if (this!=NA_INTEGER && this<0) error("Internal error: item %d of idx is %d. Negatives should have been dealt with earlier.", i+1, this);
    // this>n is treated as NA for consistency with [.data.frame and things like cbind(DT[w],DT[w+1])
    ans++;
    }
    return ans;
}

SEXP convertNegativeIdx(SEXP idx, SEXP maxArg)
{
    int this;
    // + more precise and helpful error messages telling user exactly where the problem is (saving user debugging time)
    // + a little more efficient than negativeSubscript in src/main/subscript.c (it's private to R so we can't call it anyway)

    if (!isInteger(idx)) error("Internal error. 'idx' is type '%s' not 'integer'", type2char(TYPEOF(idx)));
    if (!isInteger(maxArg) || length(maxArg)!=1) error("Internal error. 'maxArg' is type '%s' and length %d, should be an integer singleton", type2char(TYPEOF(maxArg)), length(maxArg));
    int max = INTEGER(maxArg)[0];
    if (max<0) error("Internal error. max is %d, must be >= 0.", max);  // NA also an error which'll print as INT_MIN
    int firstNegative = 0, firstPositive = 0, firstNA = 0, num0 = 0;
    int i=0;
    for (i=0; i<LENGTH(idx); i++) {
    this = INTEGER(idx)[i];
    if (this==NA_INTEGER) { if (firstNA==0) firstNA = i+1;  continue; }
    if (this==0)          { num0++;  continue; }
    if (this>0)           { if (firstPositive==0) firstPositive=i+1; continue; }
    if (firstNegative==0) firstNegative=i+1;
    }
    if (firstNegative==0) return(idx);  // 0's and NA can be mixed with positives, there are no negatives present, so we're done
    if (firstPositive) error("Item %d of i is %d and item %d is %d. Cannot mix positives and negatives.",
                 firstNegative, INTEGER(idx)[firstNegative-1], firstPositive, INTEGER(idx)[firstPositive-1]);
    if (firstNA)       error("Item %d of i is %d and item %d is NA. Cannot mix negatives and NA.",
                 firstNegative, INTEGER(idx)[firstNegative-1], firstNA);

    // idx is all negative without any NA but perhaps 0 present (num0) ...

    char *tmp = Calloc(max, char);    // 4 times less memory that INTSXP in src/main/subscript.c
    int firstDup = 0, numDup = 0, firstBeyond = 0, numBeyond = 0;
    for (i=0; i<LENGTH(idx); i++) {
    this = -INTEGER(idx)[i];
    if (this==0) continue;
    if (this>max) {
        numBeyond++;
        if (firstBeyond==0) firstBeyond=i+1;
        continue;
    }
    if (tmp[this-1]==1) {
        numDup++;
        if (firstDup==0) firstDup=i+1;
    } else tmp[this-1] = 1;
    }
    if (numBeyond)
    warning("Item %d of i is %d but there are only %d rows. Ignoring this and %d more like it out of %d.", firstBeyond, INTEGER(idx)[firstBeyond-1], max, numBeyond-1, LENGTH(idx));
    if (numDup)
    warning("Item %d of i is %d which has occurred before. Ignoring this and %d other duplicates out of %d.", firstDup, INTEGER(idx)[firstDup-1], numDup-1, LENGTH(idx));

    SEXP ans = PROTECT(allocVector(INTSXP, max-LENGTH(idx)+num0+numDup+numBeyond));
    int ansi = 0;
    for (i=0; i<max; i++) {
    if (tmp[i]==0) INTEGER(ans)[ansi++] = i+1;
    }
    Free(tmp);
    UNPROTECT(1);
    if (ansi != max-LENGTH(idx)+num0+numDup+numBeyond) error("Internal error: ansi[%d] != max[%d]-LENGTH(idx)[%d]+num0[%d]+numDup[%d]+numBeyond[%d] in convertNegativeIdx",ansi,max,LENGTH(idx),num0,numDup,numBeyond);
    return(ans);
}

SEXP subsetDT(SEXP x, SEXP rows, SEXP cols) { // rows and cols are 1-based passed from R level

// Originally for subsetting vectors in fcast and now the beginnings of [.data.table ported to C
// Immediate need is for R 3.1 as lglVec[1] now returns R's global TRUE and we don't want := to change that global [think 1 row data.tables]
// Could do it other ways but may as well go to C now as we were going to do that anyway

    SEXP ans, tmp;
    R_len_t i, j, ansn=0;
    int this;
    if (!isNewList(x)) error("Internal error. Argument 'x' to CsubsetDT is type '%s' not 'list'", type2char(TYPEOF(rows)));
    if (!length(x)) return(x);  // return empty list
    ansn = check_idx(rows, length(VECTOR_ELT(x,0)));  // check once up front before looping calls to subsetVectorRaw below
    if (!isInteger(cols)) error("Internal error. Argument 'cols' to Csubset is type '%s' not 'integer'", type2char(TYPEOF(cols)));
    for (i=0; i<LENGTH(cols); i++) {
    this = INTEGER(cols)[i];
    if (this<1 || this>LENGTH(x)) error("Item %d of 'cols' is %d which is outside 1-based range [1,ncol(x)=%d]", i+1, this, LENGTH(x));
    }
    ans = PROTECT(allocVector(VECSXP, LENGTH(cols)+64));  // just do alloc.col directly, eventually alloc.col can be deprecated.
    copyMostAttrib(x, ans);  // other than R_NamesSymbol, R_DimSymbol and R_DimNamesSymbol
                 // so includes row.names (oddly, given other dims aren't) and "sorted", dealt with below
    SET_TRUELENGTH(ans, LENGTH(ans));
    SETLENGTH(ans, LENGTH(cols));
    for (i=0; i<LENGTH(cols); i++) {
    SET_VECTOR_ELT(ans, i, subsetVectorRaw(VECTOR_ELT(x, INTEGER(cols)[i]-1), rows, ansn, ansn));  // column vectors aren't over allocated yet
    }
    setAttrib(ans, R_NamesSymbol, subsetVectorRaw( getAttrib(x, R_NamesSymbol), cols, LENGTH(cols), LENGTH(cols)+64 ));
    tmp = PROTECT(allocVector(INTSXP, 2));
    INTEGER(tmp)[0] = NA_INTEGER;
    INTEGER(tmp)[1] = -ansn;
    setAttrib(ans, R_RowNamesSymbol, tmp);  // The contents of tmp must be set before being passed to setAttrib(). setAttrib looks at tmp value and copies it in the case of R_RowNamesSymbol. Caused hard to track bug around 28 Sep 2014.
    // maintain key if ordered subset ...
    SEXP key = getAttrib(x, install("sorted"));
    if (length(key)) {
    SEXP in = PROTECT(chmatch(key,getAttrib(ans,R_NamesSymbol), 0, TRUE)); // (nomatch ignored when in=TRUE)
    i = 0;  while(i<LENGTH(key) && LOGICAL(in)[i]) i++;
    UNPROTECT(1);
    // i is now the keylen that can be kept. 2 lines above much easier in C than R
    if (i==0) {
        setAttrib(ans, install("sorted"), R_NilValue);
        // clear key that was copied over by copyMostAttrib() above
    } else if (isOrderedSubset(rows, ScalarInteger(length(VECTOR_ELT(x,0))))) {
        setAttrib(ans, install("sorted"), tmp=allocVector(STRSXP, i));
        for (j=0; j<i; j++) SET_STRING_ELT(tmp, j, STRING_ELT(key, j));
    }
    }
    setAttrib(ans, install(".data.table.locked"), R_NilValue);
    setselfref(ans);
    UNPROTECT(2);
    return ans;
}

SEXP subsetVector(SEXP x, SEXP idx) { // idx is 1-based passed from R level
    int n = check_idx(idx, length(x));
    return subsetVectorRaw(x, idx, n, n);
}
