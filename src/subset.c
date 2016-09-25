#include "data.table.h"
#include <Rdefines.h>
#ifdef _OPENMP
   #include <omp.h>
#endif

/*
* TODO: test. Taking a subset of fewer rows just goes single threaded.
* TODO: going parallel or not should also depend on the number of columns
* TODO: within column is good, but go parallel across columns, too
*/
#define P_THRESH 1000

static union {
    double d;
    long long ll;
} naval;

/*
* Previously subsetVectorRaw contained 
*   1) separate code for parallel and non-parallel versions which can become 
*       to maintain, and
*   2) computed the indices to schedule for each thread *for every column*!! 
*       That is just inefficient. 
*
* Therefore, I've moved the logic for computing the row indices start for 
* each thread to 'chk_idx()'. These globals are necessary for that. '*ctr'
* contains the start row index that each thread will start with, and 'nthr' 
* contains the number of threads (based on our <new> condition).
* 
* TODO: verify if the number of threads when set will definitely be available 
* completely within the function call. That is, 'chk_idx()' is called from 
* 'subsetDT()'. Let's say at this time we've figured out 'nthr=4'. Is it 100% 
* certain that the same 4 threads will be available for each call to 
* 'subsetVectorRaw()'? I think so, but not sure.. If so, this logic *might* 
* create problems and we might have to revert to old logic.
*/
static int *ctr, nthr=1;

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

    switch(TYPEOF(x)) {
    case INTSXP :
    #pragma omp parallel num_threads(nthr)
    {
        int tmp=0, ithread = omp_get_thread_num();
        tmp = ctr[ithread];
        #pragma omp barrier
        #pragma omp for private(this) reduction(+:ansi)
        for (i=0; i<n; i++) {
            this = pidx[i];
            if (this==0) continue;
            // have to use 'tmp' here, and not ctr[ithread++] -- false sharing
            INTEGER(ans)[tmp++] = (this==NA_INTEGER || this>max) ? NA_INTEGER : INTEGER(x)[this-1];
            ansi++; // not required, but just to be sure
        }
    }
    break;
    case REALSXP :
    #pragma omp parallel num_threads(nthr)
    {
        int tmp=0, ithread = omp_get_thread_num();
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
    break;
    case LGLSXP :
    #pragma omp parallel num_threads(nthr)
    {
        int tmp=0, ithread = omp_get_thread_num();
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
    #pragma omp parallel num_threads(nthr)
    {
        int tmp=0, ithread = omp_get_thread_num();
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
    break;
    case RAWSXP :
    #pragma omp parallel num_threads(nthr)
    {
        int tmp=0, ithread = omp_get_thread_num();
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
    break;
    default :
    free(ctr);
    error("Unknown column type '%s'", type2char(TYPEOF(x)));
    }
    if (ansi != l) {
        free(ctr);
        error("Internal error: ansi [%d] != l [%d] at the end of \
                subsetVector", ansi, l);
    }
    copyMostAttrib(x, ans);
    UNPROTECT(1);
    return(ans);
}

/* 
* chk_idx - checks row index for invalid values, counts the total number of 
* non-zero row indices provided (which will be the length of final answer) and 
* schedules the row start indices each thread has to start from (to parallelise).
* 
* The parallel logic: Since we might have some 0-indices which needs to be 
* skipped, the logic is slightly more complicated.
*
*   1) Identify the total number of non-zero indices for each thread (by 
*       scheduling in static manner).
*   2) Store those results in 'ctr' from 1-index. ctr[0]=0 which is always the 
*       start index for the first thread. 
*   3) Cumulative sum 'ctr' so as to get the set the start row index for each 
*       thread. We'll need a 'barrier' and 'single' just before this to ensure 
*       proper counts being assigned to 'ctr' and to cumulative sum only once.
*   4) Now we've the right start index in 'ctr'. Now we've to assign the 
*       count value to each thread using a local variable 'tmp'.
*   5) At this point we're all set. Loop through and assign subset to 'ans'. In 
*       addition, also use a check variable to ensure we're assigning as many 
*       rows as we are supposed to, with 'ansi'.
* 
* Return total number of rows (as it was in the older code)
*/
static int check_idx(SEXP idx, int n) {
    int i, nidx=length(idx), isError=0, *pidx;
    if (!isInteger(idx)) {
        error("Internal error. 'idx' is type '%s' not 'integer'", 
                type2char(TYPEOF(idx)));
    }
    pidx = INTEGER(idx);
    // ctr[0]=0, rest contains count of idx!=0 for each thread
    nthr = nidx<P_THRESH ? 1 : getDTthreads();
    ctr = (int *)calloc(nthr+1, sizeof(int));
    #pragma omp parallel num_threads(nthr)
    {
        // local vars
        int tmp=0, ithread=omp_get_thread_num();

        #pragma omp for reduction(+:isError)
        for (i=0; i<nidx; i++) {
            tmp += (pidx[i] != 0);
            if (pidx[i]!=NA_INTEGER && pidx[i]<0) {
                isError++;
            }
        }
        ctr[ithread+1] = tmp;   // implicit barrier after for
        #pragma omp barrier     // IMPORTANT: wait for threads, explicit barrier
        #pragma omp single
        for (i=0; i<nthr; i++) {
            ctr[i+1] += ctr[i];
        }
        if (isError) {
            free(ctr);
            error("Internal error: one or more item(s) in idx is < 0. \
                    Negatives should have been dealt with earlier.");
            // this>n is treated as NA for consistency with [.data.frame and 
            // things like cbind(DT[w],DT[w+1])
        }
    }
    return ctr[nthr];
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

/*
* subsetDT - Subsets a data.table
* NOTE:
*   1) 'rows' and 'cols' are 1-based, passed from R level
*   2) Originally for subsetting vectors in fcast and now the beginnings of 
*       [.data.table ported to C
*   3) Immediate need is for R 3.1 as lglVec[1] now returns R's global TRUE 
*       and we don't want := to change that global [think 1 row data.tables]
*   4) Could do it other ways but may as well go to C now as we were going to 
*       do that anyway
*/
SEXP subsetDT(SEXP x, SEXP rows, SEXP cols) {
    SEXP ans, tmp, key, xnames;
    R_len_t i, j, this, nx=length(VECTOR_ELT(x, 0));
    R_len_t nans=0, ncols=0;
    if (!isNewList(x)) {
        error("Internal error. Argument 'x' to CsubsetDT is type '%s' \
                not 'list'", type2char(TYPEOF(rows)));
    }
    if (!length(x)) return(x);  // return empty list
    if (!isInteger(cols)) {
        error("Internal error. Argument 'cols' to Csubset is type '%s' \
            not 'integer'", type2char(TYPEOF(cols)));
    }
    ncols = LENGTH(cols);
    for (i=0; i<ncols; i++) {
        this = INTEGER(cols)[i];
        if (this<1 || this>LENGTH(x)) {
            error("Item %d of 'cols' is %d which is outside 1-based range \
                [1,ncol(x)=%d]", i+1, this, LENGTH(x));
        }
    }
    // just do alloc.col directly, eventually alloc.col can be deprecated.
    ans = PROTECT(allocVector(VECSXP, ncols+64));
    copyMostAttrib(x, ans); // other than R_NamesSymbol, R_DimSymbol and 
    // R_DimNamesSymbol so includes row.names (oddly, given other dims aren't) 
    // and "sorted", dealt with below
    SET_TRUELENGTH(ans, LENGTH(ans));
    SETLENGTH(ans, ncols);

    nthr = 1; // make sure nthr is reset (static global declared above)
    // check_idx sets the right value of nthr
    nans = check_idx(rows, nx); // check once up front before looping calls to subsetVectorRaw below
    for (i=0; i<ncols; i++) {
        // NOTE: column vectors aren't over allocated yet
        tmp = VECTOR_ELT(x, INTEGER(cols)[i]-1);
        SET_VECTOR_ELT(ans, i, subsetVectorRaw(tmp, rows, nans, nans));
    }
    // NOTE: this part will *never* run in parallel as it is a STRSXP. 
    // If we manage to parallelise STRSXP in the future, we'll either need to 
    // recompute check_idx or set nthr=1 here.
    xnames = getAttrib(x, R_NamesSymbol);
    setAttrib(ans, R_NamesSymbol, subsetVectorRaw(xnames, cols, ncols, ncols+64));
    tmp = PROTECT(allocVector(INTSXP, 2));
    INTEGER(tmp)[0] = NA_INTEGER;
    INTEGER(tmp)[1] = -nans;
    setAttrib(ans, R_RowNamesSymbol, tmp);  // The contents of tmp must be set 
    // before being passed to setAttrib(). setAttrib looks at tmp value and 
    // copies it in the case of R_RowNamesSymbol. Caused hard to track bug 
    // around 28 Sep 2014.

    // maintain key if ordered subset ...
    key = getAttrib(x, install("sorted"));
    if (length(key)) {
        SEXP in = PROTECT(chmatch(key, getAttrib(ans, R_NamesSymbol), 0, TRUE)); // nomatch ignored when in=TRUE
        i = 0;
        while(i<LENGTH(key) && LOGICAL(in)[i]) i++;
        UNPROTECT(1);
        // i is now the keylen that can be kept. 2 lines above much easier in C than R
        if (i==0) {
            // clear key that was copied over by copyMostAttrib() above
            setAttrib(ans, install("sorted"), R_NilValue);
        } else if (isOrderedSubset(rows, ScalarInteger(nx))) {
            tmp = allocVector(STRSXP, i);
            setAttrib(ans, install("sorted"), tmp);
            for (j=0; j<i; j++) {
                SET_STRING_ELT(tmp, j, STRING_ELT(key, j));
            }
        }
    }
    setAttrib(ans, install(".data.table.locked"), R_NilValue);
    setselfref(ans);
    UNPROTECT(2);
    free(ctr);
    return ans;
}

SEXP subsetVector(SEXP x, SEXP idx) { // idx is 1-based passed from R level
    nthr = 1; // make sure nthr is reset (static global declared above)
    int n = check_idx(idx, length(x));
    SEXP ans = subsetVectorRaw(x, idx, n, n);
    free(ctr);
    return ans;
}
