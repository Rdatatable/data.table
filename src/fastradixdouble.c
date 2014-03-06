#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee

// Tested on:
// R-3.0.2 osx 10.8.5 64-bit gcc 4.2.1, 
// R-2.15.2 debian 64-bit gcc-4.7.2, 
// R-2.15.3 osx 10.8.5 32-bit gcc-4.2.1, 
// R-2.15.3 osx 10.8.5 64-bit gcc-4.2.1

// for tolerance
extern SEXP fastradixint(SEXP vec, SEXP return_index);

// adapted from Michael Herf's code - http://stereopsis.com/radix.html
// TO IMPLEMENT (probably) - R's long vector support (R_xlen_t)
// 1) logical argument 'return_index' = TRUE returns 'order' (indices) and FALSE returns sorted value directly instead of indices
// 2) also allows ordering/sorting with 'tolerance' (another pass through length of input vector will happen + multiple integer radix sort calls). 
// The performance will depend on the number of groups that have to be sorted because under given tolerance they become identical. In theory, 
// most of the times the last pass shouldn't affect the performance at all.
// the "decreasing=" feature has been removed. Use 'setrev' instead to get the reverse order if required (small performance improvement by doing this)

// Hack for 32-bit and 64-bit versions of R (or architectures):
// ------------------------------------------------------------
// data.table requires that NA and NaN be sorted before all other numbers and that NA is before NaN:
// 1) 'unsigned long' in 64-bit is 64-bit, but 32-bit in 32-bit. Therefore we've to use 'unsigned long long' which seems 64-bit in both 32-and 64-bit
// 2) Inf=0x7ff0000000000000, -Inf=0xfff0000000000000, NA=0x7ff80000000007a2 and NaN=0x7ff8000000000000 in 32-bit unsigned long long
// 3) Inf=0x7ff0000000000000, -Inf=0xfff0000000000000, NA=0x7ff00000000007a2 and NaN=0x7ff8000000000000 in 64-bit unsigned long long
// Therefore in 32-bit, NA gets sorted before NaN and in 64-bit, NaN gets sorted before NA in 64 bit.
// 4) also, sometimes NaN seems to be 0xfff8000000000000 instead of 0x7ff8000000000000.
// As of now the solution is to 'set' the first 16 bits to 7ff8 if NA/NaN for both 32/64-bit
// so that all these differences are nullified and then the usual checks are done.
// At the end, we need NA to be 0x7ff80000000007a2 and NaN to be 0x7ff8000000000000 and that's what we'll make sure of.

// HACK for NA/NaN sort before all the other numbers:
// --------------------------------------------------
// Once we set this to be NA/NaN, to make sure that NA/NaN gets sorted before 
// ALL the other numbers, we just flip the sign bit so that, 
// NA and NaN become 0xfff80000000007a2 and 0xfff8000000000000
// NA now is smaller than NaN and will be sorted before any other number (including -Inf)

#define FLIP_SIGN_BIT   0x8000000000000000
#define RESET_NA_NAN    0x0000ffffffffffff
#define SET_NA_NAN_COMP 0xfff8000000000000 // we can directly set complement in hack_na_nan

unsigned long long flip_double(unsigned long long f) {
    unsigned long long mask = -(long long)(f >> 63) | FLIP_SIGN_BIT;
    return f ^ mask;
}

void flip_double_ref(unsigned long long *f) {
    unsigned long long mask = -(long long)(*f >> 63) | FLIP_SIGN_BIT;
    *f ^= mask;
}

unsigned long long invert_flip_double(unsigned long long f) {
    unsigned long long mask = ((f >> 63) - 1) | FLIP_SIGN_BIT;
    return f ^ mask;
}

void hack_na_nan(unsigned long long *f) {
    // same as flip_double_ref but with bit twiddling first for NA/NaN
    // removed mask setting as sign bit is always 0, so we just flip it
    // since we've to flip the sign bit, why not directly set it in #define
    *f &= RESET_NA_NAN;
    *f |= SET_NA_NAN_COMP;
}

// utils for accessing 11-bit quantities
#define _0(x) (x & 0x7FF)
#define _1(x) (x >> 11 & 0x7FF)
#define _2(x) (x >> 22 & 0x7FF)
#define _3(x) (x >> 33 & 0x7FF)
#define _4(x) (x >> 44 & 0x7FF)
#define _5(x) (x >> 55)

#define STACK_HIST 2048

// x should be of type numeric
SEXP fastradixdouble(SEXP x, SEXP tol, SEXP return_index) {
    int i;
    unsigned long long pos, fi, si, n;
    unsigned long long sum0 = 0, sum1 = 0, sum2 = 0, sum3 = 0, sum4 = 0, sum5 = 0, tsum;    
    SEXP xtmp, order, ordertmp;
    
    n = length(x);
    if (!isReal(x) || n <= 0) error("List argument to 'fastradixdouble' must be non-empty and of type 'numeric'");
    if (TYPEOF(return_index) != LGLSXP || length(return_index) != 1) error("Argument 'return_index' to 'fastradixdouble' must be logical TRUE/FALSE");
    if (TYPEOF(tol) != REALSXP) error("Argument 'tol' to 'fastradixdouble' must be a numeric vector of length 1");


    xtmp  = PROTECT(allocVector(REALSXP, n));
    ordertmp = PROTECT(allocVector(INTSXP, n));
    order = PROTECT(allocVector(INTSXP, n));
    
    unsigned long long *array = (unsigned long long*)REAL(x);
    unsigned long long *sort = (unsigned long long*)REAL(xtmp);
            
    // 6 histograms on the stack:
    unsigned long long b0[STACK_HIST * 6];
    unsigned long long *b1 = b0 + STACK_HIST;
    unsigned long long *b2 = b1 + STACK_HIST;
    unsigned long long *b3 = b2 + STACK_HIST;
    unsigned long long *b4 = b3 + STACK_HIST;
    unsigned long long *b5 = b4 + STACK_HIST;

    // definitely faster on big data than a for-loop
    memset(b0, 0, STACK_HIST*6*sizeof(unsigned long long));

    // Step 1:  parallel histogramming pass
    for (i=0;i<n;i++) {
        // flip NaN/NA sign bit so that they get sorted in the front (special for data.table)
        if (ISNAN(REAL(x)[i])) hack_na_nan(&array[i]);
        fi = flip_double((unsigned long long)array[i]);
        b0[_0(fi)]++;
        b1[_1(fi)]++;
        b2[_2(fi)]++;
        b3[_3(fi)]++;
        b4[_4(fi)]++;
        b5[_5(fi)]++;
    }
    
    // Step 2:  Sum the histograms -- each histogram entry records the number of values preceding itself.
    for (i=0;i<STACK_HIST;i++) {

        tsum = b0[i] + sum0;
        b0[i] = sum0 - 1;
        sum0 = tsum;

        tsum = b1[i] + sum1;
        b1[i] = sum1 - 1;
        sum1 = tsum;

        tsum = b2[i] + sum2;
        b2[i] = sum2 - 1;
        sum2 = tsum;

        tsum = b3[i] + sum3;
        b3[i] = sum3 - 1;
        sum3 = tsum;

        tsum = b4[i] + sum4;
        b4[i] = sum4 - 1;
        sum4 = tsum;

        tsum = b5[i] + sum5;
        b5[i] = sum5 - 1;
        sum5 = tsum;
    }

    for (i=0;i<n;i++) {
        fi = array[i];
        flip_double_ref(&fi);
        pos = _0(fi);
        sort[++b0[pos]] = fi;
        INTEGER(ordertmp)[b0[pos]] = i;
    }

    for (i=0;i<n;i++) {
        si = sort[i];
        pos = _1(si);
        array[++b1[pos]] = si;
        INTEGER(order)[b1[pos]] = INTEGER(ordertmp)[i];
    }

    for (i=0;i<n;i++) {
        fi = array[i];
        pos = _2(fi);
        sort[++b2[pos]] = fi;
        INTEGER(ordertmp)[b2[pos]] = INTEGER(order)[i];
    }

    for (i=0;i<n;i++) {
        si = sort[i];
        pos = _3(si);
        array[++b3[pos]] = si;
        INTEGER(order)[b3[pos]] = INTEGER(ordertmp)[i];
    }

    for (i=0;i<n;i++) {
        fi = array[i];
        pos = _4(fi);
        sort[++b4[pos]] = fi;
        INTEGER(ordertmp)[b4[pos]] = INTEGER(order)[i];
    }

    for (i=0;i<n;i++) {
        si = sort[i];
        pos = _5(si);
        array[++b5[pos]] = invert_flip_double(si);
        INTEGER(order)[b5[pos]] = INTEGER(ordertmp)[i]+1;
    }

    // NOTE: that the result won't be 'exactly' identical to ordernumtol if there are too many values that are 'very close' to each other.
    // However, I believe this is the correct version in those cases. Why? if you've 3 numbers that are sorted with no tolerance, and now under tolerance
    // these numbers are equal, then all 3 indices *must* be sorted. That is ther right order. But in some close cases, 'ordernumtol' doesn't do this.
    // To test, do: x <- rnorm(1e6); and run fastradixdouble and ordernumtol with same tolerance and compare results.

    // check for tolerance and reorder wherever necessary
    if (length(tol) > 0) {
        i=1;
        int j, start=0, end=0;
        SEXP st,dst,rt,sq,ridx;
        PROTECT(ridx = allocVector(LGLSXP, 1));
        LOGICAL(ridx)[0] = TRUE;
        while(i<n) {
            if (!R_FINITE(REAL(x)[i]) || !R_FINITE(REAL(x)[i-1])) { i++; continue; }
            // hack to skip checking Inf=Inf, -Inf=-Inf, NA=NA and NaN=NaN... using unsigned int
            if (REAL(x)[i]-REAL(x)[i-1] > REAL(tol)[0] || array[i] == array[i-1]) { i++; continue; }
            start = i-1;
            i++;
            while(i < n && REAL(x)[i] - REAL(x)[i-1] < REAL(tol)[0]) { i++; }
            end = i-1;
            i++;
            if (end-start+1 == 1) continue;
            PROTECT(st = allocVector(INTSXP, end-start+1));
            PROTECT(sq = allocVector(REALSXP, end-start+1));
            // To investigate: probably a simple bubble sort or shell sort may be quicker on groups with < 10 items than a 3-pass radix?
            // Can't rely on R's base radix order because even if you've two items in group and one of them is 3 and the other is 1e6, then it won't work!
            // Just doing this gives 4x speed-up under small group sizes. (from 37 to 6-9 seconds)
            if (end-start+1 == 2) {
                // avoid radix sort on 2 items
                if (INTEGER(order)[start] > INTEGER(order)[end]) {
                    // then just swap
                    INTEGER(st)[0] = INTEGER(order)[start];
                    INTEGER(order)[start] = INTEGER(order)[end];
                    INTEGER(order)[end] = INTEGER(st)[0];
            
                    REAL(sq)[0] = REAL(x)[start];
                    REAL(x)[start] = REAL(x)[end];
                    REAL(x)[end] = REAL(sq)[0];
                }
                UNPROTECT(2); // st, sq
                continue;
            }
            for (j=0; j<end-start+1; j++) {
                INTEGER(st)[j] = INTEGER(order)[j+start];
                REAL(sq)[j] = REAL(x)[j+start];
            }
            PROTECT(dst = duplicate(st));
            PROTECT(rt = fastradixint(dst, ridx));
            for (j=0; j<end-start+1; j++) {
                INTEGER(order)[j+start] = INTEGER(st)[INTEGER(rt)[j]-1];
                REAL(x)[j+start] = REAL(sq)[INTEGER(rt)[j]-1];
            }
            UNPROTECT(4); // st, dst, sq, rt
        }
        UNPROTECT(1); // ridx
    }
    UNPROTECT(3); // xtmp, order, ordertmp
    if (LOGICAL(return_index)[0]) return(order); 
    return(x);
}
