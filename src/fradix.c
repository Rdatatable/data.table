#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

// adapted from Michael Herf's code - http://stereopsis.com/radix.html
unsigned long flip_double(unsigned long f) {
    unsigned long mask = -(long)(f >> 63) | 0x8000000000000000;
    return f ^ mask;
}

void flip_double_ref(unsigned long *f) {
    unsigned long mask = -(long)(*f >> 63) | 0x8000000000000000;
    *f ^= mask;
}

unsigned long invert_flip_double(unsigned long f) {
    unsigned long mask = ((f >> 63) - 1) | 0x8000000000000000;
    return f ^ mask;
}

// utils for accessing 11-bit quantities
#define _0(x) (x & 0x7FF)
#define _1(x) (x >> 11 & 0x7FF)
#define _2(x) (x >> 22 & 0x7FF)
#define _3(x) (x >> 33 & 0x7FF)
#define _4(x) (x >> 44 & 0x7FF)
#define _5(x) (x >> 55)

// x should be of type numeric
SEXP fradix_order(SEXP vec) {
    int i;
    unsigned long pos, fi, si, n;
    unsigned long sum0 = 0, sum1 = 0, sum2 = 0, sum3 = 0, sum4 = 0, sum5 = 0, tsum;    
    SEXP x, xtmp, order, ordertmp;
    
    if (TYPEOF(vec) != VECSXP) error("Input argument to 'fradix_order' must be a list");
    if (length(vec) != 1) error("Input argument to 'fradix_order' must be a list of length 1");

    PROTECT(x = VECTOR_ELT(vec, 0));
    n = length(x);
    if (!isReal(x) || n <= 0) error("Input argument to 'fradix_order' must be a list of length 1 and it's values should be type 'numeric' of length > 0");

    xtmp  = PROTECT(allocVector(REALSXP, n));
    ordertmp = PROTECT(allocVector(INTSXP, n));
    order = PROTECT(allocVector(INTSXP, n));

    unsigned long *array = (unsigned long*)REAL(x);
    unsigned long *sort = (unsigned long*)REAL(xtmp);
            
    // 6 histograms on the stack:
    const unsigned long stack_hist = 2048;
    unsigned long b0[stack_hist * 6];
    unsigned long *b1 = b0 + stack_hist;
    unsigned long *b2 = b1 + stack_hist;
    unsigned long *b3 = b2 + stack_hist;
    unsigned long *b4 = b3 + stack_hist;
    unsigned long *b5 = b4 + stack_hist;

    // definitely faster on big data than a for-loop
    memset(b0, 0, stack_hist*6*sizeof(unsigned long));

    // Step 1:  parallel histogramming pass
    for (i=0;i<n;i++) {
        // NA in unsigned long is 7ff00000000007a2 and NaN is 7ff8000000000000
        // flip the sign bit to get fff00000000007a2 and NaN is fff8000000000000
        // this'll result in the right order (NaN first, then NA)
        // flip NaN/NA sign bit so that they get sorted in the front (special for data.table)
        if (ISNAN(REAL(x)[i])) flip_double_ref(&array[i]);
        fi = flip_double((unsigned long)array[i]);
        b0[_0(fi)]++;
        b1[_1(fi)]++;
        b2[_2(fi)]++;
        b3[_3(fi)]++;
        b4[_4(fi)]++;
        b5[_5(fi)]++;
    }
    
    // Step 2:  Sum the histograms -- each histogram entry records the number of values preceding itself.
    for (i=0;i<stack_hist;i++) {

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

    UNPROTECT(4);
    return(order);
}
