#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

// adapted from Michael Herf's code - http://stereopsis.com/radix.html
// TO IMPLEMENT (probably) - R's long vector support
// returns sort 'order' or 'value' depending on 'return_index' parameter (but not both currently)
// allows for -ve integer values and NA (not possible in R's NA)
// No restrictions imposed (stack size is small and is 3-pass) as opposed to base R's (max-min <= 1e5) to radix sort
// better to replace regularorder1 (as base R seems to use 1-pass IIUC and therefore will be faster wherever applicable)
#define _0(x) (x & 0x7FF)
#define _1(x) (x >> 11 & 0x7FF)
#define _2(x) (x >> 22)

unsigned int flip_int(unsigned int f) {
    return ((int)(f ^ 0x80000000));
} 

void flip_int_ref(unsigned int *f) {
    *f = ((int)(*f ^ 0x80000000));
}
unsigned int invert_flip_int(unsigned int f) {
    return ((int)(f ^ 0x80000000));
}

void flip_int_decr(unsigned int *f) {
    *f = ((int)(*f ^ 0xFFFFFFFF));
}

SEXP fastradixint(SEXP x, SEXP return_index, SEXP decreasing) {
    int i;
    unsigned int pos, fi, si, n;
    unsigned int sum0 = 0, sum1 = 0, sum2 = 0, tsum;    
    SEXP ans, order, ordertmp;
    
    n = length(x);
    if (!isInteger(x) || n <= 0) error("Argument 'x' to 'fastradixint' must be non-empty and of type 'integer'");
    if (TYPEOF(return_index) != LGLSXP || length(return_index) != 1) error("Argument 'return_index' to 'fastradixint' must be logical TRUE/FALSE");
    if (TYPEOF(decreasing) != LGLSXP || length(decreasing) != 1 || LOGICAL(decreasing)[0] == NA_LOGICAL) error("Argument 'decreasing' to 'fastradixint' must be logical TRUE/FALSE");
    
    ans  = PROTECT(allocVector(INTSXP, n));
    order = PROTECT(allocVector(INTSXP, n));
    ordertmp = PROTECT(allocVector(INTSXP, n));
    
    unsigned int *array = (unsigned int*)INTEGER(x);
    unsigned int *sort = (unsigned int*)INTEGER(ans);     

    // 3 histograms on the stack:
    const unsigned int stack_hist = 2048;
    unsigned int b0[stack_hist * 3];
    unsigned int *b1 = b0 + stack_hist;
    unsigned int *b2 = b1 + stack_hist;
    
    // definitely faster on big data than a for-loop
    memset(b0, 0, stack_hist*3*sizeof(unsigned int));

    // Step 1:  parallel histogramming pass
    for (i=0;i<n;i++) {
        if (LOGICAL(decreasing)[0]) flip_int_decr(&array[i]);
        fi = flip_int((unsigned int)array[i]);
        b0[_0(fi)]++;
        b1[_1(fi)]++;
        b2[_2(fi)]++;
    }
    
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
    }
        
    for (i=0;i<n;i++) {
        fi = array[i];
        flip_int_ref(&fi);
        pos = _0(fi);
        sort[++b0[pos]] = fi;
        INTEGER(order)[b0[pos]] = i;
    }
    
    for (i=0;i<n;i++) {
        si = sort[i];
        pos = _1(si);
        array[++b1[pos]] = si;
        INTEGER(ordertmp)[b1[pos]] = INTEGER(order)[i];
    }

    for (i=0;i<n;i++) {
        fi = array[i];
        pos = _2(fi);
        sort[++b2[pos]] = invert_flip_int(fi);
        if (LOGICAL(decreasing)[0]) flip_int_decr(&sort[b2[pos]]);
        INTEGER(order)[b2[pos]] = INTEGER(ordertmp)[i]+1;
    }
    UNPROTECT(3); // order, ordertmp, ans
    if (LOGICAL(return_index)[0]) return(order);
    return(ans);
}
