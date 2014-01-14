#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

// See dogroups.c for these shared variables.
extern int sizes[];
#define SIZEOF(x) sizes[TYPEOF(x)]
//

// reverse a vector - equivalent of rev(x) in base, but implemented in C and about 12x faster (on 1e8)
SEXP setrev(SEXP x) {
    R_len_t j, n, len, size;
    char *tmp, *xt;
    if (TYPEOF(x) == VECSXP || isMatrix(x)) error("Input 'x' must be a vector");
    len = length(x);
    if (len <= 1) return(x);
    size = SIZEOF(x);
    if (!size) error("don't know how to reverse type '%s' of input 'x'.",type2char(TYPEOF(x)));
    n = (int)(len/2);
    xt = (char *)DATAPTR(x);
    if (size==4) {
        tmp = (char *)Calloc(1, int);
        if (!tmp) error("unable to allocate temporary working memory for reordering x");
        for (j=0;j<n;j++) {
            *(int *)tmp = ((int *)xt)[j];  // just copies 4 bytes (pointers on 32bit too)
            ((int *)xt)[j] = ((int *)xt)[len-1-j];
            ((int *)xt)[len-1-j] = *(int *)tmp;
        }
    } else {
        if (size!=8) error("Size of x isn't 4 or 8");
        tmp = (char *)Calloc(1, double);
        if (!tmp) error("unable to allocate temporary working memory for reordering x");
        for (j=0;j<n;j++) {
            *(double *)tmp = ((double *)xt)[j];  // just copies 8 bytes (pointers on 64bit too)
            ((double *)xt)[j] = ((double *)xt)[len-1-j];
            ((double *)xt)[len-1-j] = *(double *)tmp;
        }
    }
    Free(tmp);
    return(R_NilValue);
}

// reorder a vector given the vec and an order vector
// note that the idea is a *carbon copy* of 'reorder' (below) - except that 
// it's convenient to have a separate function for vectors
SEXP setreordervec(SEXP x, SEXP order) {
    char *tmp, *xt;
    R_len_t j, n, size;
    if (TYPEOF(x) == VECSXP || isMatrix(x)) error("Input 'x' must be a vector");
    n = length(x);
    if (n <= 1) return(x); // special case
    if (length(order) != n) error("logical error, length(x) != length(order)");
    if (TYPEOF(order) != INTSXP) error("type error, 'order' should be integer type");
    size = SIZEOF(x);
    if (!size) error("don't know how to reorder type '%s' of input 'x'.",type2char(TYPEOF(x)));
    if (sizeof(int) !=  4) error("sizeof(int) isn't 4");
    if (sizeof(double) != 8) error("sizeof(double) isn't 8");
    xt = (char *)DATAPTR(x);
    if (size == 4) {
        tmp = (char *)Calloc(n, int);
        if (!tmp) error("unable to allocate temporary working memory for reordering x");
        for (j = 0; j < n; j++) {
            ((int *)tmp)[j] = ((int *)xt)[INTEGER(order)[j]-1];  // just copies 4 bytes (pointers on 32bit too)
        }
    } else {
        if (size != 8) error("Size of x's type isn't 4 or 8");
        tmp = (char *)Calloc(n, double);
        if (!tmp) error("unable to allocate temporary working memory for reordering x");
        for (j = 0; j < n; j++) {
            ((double *)tmp)[j] = ((double *)xt)[INTEGER(order)[j]-1];  // just copies 8 bytes (pointers on 64bit too)
        }
    }
    memcpy(xt, tmp, ((size_t)n)*size);
    Free(tmp);
    return(R_NilValue);
}

SEXP reorder(SEXP dt, SEXP order)
{
    // For internal use only by fastorder().
    // Reordering a vector in-place doesn't change generations so we can skip SET_STRING_ELT overhead etc.
    // Speed is dominated by page fetch when input is randomly ordered so we're at the software limit here (better bus etc should shine).
    // 'order' must strictly be a permutation of 1:n (i.e. no repeats, zeros or NAs)
    // If only a small subset is reordered, this is detected using start and end.
    char *tmp, *tmpp, *vd;
    SEXP v;
    R_len_t i, j, nrow, size, start, end;
    nrow = length(VECTOR_ELT(dt,0));
    for (i=0;i<length(dt);i++) {
        if (length(VECTOR_ELT(dt,i))!=nrow) error("Column %d is length %d which differs from length of column 1 (%d). Invalid data.table. Check NEWS link at top of ?data.table for latest bug fixes. If not already reported and fixed, please report to datatable-help.", i+1, length(VECTOR_ELT(dt,i)), nrow);
    }
    if (length(order) != nrow) error("logical error nrow(dt)!=length(order)");
    if (sizeof(int)!=4) error("sizeof(int) isn't 4");
    if (sizeof(double)!=8) error("sizeof(double) isn't 8");   // 8 on both 32bit and 64bit.
    start = 0;
    while (start<nrow && INTEGER(order)[start] == start+1) start++;
    if (start==nrow) return(R_NilValue);  // input is 1:n, nothing to do
    end = nrow-1;
    while (INTEGER(order)[end] == end+1) end--;
    tmp=(char *)Calloc(end-start+1,double);   // Enough working space for one column of the largest type. setSizes() has a check too.
                                              // So we can reorder a 10GB table in 16GB of RAM
    if (!tmp) error("unable to allocate temporary working memory for reordering data.table");
    for (i=0;i<length(dt);i++) {
        v = VECTOR_ELT(dt,i);
        size = SIZEOF(v);
        if (!size) error("don't know how to reorder type '%s' of column %d. Please send this message to datatable-help",type2char(TYPEOF(VECTOR_ELT(dt,i))),i+1);
        tmpp=tmp;
        vd = (char *)DATAPTR(v);
        if (size==4) {
            for (j=start;j<=end;j++) {
                *(int *)tmpp = ((int *)vd)[INTEGER(order)[j]-1];  // just copies 4 bytes (pointers on 32bit too)
                tmpp += 4;
            }
        } else {
            if (size!=8) error("Size of column %d's type isn't 4 or 8", i+1);
            for (j=start;j<=end;j++) {
                *(double *)tmpp = ((double *)vd)[INTEGER(order)[j]-1];  // just copies 8 bytes (pointers on 64bit too)
                tmpp += 8;
            }
        }
        memcpy(vd + start*size, tmp, (end-start+1) * size);
    }
    Free(tmp);
    return(R_NilValue);
}


/* 
used to be : 
for (j=0;j<nrow;j++) {
  memcpy((char *)tmpp, (char *)DATAPTR(VECTOR_ELT(dt,i)) + ((size_t)(INTEGER(order)[j]-1))*size, size);
  tmpp += size;
}
This added 5s in 4e8 calls (see below) even with -O3. That 5s is insignificant vs page fetch, though, unless already
ordered when page fetch goes away. (Perhaps memcpy(dest, src, 4) and memcpy(dest, src, 8) would be optimized to remove
call overhead but memcpy(dest, src, size) can't be since optimizer doesn't know value of 'size' variable up front.)

DT = setDT(lapply(1:4, function(x){sample(1e5,1e8,replace=TRUE)}))
o = fastorder(DT, 1L)
none = 1:1e8                           
                                       
# worst case 5% faster, thoroughly random     # before   after
system.time(.Call(Creorder,DT,o))             # 102.301  97.082
system.time(.Call(Creorder,DT,o))             # 102.602  97.001

# best case 50% faster, already ordered       # before   after
system.time(.Call(Creorder,DT,none))          # 9.310    4.187
system.time(.Call(Creorder,DT,none))          # 9.295    4.077

# Somewhere inbetween 5%-50% would be e.g. grouped data where we're reordering the blocks.
# But, likely the worst case speedup most of the time. On my slow netbook anyway, with slow RAM.

# However, on decent laptop (faster RAM/bus etc) it looks better at 40% speedup consistently ...

# thoroughly random                           # before   after
system.time(.Call("Creorder",DT,o))           # 33.216   19.184
system.time(.Call("Creorder",DT,o))           # 30.556   18.667

# already ordered                             # before   after
system.time(.Call("Creorder",DT,none))        # 4.172    2.384
system.time(.Call("Creorder",DT,none))        # 4.076    2.356

*/



