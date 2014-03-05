#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

// See dogroups.c for these shared variables.
extern size_t sizes[];
#define SIZEOF(x) sizes[TYPEOF(x)]
//

// reverse a vector - equivalent of rev(x) in base, but implemented in C and about 12x faster (on 1e8)
SEXP setrev(SEXP x) {
    R_len_t j, n, len;
    size_t size;
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

SEXP reorder(SEXP x, SEXP order)
{
    // For internal use only by setkey().
    // Reordering a vector in-place doesn't change generations so we can skip SET_STRING_ELT overhead etc.
    // Speed is dominated by page fetch when input is randomly ordered so we're at the software limit here (better bus etc should shine).
    // 'order' must strictly be a permutation of 1:n (i.e. no repeats, zeros or NAs)
    // If only a small subset is reordered, this is detected using start and end.
    // x may be a vector, or a list of vectors e.g. data.table
    char *tmp, *tmpp, *vd;
    SEXP v;
    R_len_t i, j, itmp, nrow, ncol, start, end;
    size_t size; // must be size_t, otherwise bug #5305 (integer overflow in memcpy)

    if (isNewList(x)) {
        nrow = length(VECTOR_ELT(x,0));
        ncol = length(x);
        for (i=0;i<ncol;i++) {
            v = VECTOR_ELT(x,i);
            if (SIZEOF(v) == 0) error("Item %d of list is type '%s' which isn't yet supported", i+1, type2char(TYPEOF(v)));
            if (length(v)!=nrow) error("Column %d is length %d which differs from length of column 1 (%d). Invalid data.table.", i+1, length(v), nrow);
        }
    } else {
        if (SIZEOF(x) == 0) error("reorder accepts vectors but this non-VECSXP is type '%s' which isn't yet supported", type2char(TYPEOF(x)));
        nrow = length(x);
        ncol = 1;
    }
    if (!isInteger(order)) error("order must be an integer vector");
    if (length(order) != nrow) error("nrow(x)[%d]!=length(order)[%d]",nrow,length(order));
    
    start = 0;
    while (start<nrow && INTEGER(order)[start] == start+1) start++;
    if (start==nrow) return(R_NilValue);  // input is 1:n, nothing to do
    end = nrow-1;
    while (INTEGER(order)[end] == end+1) end--;
    for (i=start; i<=end; i++) { itmp=INTEGER(order)[i]-1; if (itmp<start || itmp>end) error("order is not a permutation of 1:nrow[%d]", nrow); }
    // Creorder is for internal use (so we should get the input right!), but the check above seems sensible, otherwise
    // would be segfault below. The for loop above should run in neglible time (sequential) and will also catch NAs.
    // It won't catch duplicates in order, but that's ok. Checking that would be going too far given this is for internal use only.
    
    tmp=(char *)malloc((end-start+1)*sizeof(double));   // Enough working space for one column of the largest type. setSizes() has a check too.
                                                        // So we can reorder a 10GB table in 16GB of RAM
    if (!tmp) error("unable to allocate %d * %d bytes of working memory for reordering data.table", end-start+1, sizeof(double));
    for (i=0; i<ncol; i++) {
        v = isNewList(x) ? VECTOR_ELT(x,i) : x;
        size = SIZEOF(v);
        if (!size) error("don't know how to reorder type '%s' of column %d. Please send this message to datatable-help",type2char(TYPEOF(v)),i+1);
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
    free(tmp);
    return(R_NilValue);
}


/* 
used to be : 
for (j=0;j<nrow;j++) {
  memcpy((char *)tmpp, (char *)DATAPTR(VECTOR_ELT(dt,i)) + ((size_t)(INTEGER(order)[j]-1))*size, size);
  tmpp += size;
}
This added 5s in 4e8 calls (see below) [-O3 on]. That 5s is insignificant vs page fetch, though, unless already
ordered when page fetch goes away. Perhaps memcpy(dest, src, 4) and memcpy(dest, src, 8) would be optimized to remove
call overhead but memcpy(dest, src, size) isn't since optimizer doesn't know value of 'size' variable up front.

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



