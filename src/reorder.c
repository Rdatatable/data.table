#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

// See dogroups.c for these shared variables.
extern int sizes[];
#define SIZEOF(x) sizes[TYPEOF(x)]
//

SEXP reorder(SEXP dt, SEXP order)
{
    // For internal use only by fastorder().
    // Reordering a vector in-place doesn't change generations so we can skip SET_STRING_ELT overhead etc.
    // Speed is dominated by page fetch when input is randomly ordered so we're at the software limit here (better bus etc should shine).
    char *tmp, *tmpp, *vd;
    SEXP v;
    R_len_t i, j, nrow, size;
    nrow = length(VECTOR_ELT(dt,0));
    for (i=0;i<length(dt);i++) {
        if (length(VECTOR_ELT(dt,i))!=nrow) error("Column %d is length %d which differs from length of column 1 (%d). Invalid data.table. Check NEWS link at top of ?data.table for latest bug fixes. If not already reported and fixed, please report to datatable-help.", i+1, length(VECTOR_ELT(dt,i)), nrow);
    }
    if (length(order) != nrow) error("logical error nrow(dt)!=length(order)");
    if (sizeof(int)!=4) error("sizeof(int) isn't 4");
    if (sizeof(double)!=8) error("sizeof(double) isn't 8");   // 8 on both 32bit and 64bit.
    tmp=(char *)Calloc(nrow,double);   // Enough working space for one column of the largest type. setSizes() has a check too.
                                       // So we can reorder a 10GB table in 16GB of RAM
    if (!tmp) error("unable to allocate temporary working memory for reordering data.table");
    for (i=0;i<length(dt);i++) {
        v = VECTOR_ELT(dt,i);
        size = SIZEOF(v);
        if (!size) error("don't know how to reorder type '%s' of column %d. Please send this message to datatable-help",type2char(TYPEOF(VECTOR_ELT(dt,i))),i+1);
        tmpp=tmp;
        vd = (char *)DATAPTR(v);
        if (size==4) {
            for (j=0;j<nrow;j++) {
                *(int *)tmpp = ((int *)vd)[INTEGER(order)[j]-1];  // just copies 4 bytes (pointers on 32bit too)
                tmpp += 4;
            }
        } else {
            if (size!=8) error("Size of column %d's type isn't 4 or 8", i+1);
            for (j=0;j<nrow;j++) {
                *(double *)tmpp = ((double *)vd)[INTEGER(order)[j]-1];  // just copies 8 bytes (pointers on 64bit too)
                tmpp += 8;
            }
        }
        memcpy(vd, tmp, ((size_t)nrow)*size);
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
ordered when page fetch goes away. (Perhaps memcpy call needs a const 4 or 8 rather than variable 'size' for optimizer
to remove the call overhead.)

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



