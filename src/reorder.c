#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

// See dogroups.c for these shared variables.
extern int sizes[];
extern int sizesSet;
extern void setSizes();
#define SIZEOF(x) sizes[TYPEOF(x)]
//

SEXP reorder(SEXP dt, SEXP order)
{
    // For internal use only by fastorder().
    char *tmp, *tmpp;
    R_len_t i, j, nrow, size;
    if (!sizesSet) setSizes();
    nrow = length(VECTOR_ELT(dt,0));
    for (i=0;i<length(dt);i++) {
        if (length(VECTOR_ELT(dt,i))!=nrow) error("Column %d is length %d which differs from length of column 1 (%d). Invalid data.table. Check NEWS link at top of ?data.table for latest bug fixes. If not already reported and fixed, please report to datatable-help.", i+1, length(VECTOR_ELT(dt,i)), nrow);
    }
    if (length(order) != nrow) error("logical error nrow(dt)!=length(order)");
    if (sizeof(double)!=8) error("sizeof(double) isn't 8");   // 8 on both 32bit and 64bit.
    tmp=(char *)Calloc(nrow,double);   // Enough working space for the largest type. setSizes() has a check too.
    if (!tmp) error("unable to allocate temporary working memory for reordering data.table");
    for (i=0;i<length(dt);i++) {
        size=SIZEOF(VECTOR_ELT(dt,i));
        if (!size) error("don't know how to reorder type %d of column %d. Please send this message to maintainer('data.table')",TYPEOF(VECTOR_ELT(dt,i)),i+1);
        tmpp=tmp;
        for (j=0;j<nrow;j++) {
            memcpy((char *)tmpp, (char *)DATAPTR(VECTOR_ELT(dt,i)) + ((size_t)(INTEGER(order)[j]-1))*size, size);
            tmpp += size;
        }
        memcpy((char *)DATAPTR(VECTOR_ELT(dt,i)), (char *)tmp, ((size_t)nrow)*size);
    }
    Free(tmp);
    return(R_NilValue);
}


