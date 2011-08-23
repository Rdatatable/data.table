#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP reorder();
#endif

// See dogroups.c for these shared variables.
int sizes[100];
int sizesSet;
void setSizes();
#define SIZEOF(x) sizes[TYPEOF(x)]
//

SEXP reorder(SEXP dt, SEXP order)
{
    // For internal use only by fastorder().
    char *tmp, *tmpp;
    int i, j, nrow, size;
    if (!sizesSet) setSizes();
    nrow = length(VECTOR_ELT(dt,0));
    if (length(order) != nrow) error("logical error nrow(dt)!=length(order)");
    tmp=calloc(nrow,sizeof(double));  // sizeof largest type. 8 on all platforms?
    if (!tmp) error("unable to allocate temporary working memory for reordering data.table");
    for (i=0;i<length(dt);i++) {
        size=SIZEOF(VECTOR_ELT(dt,i));
        if (!size) error("don't know how to reorder type %d of column %d. Please send this message to maintainer('data.table')",TYPEOF(VECTOR_ELT(dt,i)),i+1);
        tmpp=tmp;
        for (j=0;j<nrow;j++) {
            memcpy(tmpp, (char *)DATAPTR(VECTOR_ELT(dt,i)) + (INTEGER(order)[j]-1)*size, size);
            tmpp += size;
        }
        memcpy((char *)DATAPTR(VECTOR_ELT(dt,i)), tmp, nrow*size);
    }
    free(tmp);
    return(R_NilValue);
}               

