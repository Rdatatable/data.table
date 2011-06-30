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

SEXP reorder(SEXP dt, SEXP order, SEXP cols)
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
        tmpp=tmp;
        for (j=0;j<nrow;j++) {
            memcpy(tmpp, (char *)DATAPTR(VECTOR_ELT(dt,i)) + (INTEGER(order)[j]-1)*size, size);
            tmpp += size;
        }
        memcpy((char *)DATAPTR(VECTOR_ELT(dt,i)), tmp, nrow*size);
    }
    free(tmp);
    setAttrib(dt, install("sorted"), cols);  // wasn't sure how to set attributes in calling scope by reference at R level, here is fine
    return(R_NilValue);
}               

