#include <R.h>
#define USE_RINTERNALS  // test the speed increase of this, and test code works with and without it. It turns INTEGER() into a macro rather than a function call.
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP duplist();
#endif

SEXP duplist(SEXP l, SEXP ans, SEXP anslen, SEXP order)
{
    // Returns in ans (by reference) the positions of the non-duplicates. The first of each group.
    // Works like which(!duplicated(<list logic>)) but faster.
    // l must be a list of same length integer vectors
    // ans is allocated first (maximum length the number of rows) and the length returned in anslen.
    int i,j,nrow,ncol,len;
    ncol = length(l);
    nrow = length(VECTOR_ELT(l,0));
    len=1;
    INTEGER(ans)[0] = 1;    // first row is always the first of the first group
    if (INTEGER(order)[0] == -1) {      // i.e. order is missing, same order as row order. Using MISSING() does not seem stable under windows. Always having arguments passed in seems a good idea anyway.
        for (i=1; i<nrow; i++) {
            j = 0;
            while (INTEGER(VECTOR_ELT(l,j))[i] == INTEGER(VECTOR_ELT(l,j))[i-1] && ++j<ncol);
            if (j<ncol) INTEGER(ans)[len++] = i+1;
        }
    } else {
        for (i=1; i<nrow; i++) {
            j = 0;
            while (INTEGER(VECTOR_ELT(l,j))[INTEGER(order)[i]-1] == INTEGER(VECTOR_ELT(l,j))[INTEGER(order)[i-1]-1] && ++j<ncol);
            if (j<ncol) INTEGER(ans)[len++] = i+1;
        }
    }
    INTEGER(anslen)[0] = len;
    return(R_NilValue);
}
