#include <R.h>
#define USE_RINTERNALS  // robustly test the speed increase of this (didn't have a good test case yet), and test code works with and without it. It turns INTEGER() into a macro rather than a function call.
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP vecref();
#endif

SEXP vecref(SEXP v, SEXP start, SEXP len)
{
    // TO DO: replace memcpy with a direct pointer, but SEXPREC structure seems to be the limitation because its header followed by the data, not a pointer to the data
    // memcpy should be a little quicker than the C loops you see in split and ExtractData internals.
    // The first 4 ifs could be removed but leave for now for robustness.  Later we will wrap this up inside
    // a larger C loop (see note on with() in [.data.table) so these ifs's can be performed once only.
    int mode, p=1;
    SEXP ans;
    if (TYPEOF(start) != INTSXP) {
        // e.g. if vec(v,1,4) is called, the mode of the constants 1 and 4 will be double, not integer as you would expect. It would be a pain to force the user to coerce. And its slower to have a test in R vecref, or a call to as.integer always.
        PROTECT(start = coerceVector(start,INTSXP));
        p++;
        warning("coercing start to integer");
    }
    if (TYPEOF(len) != INTSXP) {
        PROTECT(len = coerceVector(len,INTSXP));
        p++;
        warning("coercing len to integer");
    }
    if (INTEGER(start)[0]<1 || INTEGER(len)[0]<0) error("start<1 or len<0");
    if (INTEGER(start)[0]+INTEGER(len)[0]-1 > length(v)) error("offset+len out of bounds");
    mode = TYPEOF(v);
    PROTECT(ans = allocVector(mode, INTEGER(len)[0]));
    switch(mode) {
    case INTSXP :
        memcpy(INTEGER(ans), INTEGER(v)+INTEGER(start)[0]-1, INTEGER(len)[0]*sizeof(int));
        break;
    case REALSXP :
        memcpy(REAL(ans), REAL(v)+INTEGER(start)[0]-1, INTEGER(len)[0]*sizeof(double));
        break;
    case LGLSXP :
        memcpy(LOGICAL(ans), LOGICAL(v)+INTEGER(start)[0]-1, INTEGER(len)[0]*sizeof(int));
        break;
    default:
        error("only integer,double and logical vectors currently coded for");
    }
    UNPROTECT(p);
    return(ans);
}
