#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
/*
Don't know why we need this. Hang over from old? Remove and see if win-builder bites.
#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP setattrib();
EXPORT SEXP copy();
#endif
*/

// Wrappers for R internal functions. We can't rely on calling
// Rf_setAttrib and Rf_duplicate directly from .Call in R on
// all platforms, as we found out when v1.6.5 went to CRAN on
// 25 Aug 2011, see Professor Ripley's response that day.

SEXP setattrib(SEXP x, SEXP name, SEXP value)
{
    setAttrib(x,name,value);
    return(R_NilValue);
}               

SEXP copy(SEXP x)
{
    return(duplicate(x));
} 


