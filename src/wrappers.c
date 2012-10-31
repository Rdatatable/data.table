#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

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
LibExtern int	R_EvalDepth;

SEXP EvalDepth()
{
    return(ScalarInteger(R_EvalDepth));
}

SEXP copyattr(SEXP from, SEXP to)
{
    // for use by [.data.table to retain attribs such as "comments" when subsetting and j is missing
    copyMostAttrib(from, to);
    return(R_NilValue);
}


