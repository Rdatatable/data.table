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

SEXP copyattr(SEXP from, SEXP to)
{
    // for use by [.data.table to retain attribs such as "comments" when subsetting and j is missing
    copyMostAttrib(from, to);
    return(R_NilValue);
}

SEXP setlistelt(SEXP l, SEXP i, SEXP value)
{
    R_len_t i2;
    // Internal use only. So that := can update elements of a list of data.table, #2204. Just needed to overallocate/grow the VECSXP.
    if (!isNewList(l)) error("First argument to setlistelt must be a list()");
    if (!isInteger(i) || LENGTH(i)!=1) error("Second argument to setlistelt must a length 1 integer vector");
    i2 = INTEGER(i)[0];
    if (LENGTH(l) < i2 || i2<1) error("i (%d) is outside the range of items [1,%d]",i2,LENGTH(l));
    SET_VECTOR_ELT(l, i2-1, value);
    return(R_NilValue);
}

SEXP setnamed(SEXP x, SEXP value)
{
    if (!isInteger(value) || LENGTH(value)!=1) error("Second argument to setnamed must a length 1 integer vector");
    SET_NAMED(x,INTEGER(value)[0]);
    return(x);
}


