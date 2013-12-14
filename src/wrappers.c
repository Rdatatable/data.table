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
    if (TYPEOF(name) != STRSXP) error("Attribute name must be of type character");
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

SEXP address(SEXP x)
{
    // A better way than : http://stackoverflow.com/a/10913296/403310
    char buffer[32];
    snprintf(buffer, 32, "%p", (void *)x);
    return(ScalarString(mkChar(buffer)));
}

SEXP copyNamedInList(SEXP x)
{
    // As from R 3.1.0 list() no longer copies NAMED inputs
    // Since data.table allows subassignment by reference, we need a way to copy NAMED inputs, still.
    // But for many other applications (such as in j and elsewhere internally) the new non-copying list() in R 3.1.0 is very welcome.
    
    // This is intended to be called just after list(...) in data.table().  It isn't for use on a single data.table, as 
    // member columns of a list aren't marked as NAMED when the VECSXP is.
    
    // For now, this makes the old behaviour of list() in R<3.1.0 available for use, where we need it.
    
    if (TYPEOF(x) != VECSXP) error("x isn't a VECSXP");
    for (int i=0; i<LENGTH(x); i++) {
	    if (NAMED(VECTOR_ELT(x, i))) {
	        SET_VECTOR_ELT(x, i, duplicate(VECTOR_ELT(x,i)));
	    }
	}
	return R_NilValue;
}

