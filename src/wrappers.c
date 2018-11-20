#include "data.table.h"
#include <Rdefines.h>

// Wrappers for R internal functions. We can't rely on calling
// Rf_setAttrib and Rf_duplicate directly from .Call in R on
// all platforms, as we found out when v1.6.5 went to CRAN on
// 25 Aug 2011, see Professor Ripley's response that day.

SEXP setattrib(SEXP x, SEXP name, SEXP value)
{
  if (!isString(name) || LENGTH(name)!=1) error("Attribute name must be a character vector of length 1");
  if (!isNewList(x) &&
      strcmp(CHAR(STRING_ELT(name,0)),"class")==0 &&
      isString(value) && LENGTH(value)>0 &&
      (strcmp(CHAR(STRING_ELT(value, 0)),"data.table")==0 || strcmp(CHAR(STRING_ELT(value,0)),"data.frame")==0) ) {
    error("Internal structure doesn't seem to be a list. Can't set class to be 'data.table' or 'data.frame'. Use 'as.data.table()' or 'as.data.frame()' methods instead.");
  }
  if (isLogical(x) && LENGTH(x)==1 &&
      (x==ScalarLogical(TRUE) || x==ScalarLogical(FALSE) || x==ScalarLogical(NA_LOGICAL))) {  // R's internal globals, #1281
    x = PROTECT(duplicate(x));
    setAttrib(x, name, MAYBE_REFERENCED(value) ? duplicate(value) : value);
    UNPROTECT(1);
    return(x);
  }
  setAttrib(x, name,
    MAYBE_REFERENCED(value) ? duplicate(value) : value);
    // duplicate is temp fix to restore R behaviour prior to R-devel change on 10 Jan 2014 (r64724).
    // TO DO: revisit. Enough to reproduce is: DT=data.table(a=1:3); DT[2]; DT[,b:=2]
    // ... Error: selfrefnames is ok but tl names [1] != tl [100]
  return(R_NilValue);
}

// fix for #1142 - duplicated levels for factors
SEXP setlevels(SEXP x, SEXP levels, SEXP ulevels) {

  R_len_t nx = length(x), i;
  SEXP xchar, newx;
  xchar = PROTECT(allocVector(STRSXP, nx));
  for (i=0; i<nx; i++)
    SET_STRING_ELT(xchar, i, STRING_ELT(levels, INTEGER(x)[i]-1));
  newx = PROTECT(chmatch(xchar, ulevels, NA_INTEGER, FALSE));
  for (i=0; i<nx; i++) INTEGER(x)[i] = INTEGER(newx)[i];
  setAttrib(x, R_LevelsSymbol, ulevels);
  UNPROTECT(2);
  return(x);
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

SEXP setmutable(SEXP x)
{
  // called from one single place at R level. TODO: avoid somehow, but fails tests without
  // At least the SET_NAMED() direct call is passed 0 and makes no assumptions about >0.  Good enough for now as patch for CRAN is needed.
  SET_NAMED(x,0);
  return(x);
}

SEXP address(SEXP x)
{
  // A better way than : http://stackoverflow.com/a/10913296/403310
  char buffer[32];
  snprintf(buffer, 32, "%p", (void *)x);
  return(mkString(buffer));
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
    SEXP col = VECTOR_ELT(x, i);
    if (MAYBE_REFERENCED(col) || ALTREP(col)) {
      SET_VECTOR_ELT(x, i, duplicate(col));
    }
  }
  return R_NilValue;
}

SEXP expandAltRep(SEXP x)
{
  // used by setDT to ensure altrep vectors in columns are expanded. Such altrep objects typically come from tests or demos, since
  // the sequence 1:n does not occur in real-world data as a column, very often.
  // Note that data.table() calls copyNamedInList() (above) which has the side-effect of expanding altrep vectors too.
  // We need regular expanded columns in data.table because `:=` relies on that, for example.
  // At R level (for example [.data.table) we use and benefit from altrep vectors very much. It's just as columns that we expand them.
  // See extensive discussion in issue #2866

  if (TYPEOF(x) != VECSXP) error("x isn't a VECSXP");
  for (int i=0; i<LENGTH(x); i++) {
    SEXP col = VECTOR_ELT(x,i);
    if (ALTREP(col)) {
      SET_VECTOR_ELT(x, i, duplicate(col));
    }
  }
  return R_NilValue;
}

SEXP dim(SEXP x)
{
  // fast implementation of dim.data.table

  if (TYPEOF(x) != VECSXP) {
    error("dim.data.table expects a data.table as input (which is a list), but seems to be of type %s",
        type2char(TYPEOF(x)));
  }

  SEXP ans = PROTECT(allocVector(INTSXP, 2));
  if(length(x) == 0) {
    INTEGER(ans)[0] = 0;
    INTEGER(ans)[1] = 0;
  }
  else {
    INTEGER(ans)[0] = length(VECTOR_ELT(x, 0));
    INTEGER(ans)[1] = length(x);
  }
  UNPROTECT(1);
  return ans;
}

