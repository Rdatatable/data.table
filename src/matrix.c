#include "data.table.h"
#include <Rdefines.h>
#include <ctype.h>   // for isdigit

SEXP asmatrix(SEXP dt, SEXP rownames)
{
  // Determine rows and colums
  int ncol = length(dt);
  int nrow = length(VECTOR_ELT(dt, 0));

  // Extract pointers to each column
  SEXP thisCol[ncol];
  for (int j=0; j<ncol; ++j) {
    thisCol[j] = VECTOR_ELT(dt, j);
  }
  
  // Extract column types and determine type to coerce to
  int maxType=RAWSXP;
  int thisType[ncol];
  bool integer64=false; // are we coercing to integer64 class?
  for (int j=0; j<ncol; ++j) {
    // Extract column type  
    thisType[j] = TYPEOF(thisCol[j]);
    // Determine the maximum type now that we have inspected this column
    if (maxType==VECSXP); // nothing to do, max type is already list
    // non-atomic non-list types are coerced / wrapped in list, see #4196
    else if (TYPEORDER(thisType[j])>TYPEORDER(VECSXP)) maxType=VECSXP;
    // otherwise if this column is higher in typeorder list, set this type as maxType
    else if (TYPEORDER(thisType[j])>TYPEORDER(maxType)) maxType=thisType[j];
  }
  
  // allocate matrix
  int nprotect=0;
  SEXP ans = PROTECT(allocMatrix(maxType, nrow, ncol)); nprotect++;
  
  // Add dimnames
  SEXP dimnames = PROTECT(allocVector(VECSXP, 2)); nprotect++;
  SET_VECTOR_ELT(dimnames, 0, rownames);
  SET_VECTOR_ELT(dimnames, 1, getAttrib(dt, R_NamesSymbol));
  setAttrib(ans, R_DimNamesSymbol, dimnames);
  
  // If any nrow 0 we can now return. ncol == 0 handled in R.
  if (nrow == 0) {
    UNPROTECT(nprotect);
    return(ans);
  }
  
  // Coerce columns (if needed) and fill
  SEXP coerced[ncol];
  int ansloc=0; // position in vector to start copying to, filling by column.
  for (int j=0; j<ncol; ++j) {
    if (thisType[j] == maxType) { // no type coercion needed
      coerced[j] = thisCol[j]; 
    } else if (maxType==VECSXP) { // coercion to list not handled by memrecycle.
      if (isVectorAtomic(thisCol[j]) || thisType[j]==LISTSXP) {
        // Atomic vectors and pairlists can be coerced to list with coerceVector:
        coerced[j] = PROTECT(coerceVector(thisCol[j], maxType)); nprotect++;
      } else if (thisType[j]==EXPRSXP) {
        // For EXPRSXP each element must be wrapped in a list and re-coerced to EXPRSXP, otherwise column is LANGSXP
        coerced[j] = PROTECT(allocVector(VECSXP, nrow)); nprotect++;
        for (int i=0; i<nrow; ++i) {
          SEXP thisElement = PROTECT(coerceVector(VECTOR_ELT(thisCol[j], i), EXPRSXP)); nprotect++;
          SET_VECTOR_ELT(coerced[j], i, thisElement);
        }
      } else if (!isVector(thisCol[j])) { 
        // Anything not a vector we can assign directly through SET_VECTOR_ELT
        // Although tecnically there should only be one list element for any type met here,
        // the length of the type may be > 1, in which case the other columns in data.table
        // will have been recycled. We therefore in turn have to recycle the list elements
        // to match the number of rows.
        coerced[j] = PROTECT(allocVector(VECSXP, nrow)); nprotect++;
        for (int i=0; i<nrow; ++i) {
          SET_VECTOR_ELT(coerced[j], i, thisCol[j]);
        }
      } else { // should be unreachable
        error("Internal error: as.matrix cannot coerce type %s to list\n", type2char(thisType[j])); // # nocov
      }
    } else {
      // else coerces if needed within memrecycle
      coerced[j] = thisCol[j]; 
    }
    
    // Fill matrix with memrecycle
    const char *ret = memrecycle(ans, R_NilValue, ansloc, nrow, coerced[j], 0, -1, 0, "V1");
    // Warning when precision is lost after coercion, should not be possible to reach
    if (ret) warning(_("Column %d: %s"), j+1, ret); // # nocov
    // TODO: but maxType should handle that and this should never warn
    ansloc += nrow;
  }
    
  UNPROTECT(nprotect);  // ans, dimnames, coerced, thisElement
  return(ans);
}
