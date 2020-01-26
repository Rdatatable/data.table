#include "data.table.h"
#include <Rdefines.h>
#include <ctype.h>   // for isdigit

SEXP asmatrix(SEXP dt, SEXP rownames)
{
  // check matrix dimensions do not exceed INT_MAX. Should not be possible, but just in case:
  int64_t ncol64=xlength(dt);
  int64_t nrow64=xlength(VECTOR_ELT(dt, 0));
  if (ncol64 > INT_MAX || nrow64 > INT_MAX) 
    error(_("matrix dimensions may not exceed %d"), INT_MAX); // # nocov
  
  // From this point we are guaranteed nrow and ncol are int
  int ncol = (int) ncol64;
  int nrow = (int) nrow64;

  Rprintf("Matrix dim: c(%d, %d), length=%ld\n", nrow, ncol, nrow64*ncol64);
  
  // Extract pointers to each column
  SEXP thisCol[ncol];
  for (int j=0; j<ncol; ++j) {
    thisCol[j] = VECTOR_ELT(dt, j);
  }
  
  Rprintf("Vector of column pointers extracted.\n");
  
  // Extract column types and determine type to coerce to
  int maxType=RAWSXP;
  int thisType[ncol];
  bool integer64=false; // are we coercing to integer64 class?
  for (int j=0; j<ncol; ++j) {
    // Extract column type or flag column as special class requiring special coercion rules
    if (isFactor(thisCol[j])) thisType[j] = -1;
    else if (INHERITS(thisCol[j], char_Date) || INHERITS(thisCol[j], char_ITime) || 
             INHERITS(thisCol[j], char_POSIXt), INHERITS(thisCol[j], char_nanotime)) thisType[j] = -2;
    else if (INHERITS(thisCol[j], char_integer64)) thisType[j] = -3;
    else thisType[j] = TYPEOF(thisCol[j]);

    // Determine the maximum type now that we have inspected this column
    if (maxType==VECSXP); // nothing to do, max type is already list
    // factors and date/poix like always coerce to string
    else if ((thisType[j] == -1 || thisType[j] == -2) && TYPEORDER(maxType) < TYPEORDER(STRSXP)) maxType = STRSXP;
    // raw, logical, and integer can be coerced to integer64
    else if (thisType[j] == -3 && TYPEORDER(maxType) > TYPEORDER(REALSXP)) { maxType = INTSXP; integer64 = true; }
    // numeric, complex, and string cannot be coerced to integer64, nor integer64 to those types, so all promoted to STRSXP
    else if (thisType[j] == -3) maxType = STRSXP;
    // non-atomic non-list types are coerced / wrapped in list, see #4196
    else if (TYPEORDER(thisType[j])>TYPEORDER(VECSXP)) maxType=VECSXP;
    // otherwise if this column is higher in typeorder list, set this type as maxType
    else if (TYPEORDER(thisType[j])>TYPEORDER(maxType)) maxType=thisType[j];
    // Set int64 to false if it was previously the maxType and the new maxType > INTSXP
    if (integer64 && TYPEORDER(maxType)>TYPEORDER(INTSXP)) integer64 = false;
  }
  
  Rprintf("Max type to coerce to is: %s, integer64=%d\n", type2char(maxType), integer64);
  
  // allocate matrix
  int nprotect=0;
  SEXP ans = PROTECT(allocMatrix(maxType, nrow, ncol)); nprotect++;
  
  // Add dimnames
  SEXP dimnames = PROTECT(allocVector(VECSXP, 2)); nprotect++;
  SET_VECTOR_ELT(dimnames, 0, rownames);
  SET_VECTOR_ELT(dimnames, 1, getAttrib(dt, R_NamesSymbol));
  setAttrib(ans, R_DimNamesSymbol, dimnames);
  
  // If any dim 0 we can now return
  if (nrow == 0 || ncol == 0) {
    UNPROTECT(nprotect);
    return(ans);
  }
  
  Rprintf("Coercing columns...\n");
  
  // Coerce columns (if needed)
  SEXP coerced[ncol];
  for (int j=0; j<ncol; ++j) {
    Rprintf("  %d\n", j+1);
    if (thisType[j] == maxType) { // no type coercion needed
      coerced[j] = thisCol[j]; 
    } else if (TYPEORDER(thisType[j]) < TYPEORDER(VECSXP)) { // coercion handled by memrecycle
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
    }
  }
    
  Rprintf("Done!\n\nFilling matrix with memrecycle...\n");
  int ansloc=0; // position in vector to start copying to, filling by column.
  for (int j=0; j<ncol; ++j) {
    Rprintf("  %d\n", j+1);
    // else coerces if needed within memrecycle; with a no-alloc direct coerce from 1.12.4 (PR #3909)
    const char *ret = memrecycle(ans, R_NilValue, ansloc, nrow, coerced[j], 0, -1, 0, "V1");
    // Warning when precision is lost after coercion, should not be possible to reach
    if (ret) warning(_("Column %d: %s"), j+1, ret); // # nocov
    // TODO: but maxType should handle that and this should never warn
    ansloc += nrow;
  }
  Rprintf("Done!\n\n");
  
  UNPROTECT(nprotect);  // ans, dimnames, coerced, thisElement
  return(ans);
}
