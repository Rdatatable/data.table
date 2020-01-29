#include "data.table.h"

SEXP asmatrix(SEXP dt, SEXP rownames)
{
  // PROTECT / UNPROTECT stack counter
  int nprotect=0;
  
  // Determine rows and colums
  int ncol = length(dt);
  int nrow = length(VECTOR_ELT(dt, 0));
  
  // Extract column types and determine type to coerce to
  int maxType=RAWSXP;
  bool coerce=false; // if no columns need coercing, can just use memcpy
  bool integer64=false; // are we coercing to integer64?
  for (int j=0; j<ncol; ++j) {
    // Extract column type. 
    SEXP thisCol = VECTOR_ELT(dt, j);
    int thisType = TYPEOF(thisCol);

    // Determine the maximum type now that we have inspected this column
    if (INHERITS(thisCol, char_integer64)) {
      // If integer64 defer coercion til after we know maxType of other columns
      if (j > 0 && !integer64) coerce=true;
      integer64=true;
    } else if (TYPEORDER(thisType)>TYPEORDER(VECSXP)) {
      // non-atomic non-list types are coerced / wrapped in list, see #4196
      if (j > 0) 
        coerce = true;
      maxType=VECSXP;
    } else if (TYPEORDER(thisType)>TYPEORDER(maxType)) {
      // otherwise if this column is higher in typeorder list, set this type as maxType
      if (j > 0) 
        coerce = true;
      maxType=thisType;
    } else if (integer64) {
      coerce=true; // earlier column is integer64 but this one isn't
    } else if (TYPEORDER(thisType)!=TYPEORDER(maxType)) {
      coerce=true; // no change to maxType, but this col is different to previous so coercion required
    }
  }
  
  // Determine type to coerce to based on presence of integer64 columns and maxType.
  if (integer64) {
    if (TYPEORDER(maxType)<TYPEORDER(REALSXP))
      // raw, logical, and integer coerced to integer64
      maxType = REALSXP; // integer64 is REALSXP with class integer64
    else if (TYPEORDER(maxType)<TYPEORDER(STRSXP))
      // if numeric or complex, all need to be coerced to STRSXP
      maxType = STRSXP;
    // else maxType is VECSXP, so no coercion needed.
  }
  
  // allocate matrix
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
  
  // for memrecycle to be integer64 aware we need to add integer64 class to ans
  SEXP matClass = PROTECT(getAttrib(ans, R_ClassSymbol)); nprotect++;
  if (integer64 && maxType == REALSXP) {
    SEXP i64Class = PROTECT(allocVector(STRSXP, 1)); nprotect++;
    SET_STRING_ELT(i64Class, 0, char_integer64);
    setAttrib(ans, R_ClassSymbol, i64Class);
  }
  
  // If no coercion needed, we can simply loop through without checking and copy.
  // I.e. optimise for usual case of calling as.matrix on a large data.table 
  // where all columns are already the same time
  if (!coerce) {
    if (nrow > 1 && TYPEORDER(maxType) < TYPEORDER(STRSXP)) {
      int64_t ansloc=0;
      switch(maxType) {
        case RAWSXP: {
          for (int j=0; j<ncol; ++j) {
            memcpy(RAW(ans)+ansloc, RAW(VECTOR_ELT(dt, j)), nrow*sizeof(Rbyte));
            ansloc+=nrow;
          }
          break;
        } case LGLSXP: {
          for (int j=0; j<ncol; ++j) {
            memcpy(LOGICAL(ans)+ansloc, LOGICAL(VECTOR_ELT(dt, j)), nrow*sizeof(Rboolean));
            ansloc+=nrow;
          }
          break;        
        } case INTSXP: {
          for (int j=0; j<ncol; ++j) {
            memcpy(INTEGER(ans)+ansloc, INTEGER(VECTOR_ELT(dt, j)), nrow*sizeof(int));
            ansloc+=nrow;
          }
          break;         
        } case REALSXP: {
          for (int j=0; j<ncol; ++j) {
            memcpy(REAL(ans)+ansloc, REAL(VECTOR_ELT(dt, j)), nrow*sizeof(double));
            ansloc+=nrow;
          }
          break;   
        } case CPLXSXP: {
          for (int j=0; j<ncol; ++j) {
            memcpy(COMPLEX(ans)+ansloc, COMPLEX(VECTOR_ELT(dt, j)), nrow*sizeof(Rcomplex));
            ansloc+=nrow;
          }
          break;
        } default: {
          error("Internal Error: unreachable state in as.matrix with nrow > 1\n"); // # nocov
        }
      }
    } else if (maxType == STRSXP) {
      int64_t ansloc=0;
      for (int j = 0; j<ncol; ++j) {
        for (int i = 0; i<nrow; ++i) {
          SET_STRING_ELT(ans, ansloc, STRING_ELT(VECTOR_ELT(dt, j), i));
          ansloc++;
        }
      }
    } else if (maxType == VECSXP) {
      int64_t ansloc=0;
      for (int j = 0; j<ncol; ++j) {
        for (int i = 0; i<nrow; ++i) {
          SET_VECTOR_ELT(ans, ansloc, VECTOR_ELT(VECTOR_ELT(dt, j), i));
          ansloc++;
        }
      }
    } else {
      // faster to assign than to memcpy single element
      switch(maxType) {
        case RAWSXP: {
          for (int j=0; j<ncol; ++j) RAW(ans)[j] = RAW(VECTOR_ELT(dt, j))[0];
          break;
        } case LGLSXP: {
          for (int j=0; j<ncol; ++j) LOGICAL(ans)[j] = LOGICAL(VECTOR_ELT(dt, j))[0];
          break;        
        } case INTSXP: {
          for (int j=0; j<ncol; ++j) INTEGER(ans)[j] = INTEGER(VECTOR_ELT(dt, j))[0];
          break;         
        } case REALSXP: {
          for (int j=0; j<ncol; ++j) REAL(ans)[j] = REAL(VECTOR_ELT(dt, j))[0];
          break;   
        } case CPLXSXP: {
          for (int j=0; j<ncol; ++j) COMPLEX(ans)[j] = COMPLEX(VECTOR_ELT(dt, j))[0];
          break;
        } default: {
          error("Internal Error: unreachable state in as.matrix with nrow == 1\n"); // # nocov
        }
      }
    }
  } else {
    // Coerce columns where needed and fill
    SEXP coerced;
    int64_t ansloc=0; // position in vector to start copying to, filling by column.
    for (int j=0; j<ncol; ++j) {
      SEXP thisCol = VECTOR_ELT(dt, j);
      if (maxType == VECSXP) { // coercion to list not handled by memrecycle.
        coerced = PROTECT(coerceAsList(thisCol, nrow)); nprotect++;
      } else if (integer64 && maxType == STRSXP && INHERITS(thisCol, char_integer64)) {
        // memrecycle does not coerce integer64 to character
        coerced = PROTECT(asCharacterInteger64(thisCol)); nprotect++;
      } else {
        coerced = thisCol; // type coercion handled by memrecycle
      }
      
      // Fill matrix with memrecycle
      const char *ret = memrecycle(ans, R_NilValue, ansloc, nrow, coerced, 0, -1, 0, "V1");
      // Warning when precision is lost after coercion, should not be possible to reach
      if (ret) warning(_("Column %d: %s"), j+1, ret); // # nocov
      // TODO: but maxType should handle that and this should never warn
      ansloc += nrow;
    }
  }
  
  // Reset class - matrices do not have class information
  if (integer64 && maxType == REALSXP) {
    setAttrib(ans, R_ClassSymbol, matClass);
  }
    
  UNPROTECT(nprotect);  // ans, dimnames, coerced, thisElement
  return(ans);
}
