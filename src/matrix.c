#include "data.table.h"

/* Fills the allocated matrix (ans) with columns of the input data.table
 * (dt) using memcpy assuming all columns have the same SEXPTYPE. 
 * Where nrow = 1 faster to just assign directly rather than use memcpy
 */
#undef OMP_MEMCPY
#define OMP_MEMCPY(RFUN, CTYPE) {{                                  \
  if (nrow == 1) {                                                  \
    for (int j=0; j<ncol; ++j) {                                    \
      RFUN(ans)[j] = RFUN(VECTOR_ELT(dt, j))[0];                    \
    }                                                               \
  } else {                                                          \
    CTYPE *pans = RFUN(ans);                                        \
    CTYPE **pcol;                                                   \
    pcol = (CTYPE **) R_alloc(ncol, sizeof(CTYPE *));               \
    for (int j=0; j<ncol; ++j) pcol[j] = RFUN(VECTOR_ELT(dt, j));   \
    _Pragma ("omp parallel for num_threads(getDTthreads())")        \
    for (int j=0; j<ncol; ++j) {                                    \
      int64_t ansloc = (int64_t)j*nrow;                             \
      memcpy(pans+ansloc, pcol[j], nrow*sizeof(CTYPE));             \
    }                                                               \
  }                                                                 \
} break; }

#undef VECCPY
#define VECCPY(GETTER, SETTER) {{                              \
  int64_t ansloc=0;                                            \
  for (int j = 0; j<ncol; ++j) {                               \
    for (int i = 0; i<nrow; ++i) {                             \
      SETTER(ans, ansloc, GETTER(VECTOR_ELT(dt, j), i));       \
      ansloc++;                                                \
    }                                                          \
  }                                                            \
} break; }
  

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
  
  // Identify and coerce columns as needed
  if (coerce) {
    for (int j=0; j<ncol; ++j) {
      SEXP thisCol = VECTOR_ELT(dt, j);
      if ((TYPEOF(thisCol) != maxType)  ||
          (integer64 && !INHERITS(thisCol, char_integer64))) {
        SEXP coerced;
        if (maxType == VECSXP) { // coercion to list not handled by memrecycle.
          coerced = PROTECT(coerceAsList(thisCol, nrow)); nprotect++;
        } else if (integer64 && maxType == STRSXP && INHERITS(thisCol, char_integer64)) {
          // memrecycle does not coerce integer64 to character
          coerced = PROTECT(asCharacterInteger64(thisCol)); nprotect++;
        } else {
          // coerce with memrecycle. For memrecycle to be integer64 aware
          // we have to add the class to the target vector
          coerced = PROTECT(allocVector(maxType, nrow));
          if(integer64) {
            SEXP i64Class = PROTECT(allocVector(STRSXP, 1)); nprotect++;
            SET_STRING_ELT(i64Class, 0, char_integer64);
            setAttrib(coerced, R_ClassSymbol, i64Class);
          }
          const char *ret = memrecycle(coerced, R_NilValue, 0, nrow, thisCol, 0, -1, 0, "V1");
          // Warning when precision is lost after coercion, should not be possible to reach
          if (ret) error("Internal Error: column %d: %s\n", j+1, ret); // # nocov
        }
        SET_VECTOR_ELT(dt, j, coerced);
      }
    }
  }
  
  // Copy columns into matrix, recycling memory
  switch(maxType) {
    case RAWSXP: OMP_MEMCPY(RAW, Rbyte);
    case LGLSXP: OMP_MEMCPY(LOGICAL, int);
    case INTSXP: OMP_MEMCPY(INTEGER, int);
    case REALSXP: OMP_MEMCPY(REAL, double);
    case CPLXSXP: OMP_MEMCPY(COMPLEX, Rcomplex);
    case STRSXP: VECCPY(STRING_ELT, SET_STRING_ELT);
    case VECSXP: VECCPY(VECTOR_ELT, SET_VECTOR_ELT);
    default:
      error("Internal Error: unreachable state in as.matrix\n"); // # nocov
  }
  
  UNPROTECT(nprotect);  // ans, dimnames, coerced, thisElement
  return(ans);
}
