#include "data.table.h"

/* Fills the allocated matrix (ans) with columns of the input data.table
 * (dt) using memcpy assuming all columns have the same SEXPTYPE. This
 * macro means we don't have to repeat the loop below for each SEXPTYPE.
 * RFUN and CTYPE should be a pair (e.g. INTEGER and int for INTSXP, or
 * REAL and double for REALSXP).
 */
#undef OMP_MEMCPY
#define OMP_MEMCPY(RFUN, CTYPE) {{                                  \
  if (nrow == 1) { /* faster to just assign directly if n=1 */      \
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

/* For STRSXP and VECSXP we need to iterate through every element,
 * extracting using STRING_ELT or VECTOR_ELT, then assing using
 * SET_STRING_ELT or SET_VECTOR_ELT. These are not thread safe, so 
 * can't use OMP.
 */
#undef VECCPY
#define VECCPY(GETTER, SETTER) {{                              \
  int64_t ansloc=0;                                            \
  for (int j = 0; j<ncol; ++j) {                               \
    for (int i = 0; i<nrow; ++i) {                             \
      SETTER(ans, ansloc, GETTER(VECTOR_ELT(dt, j), i));       \
      ansloc++;                                                \
    }                                                          \
  }                                                            \
} break; }                                                     \


/* If the vector is a factor or POSIX-like it is coerced to its string 
 * representation and replaced by reference.
 */
bool toCharacterFactorPOSIX(SEXP *thisCol, int64_t *nprotect) {
  bool coerced = true;
  if (isFactor(*thisCol)) {
    *thisCol = PROTECT(asCharacterFactor(*thisCol)); (*nprotect)++;
  } else if (INHERITS(*thisCol, char_ITime)) {
    *thisCol = PROTECT(asCharacterITime(*thisCol)); (*nprotect)++;
  } else if (INHERITS(*thisCol, char_nanotime)) {
    *thisCol = PROTECT(callRfun1("format.nanotime", "nanotime", *thisCol)); (*nprotect)++;
  } else if (INHERITS(*thisCol, char_Date)) {
    *thisCol = PROTECT(callRfun1("format.Date", "base", *thisCol)); (*nprotect)++;
  } else if (INHERITS(*thisCol, char_POSIXct)) {
    *thisCol = PROTECT(callRfun1("format.POSIXct", "base", *thisCol)); (*nprotect)++;
  } else if (INHERITS(*thisCol, char_POSIXlt)) {
    *thisCol = PROTECT(callRfun1("format.POSIXlt", "base", *thisCol)); (*nprotect)++;
  } else {
    coerced = false;
  }
  return(coerced);
}

/* Preprocess the input data.table:
 * 
 *  - Determine whether there are any NULL columns to drop
 *  - Determine whether there are any multi-dimensional columns to unpack
 *  - Determine whether there are columns of varying length where we will need to recycle
 *  - Load any FF columns into memory (using coerceIfMandatory function above)
 *  - Coearce and factor or POSIX-like columns to character vectors (using coerceIfMandatory function above)
 *  - Determine the target type of the matrix (maxType)
 *  - Determine whether column types are heterogeneous in which case we will later need to coerce to maxType.
 *  - Determine whether any or all columns are integer64 class
 *
 * Where there are data.table or data.frame columns this function recurses.
 */
void preprocess(SEXP *dt, int64_t *nprotect, int *maxType, R_xlen_t *nrow, 
                R_xlen_t *ncol, bool *coerce, bool *integer64, 
                bool *unpack, bool *recycle) {
  for (int j=0; j<xlength(*dt); ++j) {
    SEXP thisCol = VECTOR_ELT(*dt, j);
    R_xlen_t thisnrow = xlength(thisCol);
    
    // Check for empty columns or multi-dimension columns, which mean we will have to unpack and flatten
    SEXP colDim = getAttrib(thisCol, R_DimSymbol);
    if (isNull(thisCol)) {
      // NULL columns will be dropped
      *unpack=true;
      continue; // no other checks relevant, move to next column in dt
    } else if (INHERITS(thisCol, char_dataframe) || INHERITS(thisCol, char_datatable)) {
      // column is a data.table or data.frame, recurse
      *unpack=true;
      preprocess(&thisCol, nprotect, maxType, nrow, ncol, coerce, integer64, unpack, recycle);
      continue; // no other checks needed after recurse, iterate to next column in dt
    } else if (!isNull(colDim)) {
      // matrix or multidimensional array
      *unpack=true;
      R_xlen_t dims=xlength(colDim);
      if (dims > 2)
        error("Cannot unpack array column with %d dimensions", dims);
      thisnrow = INTEGER(colDim)[0];
      (*ncol) += INTEGER(colDim)[1]; 
    } else {
      (*ncol)++;
    }
    
    // Check for nrow mismatch
    if (*ncol == 0) {
      *nrow = thisnrow;
    } else if (thisnrow != *nrow) {
      *nrow = *nrow > thisnrow ? *nrow : thisnrow;
      *recycle = true; // can't determine whether recyclable until we know max nrow
    }
    
    // Load ff column into memory
    if (INHERITS(thisCol, char_ff)) {
      thisCol = PROTECT(callRfun1("[.ff", "ff", thisCol)); (*nprotect)++;
      SET_VECTOR_ELT(*dt, j, thisCol);
    }
    
    // do any mandatory coercions (e.g. factor to character)
    if (toCharacterFactorPOSIX(&thisCol, nprotect)) {
      SET_VECTOR_ELT(*dt, j, thisCol);
    }
    
    // Determine the maximum type for matrix and whether we will need to do any
    // further column coercion across all columns (if thisType != maxType).
    int thisType = TYPEOF(thisCol);
    if (INHERITS(thisCol, char_integer64)) {
      if (integer64) {
        // all columns already checked are integer64, nothing left to do here
      } else if (INHERITS(thisCol, char_integer64) && *ncol > 0 && !integer64) {
        // This column is integer64, but at least one column checked already is not
        *coerce = true;
        *integer64 = true;
        if (*maxType != VECSXP && TYPEORDER(*maxType) > TYPEORDER(INTSXP))
          *maxType = STRSXP; // numeric and complex cannot be coerced to int64, must coerce to character
        else if (TYPEORDER(*maxType) < TYPEORDER(REALSXP))
          *maxType = REALSXP;
      } else if (INHERITS(thisCol, char_integer64) && *ncol == 0) {
        // This column is integer64 and it is the first column checked
        *integer64 = true;
        *maxType = REALSXP;
      } 
    } else if (thisType == *maxType) {
      // nothing to do, can continue to next column
    } else if (TYPEORDER(thisType)>TYPEORDER(VECSXP)) {
      // non-atomic non-list types are coerced / wrapped in list, see #4196
      *coerce = true;
      *maxType=VECSXP;
    } else if (TYPEORDER(thisType)>TYPEORDER(*maxType)) {
      // otherwise if this column is higher in typeorder list, set this type as maxType
      if (j > 0) 
        *coerce = true;
      *maxType=thisType;
    } else {
      // TYPEORDER(thisType) < TYPEORDER(*maxType), 
      // no change to maxType, but this col is different to previous so coercion required
      *coerce=true;
    }
  }
}

// Make a vector of names of the format V1, V2, V3, ..., VN
SEXP makeNames(R_xlen_t n) {
  SEXP names = PROTECT(allocVector(STRSXP, n)); 
  
  // determine buffer width based on width of n
  size_t buffwidth = 2;
  for (R_xlen_t ndiv = n; ndiv > 0; ndiv /= 10) buffwidth++; 
  char buff[buffwidth];
  
  // Make names 
  for (R_xlen_t i = 0; i < n; ++i) {
    snprintf(buff, buffwidth, "V%"PRId64"", i+1);
    SET_STRING_ELT(names, i, mkChar(buff));
  }
  UNPROTECT(1);
  return(names);
}

// Add a prefix to a character vector, separated by a "."
// I.e. for concatenating names of a multi-dimensional column
SEXP prependNames(SEXP names, SEXP prefixArg) {
  R_xlen_t len = xlength(names);
  SEXP ans = PROTECT(allocVector(STRSXP, len));
  const char *prefix = CHAR(prefixArg);
  
  /* To determine the buffer width of the new column names, we need to 
   * know the maximum number of characters in any input column name as
   * well as the size of the prefix. */
  size_t prefixw = strlen(prefix); // nchar(prefix)
  size_t namesmaxw = 0; 
  for (R_xlen_t i = 0; i < len; ++i) {
    const char *namei = CHAR(STRING_ELT(names, i));
    size_t namew = strlen(namei); // nchar(names[i])
    namesmaxw = namew > namesmaxw ? namew : namesmaxw; 
  }
  size_t buffwidth = prefixw + namesmaxw + 2; // size of both buffers + extra for separator and terminating \0
  char buff[buffwidth];
  
  // make new composite column name
  for (R_xlen_t i = 0; i < len; ++i) {
    const char *namei = CHAR(STRING_ELT(names, i));
    snprintf(buff, buffwidth, "%s.%s", prefix, namei);
    SET_STRING_ELT(ans, i, mkChar(buff));
  }
  UNPROTECT(1);
  return(ans);
}

/* Recurse through a data.table, extracting any multi dimensional columns 
 * into their single columns in the top level data.table
 */
void flatten(SEXP *dt, SEXP *newdt, SEXP *newcn, R_xlen_t *jtarget, int64_t *nprotect) {
  SEXP dtcn = getAttrib(*dt, R_NamesSymbol); // names(dt)
  for (R_xlen_t j = 0; j < xlength(*dt); ++j) {
    SEXP thisCol = VECTOR_ELT(*dt, j);
    SEXP colDim = getAttrib(thisCol, R_DimSymbol);
    SEXP colName = STRING_ELT(dtcn, j);
    if (isNull(thisCol)) {
      // Empty column, do not add to newdt
      continue;
    } else if (INHERITS(thisCol, char_dataframe) || INHERITS(thisCol, char_datatable)) {
      // This column is a data.table or data.frame we will recurse into
      // Make composite column names before recursing
      SEXP subdtcn = getAttrib(thisCol, R_NamesSymbol); // names(dt[,j])
      SEXP compositecn = PROTECT(prependNames(subdtcn, colName)); (*nprotect)++;
      setAttrib(thisCol, R_NamesSymbol, compositecn);

      // recurse into data.table
      flatten(&thisCol, newdt, newcn, jtarget, nprotect);
    } else if (!isNull(colDim)) {
      // matrix, we have to split up vector into new columns of length mat nrow
      int matnrow = INTEGER(colDim)[0];
      int matncol = INTEGER(colDim)[1];
      int mattype = TYPEOF(thisCol);
      SEXP matdm = getAttrib(thisCol, R_DimNamesSymbol); // dimnames(dt[,j])
      SEXP matcn; 
      
      // If no column names we need to make our own of the form V1, V2, ..., VN
      if (isNull(matdm)) {
        matcn = PROTECT(makeNames(matncol)); (*nprotect)++;
      } else {
        matcn = STRING_ELT(matdm, 1);
        if (isNull(matcn)) {
          matcn = PROTECT(makeNames(matncol)); (*nprotect)++;
        }
      }

      // Make composite column names
      SEXP compositecn = PROTECT(prependNames(matcn, colName)); (*nprotect)++;

      // Iterate through each column of the matrix, copying its contents into the new column vector
      for (int mj = 0; mj < matncol; ++mj) {
        SEXP thisNewCol = PROTECT(allocVector(mattype, matnrow)); (*nprotect)++;
        R_xlen_t start = (R_xlen_t) mj * matnrow;
        memrecycle(thisNewCol, R_NilValue, 0, matnrow, thisCol, start, matnrow, 0, "V1");
        SET_VECTOR_ELT(*newdt, *jtarget, thisNewCol);
        SET_STRING_ELT(*newcn, *jtarget, STRING_ELT(compositecn, mj)); // add new column name
        (*jtarget)++; // Increment column index in the new flattened data.table
      }
    } else {
      // Single column, add pointer to new flattend data.table in the right spot
      SET_VECTOR_ELT(*newdt, *jtarget, thisCol);
      SET_STRING_ELT(*newcn, *jtarget, colName); // Add column name
      (*jtarget)++; // Increment column index in the new flattened data.table
    } 
  }
}

SEXP asmatrix(SEXP dt, SEXP rownames)
{
  // PROTECT / UNPROTECT stack counter
  int64_t nprotect=0;
  
  // Extract column types and determine type to coerce to
  int maxType=RAWSXP;
  R_xlen_t nrow=0;
  R_xlen_t ncol=0; 
  bool coerce=false; // if no columns need coercing, can just use memcpy
  bool integer64=false; // are we coercing to integer64?
  bool unpack=false; // are there any columns to drop or unpack?
  bool recycle=false; // do any columns need recyling to nrow?
  preprocess(&dt, &nprotect, &maxType, &nrow, &ncol, &coerce, &integer64, &unpack, &recycle);
  
  // Check matrix is not larger than allowed by R
  if (ncol > R_LEN_T_MAX || nrow > R_LEN_T_MAX) 
    error("R does not support matrices with more than %d columns or rows", R_LEN_T_MAX); // # nocov
  
  // Coerce and check rownames
  if (!isNull(rownames)) {
    // If ff vector must be loaded into memory
    if (INHERITS(rownames, char_ff)) { 
      rownames = PROTECT(callRfun1("[.ff", "ff", rownames)); nprotect++;
    }
    // Convert to string if factor, date, or time 
    toCharacterFactorPOSIX(&rownames, &nprotect);
    // if integer64 convert to character to avoid garbage numeric values
    if (INHERITS(rownames, char_integer64)) {
      rownames = PROTECT(asCharacterInteger64(rownames)); nprotect++;
    }
    if (TYPEOF(rownames) == VECSXP || TYPEOF(rownames) == LISTSXP)
      warning("Extracted rownames column or provided rownames.values are a list column, so will be converted to a character representation");
    if (!isNull(getAttrib(rownames, R_DimSymbol)))
      error("Extracted rownames column or provided rownames.values are multi-column type (e.g. a matrix or data.table) and cannot be used as rownames");
    if (nrow != 0 && xlength(rownames) != nrow)
      error("Extracted rownames column or provided rownames.values do not match the number of rows in the matrix");
  }
  
  // If no columns, nrow is dictated by rownames
  if (ncol == 0) {
    nrow = isNull(rownames) ? 0 : xlength(rownames);
    maxType = LGLSXP; // empty matrix should be logical not raw
  }

  // allocate matrix
  SEXP ans = PROTECT(allocMatrix(maxType, nrow, ncol)); nprotect++;
  
  // if ncol == 0 we can now return
  if (ncol == 0) {
    if (!isNull(rownames)) { // Add rownames if they exist
      SEXP dimnames = PROTECT(allocVector(VECSXP, 2)); nprotect++;
      SET_VECTOR_ELT(dimnames, 0, rownames);
      SET_VECTOR_ELT(dimnames, 1, R_NilValue);
      setAttrib(ans, R_DimNamesSymbol, dimnames);
    }
    UNPROTECT(nprotect);
    return(ans);
  }
  
  // if, somehow, the input data.table lacks names, make them
  SEXP cn = getAttrib(dt, R_NamesSymbol); // names(dt)
  if (isNull(cn)) {
    cn = PROTECT(makeNames(xlength(dt))); nprotect++;
    setAttrib(dt, R_NamesSymbol, cn);
  }
  
  // Unpack data.table if needed
  if (unpack) {
    SEXP newdt = PROTECT(allocVector(VECSXP, ncol)); nprotect++;
    SEXP newcn = PROTECT(allocVector(STRSXP, ncol)); nprotect++;
    R_xlen_t j = 0;
    flatten(&dt, &newdt, &newcn, &j, &nprotect);
    setAttrib(newdt, R_NamesSymbol, newcn);
    dt = newdt;
  }
  
  // Add dimension names
  SEXP dimnames = PROTECT(allocVector(VECSXP, 2)); nprotect++;
  SET_VECTOR_ELT(dimnames, 0, rownames);
  SET_VECTOR_ELT(dimnames, 1, getAttrib(dt, R_NamesSymbol));
  setAttrib(ans, R_DimNamesSymbol, dimnames);
  
  // If nrow 0 we can now return.
  if (nrow == 0) {
    UNPROTECT(nprotect);
    return(ans);
  }
  
  // Identify and coerce columns as needed
  if (coerce) {
    for (int j=0; j<ncol; ++j) {
      SEXP thisCol = VECTOR_ELT(dt, j);
      if ((TYPEOF(thisCol) != maxType)  || (integer64 && !INHERITS(thisCol, char_integer64))) {
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
