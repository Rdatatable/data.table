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
bool toCharacterFactorPOSIX(SEXP *thisCol, int *nprotect) {
  bool coerced = true;
  if (isFactor(*thisCol)) {
    *thisCol = PROTECT(asCharacterFactor(*thisCol)); (*nprotect)++;
  } else if (INHERITS(*thisCol, char_ITime)) {
    *thisCol = asCharacterITime(*thisCol, nprotect);
  } else if (INHERITS(*thisCol, char_nanotime)) {
    *thisCol = callRfun1("format.nanotime", "nanotime", *thisCol, nprotect);
  } else if (INHERITS(*thisCol, char_Date)) {
    *thisCol = callRfun1("format.Date", "base", *thisCol, nprotect);
  } else if (INHERITS(*thisCol, char_POSIXct)) {
    *thisCol = callRfun1("format.POSIXct", "base", *thisCol, nprotect);
  } else if (INHERITS(*thisCol, char_POSIXlt)) {
    *thisCol = callRfun1("format.POSIXlt", "base", *thisCol, nprotect);
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
void preprocess(SEXP *dt, int *nprotect, int *maxType, int64_t *nrow, 
                int64_t *ncol, bool *coerce, bool *integer64, 
                bool *unpack, bool *recycle) {
  for (int j=0; j<xlength(*dt); ++j) {
    SEXP thisCol = VECTOR_ELT(*dt, j);
    int64_t thisnrow = xlength(thisCol);
    
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
      int64_t dims=xlength(colDim);
      if (dims > 2)
        error("Cannot unpack array column with %d dimensions", dims);
      thisnrow = INTEGER(colDim)[0];
      (*ncol) += INTEGER(colDim)[1]; 
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
      thisCol = callRfun1("[.ff", "ff", thisCol, nprotect); // nprotect incremented in callRfun1
      SET_VECTOR_ELT(*dt, j, thisCol); UNPROTECT(1); (*nprotect)--; // 'thisCol' now PROTECTED by dt
    }
    
    // do any mandatory coercions (e.g. factor to character)
    if (toCharacterFactorPOSIX(&thisCol, nprotect)) {
      SET_VECTOR_ELT(*dt, j, thisCol); UNPROTECT(1); (*nprotect)--; // 'thisCol' now PROTECTED by dt
    }
    
    // Determine the maximum type for matrix and whether we will need to do any
    // further column coercion across all columns (if thisType != maxType).
    int thisType = TYPEOF(thisCol);
    if (INHERITS(thisCol, char_integer64)) {
      if (*integer64) {
        // all columns already checked are integer64, nothing left to do here
      } else if (*ncol > 0 && !*integer64) {
        // This column is integer64, but at least one column checked already is not
        *coerce = true;
        *integer64 = true;
        if (*maxType != VECSXP && TYPEORDER(*maxType) > TYPEORDER(INTSXP))
          *maxType = STRSXP; // numeric and complex cannot be coerced to int64, must coerce to character
        else if (TYPEORDER(*maxType) < TYPEORDER(REALSXP))
          *maxType = REALSXP;
      } else if (*ncol == 0) {
        // This column is integer64 and it is the first column checked
        *integer64 = true;
        *maxType = REALSXP;
      } 
    } else if (thisType == *maxType) {
      // nothing to do, can continue to next column
    } else if (TYPEORDER(thisType)>TYPEORDER(VECSXP)) {
      // non-atomic non-list types are coerced / wrapped in list, see #4196
      *maxType=VECSXP;
      if (*ncol > 0)
        *coerce = true;
    } else if (TYPEORDER(thisType)>TYPEORDER(*maxType)) {
      // otherwise if this column is higher in typeorder list, set this type as maxType
      if (*ncol > 0) 
        *coerce = true;
      // if any previous column is integer64, then maxType must be STRSXP if any numeric or complex cols
      if (*integer64 && thisType != VECSXP && TYPEORDER(thisType) > TYPEORDER(INTSXP))
        *maxType = STRSXP;
      // if this column is RAWSXP and any previous column is not, then maxType must be STRSXP
      else if (*ncol > 0 && *maxType != VECSXP && thisType == RAWSXP)
        *maxType = STRSXP;
      // if maxType is RAWSXP and this type is not, then maxType must also be STRSXP
      else if (*ncol > 0 && *maxType == RAWSXP && thisType != RAWSXP)
        *maxType = STRSXP;
      else
        *maxType=thisType;
    } else if (*ncol > 0) {
      // TYPEORDER(thisType) < TYPEORDER(*maxType)
      // this col is different to previous so coercion required
      *coerce=true;
      // maxType remains the same unless thisType is RAWSXP, in which case we need to coerce to string
      if (*maxType != VECSXP && thisType == RAWSXP)
        *maxType = STRSXP;
    }
    (*ncol)++;
  }
}

// Make a vector of names of the format V1, V2, V3, ..., VN
SEXP makeNames(int64_t n, int *nprotect) {
  SEXP names = PROTECT(allocVector(STRSXP, n)); (*nprotect)++;
  
  // determine buffer width based on width of n
  size_t buffwidth = 2;
  for (int64_t ndiv = n; ndiv > 0; ndiv /= 10) buffwidth++; 
  char buff[buffwidth];
  
  // Make names 
  for (int64_t i = 0; i < n; ++i) {
    snprintf(buff, buffwidth, "V%"PRId64"", i+1);
    SET_STRING_ELT(names, i, mkChar(buff));
  }
  return(names);
}

// Add a prefix to a character vector, separated by a "."
// I.e. for concatenating names of a multi-dimensional column
SEXP prependNames(SEXP names, SEXP prefixArg, int *nprotect) {
  int64_t len = xlength(names);
  SEXP ans = PROTECT(allocVector(STRSXP, len)); (*nprotect)++;
  const char *prefix = CHAR(prefixArg);
  
  /* To determine the buffer width of the new column names, we need to 
   * know the maximum number of characters in any input column name as
   * well as the size of the prefix. */
  size_t prefixw = strlen(prefix); // nchar(prefix)
  size_t namesmaxw = 0; 
  for (int64_t i = 0; i < len; ++i) {
    const char *namei = CHAR(STRING_ELT(names, i));
    size_t namew = strlen(namei); // nchar(names[i])
    namesmaxw = namew > namesmaxw ? namew : namesmaxw; 
  }
  size_t buffwidth = prefixw + namesmaxw + 2; // size of both buffers + extra for separator and terminating \0
  char buff[buffwidth];
  
  // make new composite column name
  for (int64_t i = 0; i < len; ++i) {
    const char *namei = CHAR(STRING_ELT(names, i));
    snprintf(buff, buffwidth, "%s.%s", prefix, namei);
    SET_STRING_ELT(ans, i, mkChar(buff));
  }
  return(ans);
}

/* Recurse through a data.table, extracting any multi dimensional columns 
 * into their single columns in the top level data.table
 */
void flatten(SEXP *dt, SEXP *dtcn, SEXP *newdt, SEXP *newcn, int64_t *jtarget, int *nprotect) {
  for (int64_t j = 0; j < xlength(*dt); ++j) {
    SEXP thisCol = VECTOR_ELT(*dt, j);
    SEXP colDim = getAttrib(thisCol, R_DimSymbol);
    SEXP colName = STRING_ELT(*dtcn, j);
    if (isNull(thisCol)) {
      // Empty column, do not add to newdt
      continue;
    } else if (INHERITS(thisCol, char_dataframe) || INHERITS(thisCol, char_datatable)) {
      // This column is a data.table or data.frame we will recurse into
      // Make composite column names before recursing
      SEXP subdtcn = getAttrib(thisCol, R_NamesSymbol); // names(dt[,j])
      SEXP compositecn = prependNames(subdtcn, colName, nprotect); // nprotect incremented

      // recurse into data.table
      int recurseflattenstack = 0;
      flatten(&thisCol, &compositecn, newdt, newcn, jtarget, &recurseflattenstack);
      UNPROTECT(recurseflattenstack); // everything PROTECTED behind 'dt'
      UNPROTECT(1); (*nprotect)--; // no longer need to PROTECT compositecn, all elements now PROTECTED in dtcn
    } else if (!isNull(colDim)) {
      // matrix, we have to split up vector into new columns of length mat nrow
      int matnrow = INTEGER(colDim)[0];
      int matncol = INTEGER(colDim)[1];
      int mattype = TYPEOF(thisCol);
      SEXP matdm = getAttrib(thisCol, R_DimNamesSymbol); // dimnames(dt[,j])
      SEXP matcn; 
      
      // If no column names we need to make our own of the form V1, V2, ..., VN
      int matcnprotect = 0; // below may add 0 or 1 to protect stack
      if (isNull(matdm)) {
        matcn = makeNames(matncol, &matcnprotect); // matcnprotect incremented
      } else {
        matcn = STRING_ELT(matdm, 1);
        if (isNull(matcn)) {
          matcn = makeNames(matncol, &matcnprotect); // matcnprotect incremented
        }
      }

      // Make composite column names
      SEXP compositecn = prependNames(matcn, colName, nprotect); // nprotect incremented

      // Iterate through each column of the matrix, copying its contents into the new column vector
      for (int mj = 0; mj < matncol; ++mj) {
        SEXP thisNewCol = PROTECT(allocVector(mattype, matnrow)); (*nprotect)++;
        int64_t start = (int64_t) mj * matnrow;
        memrecycle(thisNewCol, R_NilValue, 0, matnrow, thisCol, start, matnrow, 0, "V1");
        SET_VECTOR_ELT(*newdt, *jtarget, thisNewCol); UNPROTECT(1); (*nprotect)--;
        SET_STRING_ELT(*newcn, *jtarget, STRING_ELT(compositecn, mj)); // add new column name
        (*jtarget)++; // Increment column index in the new flattened data.table
      }
      UNPROTECT(1); (*nprotect)--; // pop compositecn from protect stack, now PROTECTED by newcn
      UNPROTECT(matcnprotect); // pop any PROTECTED created matrix column names, now PROTECTED in newcn. 
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
  int nprotect=0;
  
  SEXP rncontainer = PROTECT(allocVector(VECSXP, 1)); nprotect++; // PROTECTED container to hold (maybe coerced) rownames 

  // Coerce and check rownames - do this first so rownames, if coerced,
  // is placed at the bottom of the protection stack. This way we don't
  // unintentionally unprotect it when poping the stack, e.g after coercing
  // a column and assigning back into the protected list container.
  if (!isNull(rownames)) {
    int rnstack = 0; // separate protect stack counter because number of coercions may be 0-3
    // If ff vector must be loaded into memory
    if (INHERITS(rownames, char_ff)) { 
      rownames = callRfun1("[.ff", "ff", rownames, &rnstack); // rnstack incremented in function
    }
    // Convert to string if factor, date, or time 
    toCharacterFactorPOSIX(&rownames, &rnstack);
    // if integer64 convert to character to avoid garbage numeric values
    if (INHERITS(rownames, char_integer64)) {
      rownames = asCharacterInteger64(rownames, &rnstack); // rnstack incremented in function
    }
    if (INHERITS(rownames, char_dataframe) || INHERITS(rownames, char_datatable) || 
        !isNull(getAttrib(rownames, R_DimSymbol)))
        error("Extracted rownames column or provided rownames.values are multi-column type (e.g. a matrix or data.table) and cannot be used as rownames");
    else if (TYPEOF(rownames) == VECSXP || TYPEOF(rownames) == LISTSXP)
      warning("Extracted rownames column or provided rownames.values are a list column, so will be converted to a character representation");
    SET_VECTOR_ELT(rncontainer, 0, rownames); UNPROTECT(rnstack); // coerced rownames now PROTECTED in rnContainer 
    rownames = VECTOR_ELT(rncontainer, 0);
  }
  
  // Extract column types and determine type to coerce to
  int preprocessstack=0;
  int maxType=LGLSXP;
  int64_t nrow=0;
  int64_t ncol=0; 
  bool coerce=false; // if no columns need coercing, can just use memcpy
  bool integer64=false; // are we coercing to integer64?
  bool unpack=false; // are there any columns to drop or unpack?
  bool recycle=false; // do any columns need recyling to nrow?
  preprocess(&dt, &preprocessstack, &maxType, &nrow, &ncol, &coerce, &integer64, &unpack, &recycle);
  
  // Check matrix is not larger than allowed by R
  if (ncol > R_LEN_T_MAX || nrow > R_LEN_T_MAX)
    error("R does not support matrices with more than %d columns or rows", R_LEN_T_MAX); // # nocov
  
  // Check rownames length for errors now we know nrow.
  if (!isNull(rownames) && nrow != 0 && xlength(rownames) != nrow) {
    error("Extracted rownames column or provided rownames.values do not match the number of rows in the matrix");
  }
  
  // If no columns, nrow is dictated by rownames
  if (ncol == 0) {
    nrow = isNull(rownames) ? 0 : xlength(rownames);
    maxType = LGLSXP; // empty matrix should be logical not raw
  }

  // allocate matrix
  SEXP ans = PROTECT(allocMatrix(maxType, nrow, ncol)); nprotect++; // should only be 'rnContainer' and 'ans' on PROTECT stack
  
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
  SEXP colnames = getAttrib(dt, R_NamesSymbol); // names(dt)
  if (isNull(colnames)) {
    colnames = makeNames(xlength(dt), &nprotect); // nprotect incremented
    setAttrib(dt, R_NamesSymbol, colnames); UNPROTECT(1); nprotect--; // PROTECTED by dt now
  }
  
  // Unpack data.table if needed
  if (unpack) {
    SEXP newdt = PROTECT(allocVector(VECSXP, ncol)); nprotect++;
    SEXP newcn = PROTECT(allocVector(STRSXP, ncol)); nprotect++;
    int64_t j = 0;
    int flprotect=0; // protect stack counter for flatten
    flatten(&dt, &colnames, &newdt, &newcn, &j, &flprotect);
    setAttrib(newdt, R_NamesSymbol, newcn);
    UNPROTECT(flprotect); // everything in flattened now PROTECTED in newdt
    UNPROTECT(1); nprotect--; // pop newcn PROTECT call, now PROTECTED by 'newdt' after setAttrib.
    dt = newdt;
    colnames = getAttrib(dt, R_NamesSymbol);
  }
  // top of PROTECT stack is either 'ans' or 'newdt' (if unpack was true)

  // Add dimension names
  SEXP dimnames = PROTECT(allocVector(VECSXP, 2)); nprotect++;
  SET_VECTOR_ELT(dimnames, 0, rownames);
  SET_VECTOR_ELT(dimnames, 1, colnames);
  setAttrib(ans, R_DimNamesSymbol, dimnames); UNPROTECT(1); nprotect--; // dimnames now PROTECTED by ans
  
  // If nrow 0 we can now return.
  if (nrow == 0) {
    UNPROTECT(nprotect);
    return(ans);
  }

  // recycle columns to match nrow if needed
  if (recycle) {
    for (int64_t j = 0; j < ncol; ++j) {
      SEXP thisCol = VECTOR_ELT(dt, j);
      int thisnrow = length(thisCol);
      if (thisnrow != nrow) {
        if (nrow % thisnrow != 0) { // nrow not divisible by thisnrow; error
          error("Could not recycle column of length %d into nrow of %d", thisnrow, nrow);
        }
        // Allocate new column to fill with memrecycle. Add integer64 class if needed.
        SEXP newCol = PROTECT(allocVector(TYPEOF(thisCol), nrow)); nprotect++;
        if (INHERITS(thisCol, char_integer64)) {
          SEXP i64Class = PROTECT(allocVector(STRSXP, 1)); nprotect++;
          SET_STRING_ELT(i64Class, 0, char_integer64);
          setAttrib(newCol, R_ClassSymbol, i64Class); UNPROTECT(1); nprotect--; // class PROTECTED in newCol
        }
        // Fill column repeating elements as needed
        for (int i = 0; i < (nrow / thisnrow); ++i) {
          memrecycle(newCol, R_NilValue, i*thisnrow, thisnrow, thisCol, 0, -1, 0, "V1");
        }
        SET_VECTOR_ELT(dt, j, newCol); UNPROTECT(1); nprotect--; // newCol PROTECTED in dt
      } 
    }
  }

  // Identify and coerce columns as needed
  if (coerce) {
    for (int j=0; j<ncol; ++j) {
      SEXP thisCol = VECTOR_ELT(dt, j);
      int thisprotect=0;
      if ((TYPEOF(thisCol) != maxType)  || (integer64 && !INHERITS(thisCol, char_integer64))) {
        SEXP coerced;
        if (maxType == VECSXP) { // coercion to list not handled by memrecycle.
          coerced = coerceAsList(thisCol, nrow, &thisprotect); // thisprotect incremented
        } else if (integer64 && maxType == STRSXP && INHERITS(thisCol, char_integer64)) {
          // memrecycle does not coerce integer64 to character
          coerced = asCharacterInteger64(thisCol, &thisprotect); // thisprotect incremented
        } else {
          // coerce with memrecycle. For memrecycle to be integer64 aware
          // we have to add the class to the target vector
          coerced = PROTECT(allocVector(maxType, nrow)); thisprotect++;
          if(integer64) {
            SEXP i64Class = PROTECT(allocVector(STRSXP, 1)); thisprotect++;
            SET_STRING_ELT(i64Class, 0, char_integer64);
            setAttrib(coerced, R_ClassSymbol, i64Class); UNPROTECT(1); thisprotect--; // class PROTECTED in coerced
          }
          const char *ret = memrecycle(coerced, R_NilValue, 0, nrow, thisCol, 0, -1, 0, "V1");
          // Warning when precision is lost after coercion, should not be possible to reach
          if (ret) error("Internal Error: column %d: %s\n", j+1, ret); // # nocov
        }
        SET_VECTOR_ELT(dt, j, coerced); UNPROTECT(thisprotect); // coerced PROTECTED in dt
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
  
  UNPROTECT(nprotect);
  return(ans);
}
