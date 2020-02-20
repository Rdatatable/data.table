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
    // Extract this column for checks
    SEXP thisCol = VECTOR_ELT(*dt, j);
    int64_t thisnrow; // how many rows does this column occupy?
    int thisType; // what is the type of this column?
    bool thisI64; // does this column have class integer64?

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
      
      // How many rows does this column have?
      thisnrow = INTEGER(colDim)[0];
      
      // if first column initialize maxType and nrow.
      if (*ncol == 0) {
        *maxType = TYPEOF(thisCol);
        *nrow = thisnrow;
      } 
      // If not the first column, compare number of rows to nrow to see if we need to recycle
      else if (thisnrow != *nrow) {
        *nrow = *nrow > thisnrow ? *nrow : thisnrow;
        *recycle = true; // can't determine whether recyclable until we know max nrow
      }
      
      // increment the column counter by the number of columns in the matrix
      (*ncol) += INTEGER(colDim)[1]; 
      
      continue; // no more checks for matrixcolumns
    }
    // From here on, we're always dealing with a column that is a vector/list (doesn't have dimensions)
    
    // Load ff column into memory
    if (INHERITS(thisCol, char_ff)) {
      thisCol = callRfun1("[.ff", "ff", thisCol, nprotect); // nprotect incremented in callRfun1
      SET_VECTOR_ELT(*dt, j, thisCol); UNPROTECT(1); (*nprotect)--; // 'thisCol' now PROTECTED by dt
    }
    
    // Coerce factor and date/time to character (always happens regardless of target matrix type)
    if (toCharacterFactorPOSIX(&thisCol, nprotect)) {
      SET_VECTOR_ELT(*dt, j, thisCol); UNPROTECT(1); (*nprotect)--; // 'thisCol' now PROTECTED by dt
    }
    
    // Now we can determine length and type of this column
    thisnrow = xlength(thisCol);
    thisType = TYPEOF(thisCol);
    thisI64 = INHERITS(thisCol, char_integer64);
    
    // If this is the first column we use it to initialize maxType and nrow
    if (*ncol == 0) {
      // initialize nrow and increment column counter
      *nrow = thisnrow;
      (*ncol)++; 
      
      // initialize maxType to type of column
      if (INHERITS(thisCol, char_integer64)) {
        *integer64 = true;
        *maxType = REALSXP;
      } else {
        *maxType = thisType;
      }
      
      continue; // no more checks required on first column
    }
    // For remaining columns we need to check for nrow mismatch and compare column type to maxType
    
    // Increment column counter
    (*ncol)++;
    
    // Check for nrow mismatch
    if (thisnrow != *nrow) {
      *nrow = *nrow > thisnrow ? *nrow : thisnrow;
      *recycle = true; // can't determine whether recyclable until we know max nrow
    }
  
    // Compare column type to maxType to determine whether maxType needs to change and later column coercion is required
    if (TYPEORDER(thisType)>TYPEORDER(VECSXP)) {
      // Non-atomic non-list types are wrapped in a list, see #4196
      *maxType=VECSXP;
      *coerce = true;
    } else if (thisType == VECSXP) {
      // This column is a list. In this case, all columns become wrapped in list
      // If the maxType isn't already VECSXP, then a previous column is not a list,
      // so we need to set the maxType and let asmatrix know we need to coerce these 
      // columns later.
      if (*maxType != VECSXP) {
        *maxType = VECSXP;
        *coerce = true;
      }
    } else if (thisType == RAWSXP) {
      // raw is handled specially: matrix can only be raw type if all columns are raw
      // otherwise columns are coerced to character, or if maxType is VECSXP, then each
      // element of the raw column becomes an element of a list.
      if (*maxType != RAWSXP && *maxType != VECSXP) {
        *maxType = STRSXP;
        *coerce = true;
      }
    } else if (*maxType == RAWSXP && thisType != RAWSXP) {
      // a previous column is raw, but this column is a non-raw vector. Must coerce to STRSXP.
      *coerce = true;
      *maxType = STRSXP;
      
      // if this column is integer64 we also need to flag that there is at least one integer64 column
      if (thisI64)
        *integer64 = true;
      
    } else if (thisI64) {
      // This column has class "integer64" (type is REALSXP). Only integer and logical columns 
      // can be coerced to integer64. Integer64 columns cannot be coerced to numeric or complex,
      // so if maxType is either of these, then the maxType becomes STRSXP (integer64 coerced to
      // character vector). 
      if (!*integer64) { 
        // This column is integer64, but at least one column checked already is not
        *coerce = true;
        *integer64 = true;
        if (*maxType == REALSXP || *maxType == CPLXSXP)
          *maxType = STRSXP; // integer64 cannot be coerced to numeric or complex, must coerce to character
        else if (*maxType == LGLSXP || *maxType == INTSXP)
          *maxType = REALSXP; // logical and integer can be coerced to integer64
        // else *maxType == VECSXP, in which case no change to maxType and integer64 column elements wrapped in list
      }
    }  else if (TYPEORDER(thisType)>TYPEORDER(*maxType)) {
      // the type of this column is higher in the typeorder list than maxType, 
      // so we need to change maxType and flag that we will need to do column coercion
      *coerce = true;
      
      // if any previous column is integer64, then maxType must be STRSXP if this column is numeric or complex
      if (*integer64 && (thisType == REALSXP || thisType == CPLXSXP))
        *maxType = STRSXP;
      // otherwise the type of this column becomes the maxType
      else 
        *maxType=thisType;
      
    } else if (TYPEORDER(thisType)<TYPEORDER(*maxType)) {
      // this column is different to a previous column, but lower in the typeorder list so coercion required
      *coerce=true;
    }
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
  int maxType=LGLSXP; // initialised to type of first column by preprocess(), LGLSXP if empty matrix
  int64_t nrow=0; // initialised to length of first column by preprocess(), 0 rows if emtpy matrix
  int64_t ncol=0;  // Incremented by preprocess() based on columns encountered (some may be NULL or multi-column)
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
  if (ncol == 0)
    nrow = isNull(rownames) ? 0 : xlength(rownames);

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
      int64_t thisnrow = xlength(thisCol);
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
