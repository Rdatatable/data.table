#include "fread.h"
#include "data.table.h"

/*****    TO DO    *****
Restore test 1339 (balanced embedded quotes, see ?fread already updated).
Confirm: http://stackoverflow.com/questions/23833294/data-tablefread-doesnt-like-missing-values-in-first-column
construct test and investigate skip for completeness here: http://stackoverflow.com/questions/22086780/data-table-fread-error
http://stackoverflow.com/questions/22229109/r-data-table-fread-command-how-to-read-large-files-with-irregular-separators
http://r.789695.n4.nabble.com/Odd-problem-using-fread-to-read-in-a-csv-file-no-data-just-headers-tp4686302.html
And even more diagnostics to verbose=true so we can see where crashes are.
Detect and coerce dates and times. By searching for - and :, and dateTtime etc, or R's own method or fasttime. POSIXct default, for microseconds? : http://stackoverflow.com/questions/14056370/cast-string-to-idatetime
Add as.colClasses to fread.R after return from C level (e.g. for colClasses "Date", although as slow as read.csv via character)
Allow comment char to ignore. Important in format detection. But require valid line data before comment character in the read loop? See http://stackoverflow.com/a/18922269/403310
Deal with row.names e.g. http://stackoverflow.com/questions/15448732/reading-csv-with-row-names-by-fread
Test Garrett's two files again (wrap around ,,,,,, and different row lengths that the wc -l now fixes)
Post from patricknik on 5 Jan re ""b"" in a field. And Aykut Firat on email.
Save repeated ch<eof checking in main read step. Last line might still be tricky if last line has no eol.
Test using at least "grep read.table ...Rtrunk/tests/
Look for any non-alpha-numeric characters in the output and try each of them. That way can handle bell character as well and save testing separators which aren't there.
Column all 0 and 1 treated as logical?
Just one "NA" current default but empty in future when numerics handle NA string variants directly.
---
Secondary separator for list() columns, such as columns 11 and 12 in BED (no need for strsplit).
Add LaF comparison.
as.read.table=true/false option.  Or fread.table and fread.csv (see http://r.789695.n4.nabble.com/New-function-fread-in-v1-8-7-tp4653745p4654194.html).
*****/

#define NUT  NUMTYPE+2  // +1 for "numeric" alias for "double"; +1 for CLASS fallback using as.class() at R level afterwards

static int  typeSxp[NUT] =     { NILSXP, LGLSXP,   INTSXP,        INTSXP,        REALSXP,    REALSXP,   STRSXP,     REALSXP,   STRSXP   };
static char typeRName[NUT][10]={"drop", "logical","integer",     "integer",     "integer64","double",  "character","numeric", "CLASS"   };
static int  typeEnum[NUT] =    { CT_DROP,CT_BOOL8, CT_INT32_BARE, CT_INT32_FULL, CT_INT64,   CT_FLOAT64,CT_STRING,  CT_FLOAT64,CT_STRING};
static colType readInt64As=CT_INT64;
static SEXP selectSxp;
static SEXP dropSxp;
static SEXP colNamesSxp;
static SEXP colClassesSxp;
static cetype_t ienc = CE_NATIVE;
static SEXP DT;
static int8_t *type;
static int8_t *size;
static int ncol;
static int protecti=0;
static _Bool verbose = 0;
static _Bool warningsAreErrors = 0;


SEXP freadR(
  // params passed to freadMain
  SEXP inputArg,
  SEXP sepArg,
  SEXP decArg,
  SEXP quoteArg,
  SEXP headerArg,
  SEXP nrowLimitArg,
  SEXP skipArg,
  SEXP NAstringsArg,
  SEXP stripWhiteArg,
  SEXP skipEmptyLinesArg,
  SEXP fillArg,
  SEXP showProgressArg,
  SEXP nThreadArg,
  SEXP verboseArg,
  SEXP warnings2errorsArg,

  // extras needed by callbacks from freadMain
  SEXP selectArg,
  SEXP dropArg,
  SEXP colClassesArg,
  SEXP integer64Arg,
  SEXP encodingArg
) {
  verbose = LOGICAL(verboseArg)[0];
  warningsAreErrors = LOGICAL(warnings2errorsArg)[0];

  freadMainArgs args;
  protecti=0;
  const char *ch, *ch2;
  if (!isString(inputArg) || LENGTH(inputArg)!=1)
    error("fread input must be a single character string: a filename or the data itself");
  ch = ch2 = (const char *)CHAR(STRING_ELT(inputArg,0));
  while (*ch2!='\n' && *ch2!='\0') ch2++;
  args.input = (*ch2=='\n') ? ch : R_ExpandFileName(ch); // for convenience so user doesn't have to call path.expand()

  ch = args.input;
  while (*ch!='\0' && *ch!='\n') ch++;
  if (*ch=='\n' || args.input[0]=='\0') {
    if (verbose) DTPRINT("Input contains a \\n (or is \"\"). Taking this to be text input (not a filename)\n");
    args.filename = NULL;
  } else {
    if (verbose) DTPRINT("Input contains no \\n. Taking this to be a filename to open\n");
    args.filename = args.input;
    args.input = NULL;
  }

  if (!isString(sepArg) || LENGTH(sepArg)!=1 || strlen(CHAR(STRING_ELT(sepArg,0)))>1)
    error("CfreadR: sep must be 'auto' or a single character ('\\n' is an acceptable single character)");
  args.sep = CHAR(STRING_ELT(sepArg,0))[0];   // '\0' when default "auto" was replaced by "" at R level

  if (!(isString(decArg) && LENGTH(decArg)==1 && strlen(CHAR(STRING_ELT(decArg,0)))==1))
    error("CfreadR: dec must be a single character such as '.' or ','");
  args.dec = CHAR(STRING_ELT(decArg,0))[0];

  if (!isString(quoteArg) || LENGTH(quoteArg)!=1 || strlen(CHAR(STRING_ELT(quoteArg,0))) > 1)
    error("CfreadR: quote must be a single character or empty \"\"");
  args.quote = CHAR(STRING_ELT(quoteArg,0))[0];

  // header is the only boolean where NA is valid and means 'auto'.
  // LOGICAL in R is signed 32 bits with NA_LOGICAL==INT_MIN, currently.
  args.header = FALSE;
  if (LOGICAL(headerArg)[0]==NA_LOGICAL) args.header = NA_BOOL8;
  else if (LOGICAL(headerArg)[0]==TRUE) args.header = TRUE;

  args.nrowLimit = INT64_MAX;
  // checked at R level
  if (isReal(nrowLimitArg)) {
    if (R_FINITE(REAL(nrowLimitArg)[0]) && REAL(nrowLimitArg)[0]>=0.0) args.nrowLimit = (int64_t)(REAL(nrowLimitArg)[0]);
  } else {
    if (INTEGER(nrowLimitArg)[0]>=0) args.nrowLimit = (int64_t)INTEGER(nrowLimitArg)[0];
  }

  args.skipNrow=0;
  args.skipString=NULL;
  if (isString(skipArg)) {
    args.skipString = CHAR(STRING_ELT(skipArg,0));  // LENGTH==1 was checked at R level
  } else if (isReal(skipArg)) {
    if (R_FINITE(REAL(skipArg)[0]) && REAL(skipArg)[0]>0.0) args.skipNrow = (uint64_t)REAL(skipArg)[0];
  } else if (isInteger(skipArg)) {
    if (INTEGER(skipArg)[0]>0) args.skipNrow = (uint64_t)INTEGER(skipArg)[0];
  } else error("skip must be a single positive numeric (integer or double), or a string to search for");

  if (!isNull(NAstringsArg) && !isString(NAstringsArg))
    error("'na.strings' is type '%s'.  Must be either NULL or a character vector.", type2char(TYPEOF(NAstringsArg)));
  int nnas = length(NAstringsArg);
  if (nnas>100)  // very conservative limit
    error("length(na.strings)==%d. This is too many to allocate pointers for on stack", nnas);
  const char **NAstrings = alloca((nnas + 1) * sizeof(char*));
  for (int i=0; i<nnas; i++)
    NAstrings[i] = CHAR(STRING_ELT(NAstringsArg,i));
  NAstrings[nnas] = NULL;
  args.NAstrings = NAstrings;


  // here we use _Bool and rely on fread at R level to check these do not contain NA_LOGICAL
  args.stripWhite = LOGICAL(stripWhiteArg)[0];
  args.skipEmptyLines = LOGICAL(skipEmptyLinesArg)[0];
  args.fill = LOGICAL(fillArg)[0];
  args.showProgress = LOGICAL(showProgressArg)[0];
  if (INTEGER(nThreadArg)[0]<1) error("nThread(%d)<1", INTEGER(nThreadArg)[0]);
  args.nth = (uint32_t)INTEGER(nThreadArg)[0];
  args.verbose = verbose;
  args.warningsAreErrors = warningsAreErrors;

  // === extras used for callbacks ===
  if (!isString(integer64Arg) || LENGTH(integer64Arg)!=1) error("'integer64' must be a single character string");
  const char *tt = CHAR(STRING_ELT(integer64Arg,0));
  if (strcmp(tt, "integer64")==0) {
    readInt64As = CT_INT64;
  } else if (strcmp(tt, "character")==0) {
    readInt64As = CT_STRING;
  } else if (strcmp(tt,"double")==0 || strcmp(tt,"numeric")==0) {
    readInt64As = CT_FLOAT64;
  } else STOP("Invalid value integer64='%s'. Must be 'integer64', 'character', 'double' or 'numeric'", tt);

  colClassesSxp = colClassesArg;   // checked inside userOverride where it is used.

  if (!isNull(selectArg) && !isNull(dropArg)) STOP("Use either select= or drop= but not both.");
  selectSxp = selectArg;
  dropSxp = dropArg;

  // Encoding, #563: Borrowed from do_setencoding from base R
  // https://github.com/wch/r-source/blob/ca5348f0b5e3f3c2b24851d7aff02de5217465eb/src/main/util.c#L1115
  // Check for mkCharLenCE function to locate as to where where this is implemented.
  tt = CHAR(STRING_ELT(encodingArg, 0));
  if (strcmp(tt, "unknown")==0) ienc = CE_NATIVE;
  else if (strcmp(tt, "Latin-1")==0) ienc = CE_LATIN1;
  else if (strcmp(tt, "UTF-8")==0) ienc = CE_UTF8;
  else STOP("encoding='%s' invalid. Must be 'unknown', 'Latin-1' or 'UTF-8'", tt);
  // === end extras ===

  DT = R_NilValue; // created by callback
  freadMain(args);
  UNPROTECT(protecti);
  return DT;
}


_Bool userOverride(int8_t *type, lenOff *colNames, const char *anchor, int ncol)
{
  // use typeSize superfluously to avoid not-used warning; otherwise could move typeSize from fread.h into fread.c
  if (typeSize[CT_BOOL8]!=1) STOP("Internal error: typeSize[CT_BOOL8] != 1");
  if (typeSize[CT_STRING]!=8) STOP("Internal error: typeSize[CT_STRING] != 1");
  colNamesSxp = NULL;
  if (colNames!=NULL) {
    colNamesSxp = PROTECT(allocVector(STRSXP, ncol));
    protecti++;
    for (int i=0; i<ncol; i++) {
      SEXP this;
      if (colNames[i].len<=0) {
        char buff[10];
        sprintf(buff,"V%d",i+1);
        this = mkChar(buff);
      } else {
        this = mkCharLenCE(anchor+colNames[i].off, colNames[i].len, ienc);
      }
      SET_STRING_ELT(colNamesSxp, i, this);
    }
  }
  if (length(colClassesSxp)) {
    SEXP typeRName_sxp = PROTECT(allocVector(STRSXP, NUT));
    protecti++;
    for (int i=0; i<NUT; i++) SET_STRING_ELT(typeRName_sxp, i, mkChar(typeRName[i]));
    if (isString(colClassesSxp)) {
      SEXP typeEnum_idx = PROTECT(chmatch(colClassesSxp, typeRName_sxp, NUT, FALSE));
      protecti++;
      if (LENGTH(colClassesSxp)==1) {
        signed char newType = typeEnum[INTEGER(typeEnum_idx)[0]-1];
        if (newType == CT_DROP) STOP("colClasses='drop' is not permitted; i.e. to drop all columns and load nothing");
        for (int i=0; i<ncol; i++) type[i]=newType;   // freadMain checks bump up only not down
      } else if (LENGTH(colClassesSxp)==ncol) {
        for (int i=0; i<ncol; i++) {
          if (STRING_ELT(colClassesSxp,i)==NA_STRING) continue; // user is ok with inherent type for this column
          type[i] = typeEnum[INTEGER(typeEnum_idx)[i]-1];
        }
      } else {
        STOP("colClasses is an unnamed character vector but its length is %d. Must be length 1 or ncol (%d in this case) when unnamed. To specify types for a subset of columns you can either name the items with the column names or pass list() format to colClasses using column names or column numbers. See examples in ?fread.",
              LENGTH(colClassesSxp), ncol);
      }
    } else {
      if (!isNewList(colClassesSxp)) STOP("CfreadR: colClasses is not type list");
      if (!length(getAttrib(colClassesSxp, R_NamesSymbol))) STOP("CfreadR: colClasses is type list but has no names");
      SEXP typeEnum_idx = PROTECT(chmatch(getAttrib(colClassesSxp, R_NamesSymbol), typeRName_sxp, NUT, FALSE));
      protecti++;
      for (int i=0; i<LENGTH(colClassesSxp); i++) {
        SEXP items;
        signed char thisType = typeEnum[INTEGER(typeEnum_idx)[i]-1];
        items = VECTOR_ELT(colClassesSxp,i);
        if (thisType == CT_DROP) {
          if (!isNull(dropSxp) || !isNull(selectSxp)) STOP("Can't use NULL in colClasses when select or drop is used as well.");
          dropSxp = items;
          continue;
        }
        SEXP itemsInt;
        if (isString(items)) itemsInt = PROTECT(chmatch(items, colNamesSxp, NA_INTEGER, FALSE));
        else itemsInt = PROTECT(coerceVector(items, INTSXP));
        protecti++;
        for (int j=0; j<LENGTH(items); j++) {
          int k = INTEGER(itemsInt)[j];
          if (k==NA_INTEGER) {
            if (isString(items)) STOP("Column name '%s' in colClasses[[%d]] not found", CHAR(STRING_ELT(items, j)),i+1);
            else STOP("colClasses[[%d]][%d] is NA", i+1, j+1);
          } else {
            if (k<1 || k>ncol) STOP("Column number %d (colClasses[[%d]][%d]) is out of range [1,ncol=%d]",k,i+1,j+1,ncol);
            k--;
            if (type[k]<0) STOP("Column '%s' appears more than once in colClasses", CHAR(STRING_ELT(colNamesSxp,k)));
            type[k] = -thisType;
            // freadMain checks bump up only not down.  Deliberately don't catch here to test freadMain; e.g. test 959
          }
        }
      }
      for (int i=0; i<ncol; i++) if (type[i]<0) type[i] *= -1;  // undo sign; was used to detect duplicates
    }
  }
  if (readInt64As != CT_INT64) {
    for (int i=0; i<ncol; i++) if (type[i]==CT_INT64) type[i] = readInt64As;
  }
  if (length(dropSxp)) {
    SEXP itemsInt;
    if (isString(dropSxp)) itemsInt = PROTECT(chmatch(dropSxp, colNamesSxp, NA_INTEGER, FALSE));
    else itemsInt = PROTECT(coerceVector(dropSxp, INTSXP));
    protecti++;
    for (int j=0; j<LENGTH(itemsInt); j++) {
      int k = INTEGER(itemsInt)[j];
      if (k==NA_INTEGER) {
        if (isString(dropSxp)) {
          DTWARN("Column name '%s' in 'drop' not found", CHAR(STRING_ELT(dropSxp, j)));
        } else {
          DTWARN("drop[%d] is NA", j+1);
        }
      } else {
        if (k<1 || k>ncol) {
          DTWARN("Column number %d (drop[%d]) is out of range [1,ncol=%d]",k,j+1,ncol);
        } else {
          if (type[k-1] == CT_DROP) STOP("Duplicates detected in drop");
          type[k-1] = CT_DROP;
        }
      }
    }
  } else if (length(selectSxp)) {
    SEXP tt;
    if (isString(selectSxp)) {
      // invalid cols check part of #1445 moved here (makes sense before reading the file)
      tt = PROTECT(chmatch(selectSxp, colNamesSxp, NA_INTEGER, FALSE));
      protecti++;
      for (int i=0; i<length(selectSxp); i++) if (INTEGER(tt)[i]==NA_INTEGER)
        DTWARN("Column name '%s' not found in column name header (case sensitive), skipping.", CHAR(STRING_ELT(selectSxp, i)));
    } else tt = selectSxp;
    for (int i=0; i<LENGTH(tt); i++) {
      int k = isInteger(tt) ? INTEGER(tt)[i] : (int)REAL(tt)[i];
      if (k == NA_INTEGER) continue;
      if (k<1 || k>ncol) STOP("Column number %d (select[%d]) is out of range [1,ncol=%d]",k,i+1,ncol);
      if (type[k-1]<0) STOP("Column number %d ('%s') has been selected twice by select=", k, STRING_ELT(colNames,k-1));
      type[k-1] *= -1; // detect and error on duplicates on all types without calling duplicated() at all
    }
    for (int i=0; i<ncol; i++) {
      if (type[i]<0) type[i] *= -1;
      else type[i]=CT_DROP;
    }
  }
  return TRUE;  // continue
}


size_t allocateDT(int8_t *typeArg, int8_t *sizeArg, int ncolArg, int ndrop, int64_t allocNrow) {
  // save inputs for use by pushBuffer
  ncol = ncolArg;
  size = sizeArg;
  type = typeArg;
  DT=PROTECT(allocVector(VECSXP,ncol-ndrop));  // safer to leave over allocation to alloc.col on return in fread.R
  protecti++;
  if (ndrop==0) {
    setAttrib(DT,R_NamesSymbol,colNamesSxp);  // colNames mkChar'd in userOverride step
  } else {
    SEXP tt;
    setAttrib(DT, R_NamesSymbol, tt = allocVector(STRSXP, ncol-ndrop));
    for (int i=0,resi=0; i<ncol; i++) if (type[i]!=CT_DROP) {
      SET_STRING_ELT(tt,resi++,STRING_ELT(colNamesSxp,i));
    }
  }
  size_t DTbytes = SIZEOF(DT)*(ncol-ndrop)*2; // the VECSXP and its column names (exclude global character cache usage)
  for (int i=0,resi=0; i<ncol; i++) {
    if (type[i] == CT_DROP) continue;
    SEXP thiscol = allocVector(typeSxp[ type[i] ], allocNrow);
    SET_VECTOR_ELT(DT,resi++,thiscol);     // no need to PROTECT thiscol, see R-exts 5.9.1
    if (type[i]==CT_INT64) setAttrib(thiscol, R_ClassSymbol, ScalarString(char_integer64));
    SET_TRUELENGTH(thiscol, allocNrow);
    DTbytes += SIZEOF(thiscol)*allocNrow;
  }
  return DTbytes;
}


void reallocColType(int col,  // which column of the result, not of type[]. (they are different when ndrop>0)
                    colType newType) {
  uint64_t nrow = length(VECTOR_ELT(DT,0));
  SEXP tt;
  SET_VECTOR_ELT(DT, col, tt=allocVector(typeSxp[ newType ], nrow));
  if (newType==CT_INT64) setAttrib(tt, R_ClassSymbol, ScalarString(char_integer64));
}


void setFinalNrow(int64_t nrow) {
  // TODO realloc
  if (length(DT)) {
    if (nrow == length(VECTOR_ELT(DT, 0)))
      return;
    for (int i=0; i<LENGTH(DT); i++) {
      SETLENGTH(VECTOR_ELT(DT,i), nrow);
      SET_TRUELENGTH(VECTOR_ELT(DT,i), nrow);
    }
  }
}


void pushBuffer(const void *buff, const char *anchor, int nRows, int64_t DTi,
                int rowSize, int nStringCols, int nNonStringCols)
{
  // Do all the string columns first so as to minimize and concentrate the time inside the single critical.
  // While the string columns are happening other threads before me can be copying their non-string buffers to the
  // final DT and other threads after me can be filling their buffers too.
  // rowSize is passed in because it will be different (much smaller) on the reread covering any type exception columns
  // locals passed in on stack so openmp knows that no synchonization is required

  int off = 0;   // the byte position of this column in the first row of the row-major buffer
  if (nStringCols) {
    #pragma omp critical
    {
      for (int j=0, resj=-1, done=0; done<nStringCols && j<ncol; j++) {
        if (type[j] == CT_DROP) continue;
        resj++;
        if (type[j] == CT_STRING) {
          SEXP dest = VECTOR_ELT(DT, resj);
          lenOff *source = (lenOff *)((char *)buff + off);
          for (int i=0; i<nRows; i++) {
            int strLen = source->len;
            if (strLen) {
              SEXP thisStr = strLen<0 ? NA_STRING : mkCharLenCE(anchor + source->off, strLen, ienc);
              // stringLen == INT_MIN => NA, otherwise not a NAstring was checked inside fread_mean
              SET_STRING_ELT(dest, DTi+i, thisStr);
            } // else dest was already initialized with R_BlankString by allocVector()
            source = (lenOff *)((char *)source + rowSize);
          }
          done++; // if just one string col near the start, don't loop over the other 10,000 cols. TODO? start on first too
        }
        off += size[j];
      }
    }
  }
  off = 0;
  for (int j=0, resj=-1, done=0; done<nNonStringCols && j<ncol; j++) {
    if (type[j]==CT_DROP) continue;
    resj++;
    if (type[j]!=CT_STRING && type[j]>0) {
      char *source = (char *)buff + off;
      if (type[j]!=CT_BOOL8) {
        int thisSize = size[j];
        char *dest = (char *)DATAPTR(VECTOR_ELT(DT, resj)) + DTi*thisSize;
        for (int i=0; i<nRows; i++) {
          memcpy(dest, source, thisSize);
          source += rowSize;
          dest += thisSize;
        }
      } else {
        Rboolean *dest = (Rboolean *)((char *)DATAPTR(VECTOR_ELT(DT, resj)) + DTi*sizeof(Rboolean));
        for (int i=0; i<nRows; i++) {
          int8_t v = *(int8_t *)source;
          *dest = (v==INT8_MIN ? NA_INTEGER : v);
          source += rowSize;
          dest++;
        }
      }
      done++;
    }
    off += size[j];
  }
}


void progress(double p, double eta) {
  Rprintf("\rRead %.0f%%. ETA %02d:%02d ", p, (int)eta/60, (int)eta%60);
  // See comments in fwrite.c for how we might in future be able to R_CheckUserInterrupt() here.
  //   ( Had crashes with R_CheckUserInterrupt() even when called only from master thread, to overcome. )
  #ifdef WIN32
  R_FlushConsole();
  // Could try R_ProcessEvents() too as per
  // https://cran.r-project.org/bin/windows/base/rw-FAQ.html#The-console-freezes-when-my-compiled-code-is-running
  #endif
}


void STOP(const char *format, ...) {
  // Solves: http://stackoverflow.com/questions/18597123/fread-data-table-locks-files
  // TODO: always include fnam in the STOP message. For log files etc.
  va_list args;
  va_start(args, format);
  char msg[2000];
  vsnprintf(msg, 2000, format, args);
  va_end(args);
  freadCleanup();
  error(msg);
}

void freadLastWarning(const char *format, ...) {
  va_list args;
  va_start(args, format);
  static char msg[2000];
  vsnprintf(msg, 2000, format, args);
  va_end(args);
  freadCleanup();
  warning(msg);
}
