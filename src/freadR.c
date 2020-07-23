#include "fread.h"
#include "freadR.h"
#include "data.table.h"

/*****    TO DO    *****
Restore test 1339 (balanced embedded quotes, see ?fread already updated).
Confirm: http://stackoverflow.com/questions/23833294/data-tablefread-doesnt-like-missing-values-in-first-column
construct test and investigate skip for completeness here: http://stackoverflow.com/questions/22086780/data-table-fread-error
http://stackoverflow.com/questions/22229109/r-data-table-fread-command-how-to-read-large-files-with-irregular-separators
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
Just one "NA" current default but empty in future when numerics handle NA string variants directly.
---
Secondary separator for list() columns, such as columns 11 and 12 in BED (no need for strsplit).
*****/

#define NUT  NUMTYPE+2  // +1 for "numeric" alias for "double"; +1 for CLASS fallback using as.class() at R level afterwards

static int  typeSxp[NUT] =     {NILSXP,  LGLSXP,     LGLSXP,     LGLSXP,     LGLSXP,     INTSXP,    REALSXP,     REALSXP,    REALSXP,        REALSXP,        INTSXP,          REALSXP,         STRSXP,      REALSXP,    STRSXP   };
static char typeRName[NUT][10]={"NULL",  "logical",  "logical",  "logical",  "logical",  "integer", "integer64", "double",   "double",       "double",       "IDate",         "POSIXct",       "character", "numeric",  "CLASS"  };
static int  typeEnum[NUT] =    {CT_DROP, CT_BOOL8_N, CT_BOOL8_U, CT_BOOL8_T, CT_BOOL8_L, CT_INT32,  CT_INT64,    CT_FLOAT64, CT_FLOAT64_HEX, CT_FLOAT64_EXT, CT_ISO8601_DATE, CT_ISO8601_TIME, CT_STRING,   CT_FLOAT64, CT_STRING};
static colType readInt64As=CT_INT64;
static SEXP selectSxp;
static SEXP dropSxp;
static SEXP colClassesSxp;
static bool selectColClasses = false;
static cetype_t ienc = CE_NATIVE;
static SEXP RCHK;
static SEXP DT;
static SEXP colNamesSxp;
static SEXP colClassesAs; // the classes like factor, POSIXct which are currently done afterwards at R level: strings don't match typeRName above => NUT / "CLASS"
static SEXP selectRank;   // C level returns the column reording vector to be done by setcolorder() at R level afterwards
static int8_t *type;
static int8_t *size;
static int ncol = 0;
static int64_t dtnrows = 0;
static bool verbose = false;
static bool warningsAreErrors = false;
static bool oldNoDateTime = false;


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
  SEXP logical01Arg,

  // extras needed by callbacks from freadMain
  SEXP selectArg,
  SEXP dropArg,
  SEXP colClassesArg,
  SEXP integer64Arg,
  SEXP encodingArg,
  SEXP keepLeadingZerosArgs,
  SEXP noTZasUTC
) {
  verbose = LOGICAL(verboseArg)[0];
  warningsAreErrors = LOGICAL(warnings2errorsArg)[0];

  freadMainArgs args;
  ncol = 0;
  dtnrows = 0;
  const char *ch, *ch2;
  if (!isString(inputArg) || LENGTH(inputArg)!=1)
    error(_("Internal error: freadR input not a single character string: a filename or the data itself. Should have been caught at R level."));  // # nocov
  ch = ch2 = (const char *)CHAR(STRING_ELT(inputArg,0));
  while (*ch2!='\n' && *ch2!='\r' && *ch2!='\0') ch2++;
  args.input = (*ch2=='\0') ? R_ExpandFileName(ch) : ch; // for convenience so user doesn't have to call path.expand()

  ch = args.input;
  while (*ch!='\0' && *ch!='\n' && *ch!='\r') ch++;
  if (*ch!='\0' || args.input[0]=='\0') {
    if (verbose) DTPRINT(_("Input contains a \\n or is \")\". Taking this to be text input (not a filename)\n"));
    args.filename = NULL;
  } else {
    if (verbose) DTPRINT(_("Input contains no \\n. Taking this to be a filename to open\n"));
    args.filename = args.input;
    args.input = NULL;
  }

  if (!isString(sepArg) || LENGTH(sepArg)!=1 || strlen(CHAR(STRING_ELT(sepArg,0)))>1)
    error(_("Internal error: freadR sep not a single character. R level catches this."));  // # nocov
  args.sep = CHAR(STRING_ELT(sepArg,0))[0];   // '\0' when default "auto" was replaced by "" at R level

  if (!(isString(decArg) && LENGTH(decArg)==1 && strlen(CHAR(STRING_ELT(decArg,0)))==1))
    error(_("Internal error: freadR dec not a single character. R level catches this."));  // # nocov
  args.dec = CHAR(STRING_ELT(decArg,0))[0];

  if (IS_FALSE(quoteArg)) {
    args.quote = '\0';
  } else {
    if (!isString(quoteArg) || LENGTH(quoteArg)!=1 || strlen(CHAR(STRING_ELT(quoteArg,0))) > 1)
      error(_("quote= must be a single character, blank \"\", or FALSE"));
    args.quote = CHAR(STRING_ELT(quoteArg,0))[0];
  }

  // header is the only boolean where NA is valid and means 'auto'.
  // LOGICAL in R is signed 32 bits with NA_LOGICAL==INT_MIN, currently.
  args.header = false;
  if (LOGICAL(headerArg)[0]==NA_LOGICAL) args.header = NA_BOOL8;
  else if (LOGICAL(headerArg)[0]==TRUE) args.header = true;

  args.nrowLimit = INT64_MAX;
  // checked at R level
  if (isReal(nrowLimitArg)) {
    if (R_FINITE(REAL(nrowLimitArg)[0]) && REAL(nrowLimitArg)[0]>=0.0) args.nrowLimit = (int64_t)(REAL(nrowLimitArg)[0]);
  } else {
    if (INTEGER(nrowLimitArg)[0]>=1) args.nrowLimit = (int64_t)INTEGER(nrowLimitArg)[0];
  }

  args.logical01 = LOGICAL(logical01Arg)[0];
  {
    SEXP tt = PROTECT(GetOption(sym_old_fread_datetime_character, R_NilValue));
    args.oldNoDateTime = oldNoDateTime = isLogical(tt) && LENGTH(tt)==1 && LOGICAL(tt)[0]==TRUE;
    UNPROTECT(1);
  }
  args.skipNrow=-1;
  args.skipString=NULL;
  if (isString(skipArg)) {
    args.skipString = CHAR(STRING_ELT(skipArg,0));  // LENGTH==1 was checked at R level
  } else if (isInteger(skipArg)) {
    args.skipNrow = (int64_t)INTEGER(skipArg)[0];
  } else error(_("Internal error: skip not integer or string in freadR.c")); // # nocov

  if (!isNull(NAstringsArg) && !isString(NAstringsArg))
    error(_("Internal error: NAstringsArg is type '%s'. R level catches this"), type2char(TYPEOF(NAstringsArg)));  // # nocov
  int nnas = length(NAstringsArg);
  const char **NAstrings = (const char **)R_alloc((nnas + 1), sizeof(char*));  // +1 for the final NULL to save a separate nna variable
  for (int i=0; i<nnas; i++)
    NAstrings[i] = CHAR(STRING_ELT(NAstringsArg,i));
  NAstrings[nnas] = NULL;
  args.NAstrings = NAstrings;

  // here we use bool and rely on fread at R level to check these do not contain NA_LOGICAL
  args.stripWhite = LOGICAL(stripWhiteArg)[0];
  args.skipEmptyLines = LOGICAL(skipEmptyLinesArg)[0];
  args.fill = LOGICAL(fillArg)[0];
  args.showProgress = LOGICAL(showProgressArg)[0];
  if (INTEGER(nThreadArg)[0]<1) error(_("nThread(%d)<1"), INTEGER(nThreadArg)[0]);
  args.nth = (uint32_t)INTEGER(nThreadArg)[0];
  args.verbose = verbose;
  args.warningsAreErrors = warningsAreErrors;
  args.keepLeadingZeros = LOGICAL(keepLeadingZerosArgs)[0];
  args.noTZasUTC = LOGICAL(noTZasUTC)[0];

  // === extras used for callbacks ===
  if (!isString(integer64Arg) || LENGTH(integer64Arg)!=1) error(_("'integer64' must be a single character string"));
  const char *tt = CHAR(STRING_ELT(integer64Arg,0));
  if (strcmp(tt, "integer64")==0) {
    readInt64As = CT_INT64;
  } else if (strcmp(tt, "character")==0) {
    readInt64As = CT_STRING;
  } else if (strcmp(tt,"double")==0 || strcmp(tt,"numeric")==0) {
    readInt64As = CT_FLOAT64;
  } else STOP(_("Invalid value integer64='%s'. Must be 'integer64', 'character', 'double' or 'numeric'"), tt);

  colClassesSxp = colClassesArg;

  selectSxp = selectArg;
  dropSxp = dropArg;
  selectColClasses = false;
  if (!isNull(selectSxp)) {
    if (!isNull(dropSxp)) STOP(_("Use either select= or drop= but not both."));
    if (isNewList(selectArg)) {
      if (!isNull(colClassesSxp))
        STOP(_("select= is type list for specifying types in select=, but colClasses= has been provided as well. Please remove colClasses=."));
      if (!length(getAttrib(selectArg, R_NamesSymbol)))
        STOP(_("select= is type list but has no names; expecting list(type1=cols1, type2=cols2, ...)"));
      colClassesSxp = selectArg;
      selectColClasses = true;
      selectSxp = R_NilValue;
    } else {
      if (!isNull(getAttrib(selectArg, R_NamesSymbol))) {
        if (!isNull(colClassesSxp))
          STOP(_("select= is a named vector specifying the columns to select and their types, but colClasses= has been provided as well. Please remove colClasses=."));
        colClassesSxp = selectArg;
        selectSxp = getAttrib(selectArg, R_NamesSymbol);
        selectColClasses = true;
      }
    }
  } else {
    if (TYPEOF(colClassesSxp)==VECSXP && !length(getAttrib(colClassesSxp, R_NamesSymbol)))
       STOP(_("colClasses is type list but has no names"));
  }

  // Encoding, #563: Borrowed from do_setencoding from base R
  // https://github.com/wch/r-source/blob/ca5348f0b5e3f3c2b24851d7aff02de5217465eb/src/main/util.c#L1115
  // Check for mkCharLenCE function to locate as to where where this is implemented.
  tt = CHAR(STRING_ELT(encodingArg, 0));
  if (strcmp(tt, "unknown")==0) ienc = CE_NATIVE;
  else if (strcmp(tt, "Latin-1")==0) ienc = CE_LATIN1;
  else if (strcmp(tt, "UTF-8")==0) ienc = CE_UTF8;
  else STOP(_("encoding='%s' invalid. Must be 'unknown', 'Latin-1' or 'UTF-8'"), tt);  // # nocov
  // === end extras ===

  RCHK = PROTECT(allocVector(VECSXP, 4));
  // see kalibera/rchk#9 and Rdatatable/data.table#2865.  To avoid rchk false positives.
  // allocateDT() assigns DT to position 0. userOverride() assigns colNamesSxp to position 1 and colClassesAs to position 2 (both used in allocateDT())
  freadMain(args);
  UNPROTECT(1);
  return DT;
}

static void applyDrop(SEXP items, int8_t *type, int ncol, int dropSource) {
  if (!length(items)) return;
  SEXP itemsInt = PROTECT( isString(items) ? chmatch(items, colNamesSxp, NA_INTEGER) : coerceVector(items, INTSXP) );
  const int *itemsD = INTEGER(itemsInt), n=LENGTH(itemsInt);
  for (int j=0; j<n; ++j) {
    int k = itemsD[j];
    if (k==NA_INTEGER || k<1 || k>ncol) {
      static char buff[51];
      if (dropSource==-1) snprintf(buff, 50, "drop[%d]", j+1);
      else snprintf(buff, 50, "colClasses[[%d]][%d]", dropSource+1, j+1);
      if (k==NA_INTEGER) {
        if (isString(items))
          DTWARN(_("Column name '%s' (%s) not found"), CHAR(STRING_ELT(items, j)), buff);
        else
          DTWARN(_("%s is NA"), buff);
      } else {
        DTWARN(_("%s is %d which is out of range [1,ncol=%d]"), buff, k, ncol);
      }
    } else {
      type[k-1] = CT_DROP;
      // aside: dropping the same column several times is acceptable with no warning. This could arise via duplicates in the drop= vector,
      // or specifying the same column to drop using NULLs in colClasses and drop= too.
    }
  }
  UNPROTECT(1);
}

bool userOverride(int8_t *type, lenOff *colNames, const char *anchor, const int ncol)
{
  // use typeSize superfluously to avoid not-used warning; otherwise could move typeSize from fread.h into fread.c
  if (typeSize[CT_BOOL8_N]!=1) STOP(_("Internal error: typeSize[CT_BOOL8_N] != 1")); // # nocov
  if (typeSize[CT_STRING]!=8) STOP(_("Internal error: typeSize[CT_STRING] != 1")); // # nocov
  colNamesSxp = R_NilValue;
  SET_VECTOR_ELT(RCHK, 1, colNamesSxp=allocVector(STRSXP, ncol));
  for (int i=0; i<ncol; i++) {
    SEXP elem;
    if (colNames==NULL || colNames[i].len<=0) {
      char buff[12];
      snprintf(buff,12,"V%d",i+1);
      elem = mkChar(buff);  // no PROTECT as passed immediately to SET_STRING_ELT
    } else {
      elem = mkCharLenCE(anchor+colNames[i].off, colNames[i].len, ienc);  // no PROTECT as passed immediately to SET_STRING_ELT
    }
    SET_STRING_ELT(colNamesSxp, i, elem);
  }
  // "use either select= or drop= but not both" was checked earlier in freadR
  applyDrop(dropSxp, type, ncol, /*dropSource=*/-1);
  if (TYPEOF(colClassesSxp)==VECSXP) {  // not isNewList() because that returns true for NULL
    SEXP listNames = PROTECT(getAttrib(colClassesSxp, R_NamesSymbol));  // rchk wanted this protected
    for (int i=0; i<LENGTH(colClassesSxp); ++i) {
      if (STRING_ELT(listNames, i) == char_NULL) {
        SEXP items = VECTOR_ELT(colClassesSxp,i);
        applyDrop(items, type, ncol, /*dropSource=*/i);
      }
    }
    UNPROTECT(1);  // listNames
  }
  selectRank = NULL;
  const int *selectInts = NULL; // if select is provided this will point to 1-based ints of the column numbers (which might already be the input as-is)
  int nprotect = 0;  // just used for select; other protects are specifically balanced within loops to save the protection stack, whereas select is long-lived or no-alloc.
  if (length(selectSxp)) {
    const int n = length(selectSxp);
    if (isString(selectSxp)) {
      selectInts = INTEGER(PROTECT(chmatch(selectSxp, colNamesSxp, NA_INTEGER))); nprotect++;
      for (int i=0; i<n; ++i) if (selectInts[i]==NA_INTEGER)
        DTWARN(_("Column name '%s' not found in column name header (case sensitive), skipping."), CHAR(STRING_ELT(selectSxp, i)));
    } else {
      if (!isInteger(selectSxp)) { selectSxp=PROTECT(coerceVector(selectSxp, INTSXP)); nprotect++; }  // coerce numeric to int
      selectInts = INTEGER(selectSxp);
    }
    SET_VECTOR_ELT(RCHK, 3, selectRank=allocVector(INTSXP, ncol));
    int *selectRankD = INTEGER(selectRank), rank = 1;
    for (int i=0; i<n; ++i) {
      int k = selectInts[i];
      if (k==NA_INTEGER) continue; // missing column name warned above and skipped
      if (k<0) STOP(_("Column number %d (select[%d]) is negative but should be in the range [1,ncol=%d]. Consider drop= for column exclusion."),k,i+1,ncol);
      if (k==0) STOP(_("select = 0 (select[%d]) has no meaning. All values of select should be in the range [1,ncol=%d]."),i+1,ncol);
      if (k>ncol) STOP(_("Column number %d (select[%d]) is too large for this table, which only has %d columns."),k,i+1,ncol);
      if (type[k-1]<0) STOP(_("Column number %d ('%s') has been selected twice by select="), k, CHAR(STRING_ELT(colNamesSxp,k-1)));
      type[k-1] *= -1; // detect and error on duplicates on all types without calling duplicated() at all
      selectRankD[k-1] = rank++;  // rank not i to skip missing column names
    }
    for (int i=0; i<ncol; ++i) {
      if (type[i]<0) type[i] *= -1;
      else type[i]=CT_DROP;
    }
  }
  colClassesAs = NULL;
  if (length(colClassesSxp)) {
    SEXP typeRName_sxp = PROTECT(allocVector(STRSXP, NUT));
    for (int i=0; i<NUT; i++) SET_STRING_ELT(typeRName_sxp, i, mkChar(typeRName[i]));
    if (oldNoDateTime) {
      // prevent colClasses="IDate"/"POSIXct" being recognized so that colClassesAs is assigned here ready for type massage after reading at R level; test 2150.14
      SET_STRING_ELT(typeRName_sxp, CT_ISO8601_DATE, R_BlankString);
      SET_STRING_ELT(typeRName_sxp, CT_ISO8601_TIME, R_BlankString);
    }
    SET_VECTOR_ELT(RCHK, 2, colClassesAs=allocVector(STRSXP, ncol));  // if any, this attached to the DT for R level to call as_ methods on
    if (isString(colClassesSxp)) {
      SEXP typeEnum_idx = PROTECT(chmatch(colClassesSxp, typeRName_sxp, NUT));
      if (selectColClasses==false) {
        if (LENGTH(colClassesSxp)!=ncol && LENGTH(colClassesSxp)!=1)
          STOP(_("colClasses= is an unnamed vector of types, length %d, but there are %d columns in the input. To specify types for a subset of columns, you can use "
                 "a named vector, list format, or specify types using select= instead of colClasses=. Please see examples in ?fread."), LENGTH(colClassesSxp), ncol);
        const int mask = LENGTH(colClassesSxp)==1 ? 0 : INT_MAX;  // to have one consistent loop/logic for the length-1 recycling case too; #4237
        for (int i=0; i<ncol; ++i) {
          if (type[i]==CT_DROP) continue;                    // user might have specified the type of all columns including those dropped with drop=
          const SEXP tt = STRING_ELT(colClassesSxp, i&mask); // mask recycles colClassesSxp when it's length-1
          if (tt==NA_STRING || tt==R_BlankString) continue;  // user is ok with inherent type for this column
          int w = INTEGER(typeEnum_idx)[i&mask];
          if (tt==char_POSIXct) {
            // from v1.13.0, POSIXct is a built in type, but if the built-in doesn't support (e.g. test 1743.25 has missing tzone) then we still dispatch to as.POSIXct afterwards
            if (type[i]!=CT_ISO8601_TIME) {
              type[i]=CT_STRING; // e.g. CT_ISO8601_DATE changed to character here so that as.POSIXct treats the date-only as local time in tests 1743.122 and 2150.11
              SET_STRING_ELT(colClassesAs, i, tt);
            }
          } else { 
            type[i] = typeEnum[w-1];                           // freadMain checks bump up only not down
            if (w==NUT) SET_STRING_ELT(colClassesAs, i, tt);
          }
        }
      } else { // selectColClasses==true
        if (!selectInts) STOP(_("Internal error: selectInts is NULL but selectColClasses is true"));
        const int n = length(colClassesSxp);
        if (length(selectSxp)!=n) STOP(_("Internal error: length(selectSxp)!=length(colClassesSxp) but selectColClasses is true"));
        for (int i=0; i<n; ++i) {
          SEXP tt = STRING_ELT(colClassesSxp,i);
          if (tt==NA_STRING || tt==R_BlankString) continue;
          int w = INTEGER(typeEnum_idx)[i];
          int y = selectInts[i];
          if (y==NA_INTEGER) continue;
          if (tt==char_POSIXct) {
            if (type[y-1]!=CT_ISO8601_TIME) {
              type[y-1]=CT_STRING;
              SET_STRING_ELT(colClassesAs, y-1, tt);
            }
          } else {
            type[y-1] = typeEnum[w-1];
            if (w==NUT) SET_STRING_ELT(colClassesAs, y-1, tt);
          }
        }
      }
      UNPROTECT(1); // typeEnum_idx
    } else {
      if (!isNewList(colClassesSxp)) STOP(_("colClasses is type '%s' but should be list or character"), type2char(TYPEOF(colClassesSxp)));
      SEXP listNames = PROTECT(getAttrib(colClassesSxp, R_NamesSymbol));  // rchk wanted this protected
      if (!length(listNames)) STOP(_("colClasses is type list but has no names"));
      SEXP typeEnum_idx = PROTECT(chmatch(listNames, typeRName_sxp, NUT));

      int *selectRankD = NULL, rank = 1;
      if (selectColClasses) {
        SET_VECTOR_ELT(RCHK, 3, selectRank=allocVector(INTSXP, ncol));  // column order changed in setFinalNRow
        selectRankD = INTEGER(selectRank);
      }

      for (int i=0; i<LENGTH(colClassesSxp); i++) {
        const int w = INTEGER(typeEnum_idx)[i];
        signed char thisType = typeEnum[w-1];
        if (thisType==CT_DROP) continue;  // was dealt with earlier above
        SEXP items = VECTOR_ELT(colClassesSxp,i);
        SEXP itemsInt;
        if (isString(items)) itemsInt = PROTECT(chmatch(items, colNamesSxp, NA_INTEGER));
        else                 itemsInt = PROTECT(coerceVector(items, INTSXP));
        // UNPROTECTed directly just after this for loop. No nprotect++ here is correct.
        for (int j=0; j<LENGTH(items); j++) {
          int k = INTEGER(itemsInt)[j];
          if (k==NA_INTEGER) {
            if (isString(items))
              DTWARN(_("Column name '%s' (colClasses[[%d]][%d]) not found"), CHAR(STRING_ELT(items, j)), i+1, j+1);
            else
              DTWARN(_("colClasses[[%d]][%d] is NA"), i+1, j+1);
          } else {
            if (k>=1 && k<=ncol) {
              if (type[k-1]<0)
                DTWARN(_("Column %d ('%s') appears more than once in colClasses. The second time is colClasses[[%d]][%d]."), k, CHAR(STRING_ELT(colNamesSxp,k-1)), i+1, j+1);
              else if (type[k-1]!=CT_DROP) {
                if (thisType==CT_ISO8601_TIME && type[k-1]!=CT_ISO8601_TIME) {
                  type[k-1] = -CT_STRING; // don't use in-built UTC parser, defer to character and as.POSIXct afterwards which reads in local time
                  SET_STRING_ELT(colClassesAs, k-1, STRING_ELT(listNames,i));
                } else {
                  type[k-1] = -thisType;     // freadMain checks bump up only not down.  Deliberately don't catch here to test freadMain; e.g. test 959
                  if (w==NUT) SET_STRING_ELT(colClassesAs, k-1, STRING_ELT(listNames,i));
                }
                if (selectRankD) selectRankD[k-1] = rank++;
              }
            } else {
              DTWARN(_("Column number %d (colClasses[[%d]][%d]) is out of range [1,ncol=%d]"), k, i+1, j+1, ncol);
            }
          }
        }
        UNPROTECT(1); // UNPROTECTing itemsInt inside loop to save protection stack
      }
      for (int i=0; i<ncol; i++) {
        if (type[i]<0) type[i] *= -1;                  // undo sign; was used to detect duplicates
        else if (selectColClasses) type[i] = CT_DROP;  // reading will proceed in order of columns in file; reorder happens afterwards at R level
      }
      UNPROTECT(2);  // listNames and typeEnum_idx
    }
    UNPROTECT(1);  // typeRName_sxp
  }
  UNPROTECT(nprotect);
  if (readInt64As != CT_INT64) {
    for (int i=0; i<ncol; i++) if (type[i]==CT_INT64) type[i] = readInt64As;
  }
  return true;
}


size_t allocateDT(int8_t *typeArg, int8_t *sizeArg, int ncolArg, int ndrop, size_t allocNrow) {
  // save inputs for use by pushBuffer
  size = sizeArg;
  type = typeArg;
  int newDT = (ncol == 0);
  if (newDT) {
    ncol = ncolArg;
    dtnrows = allocNrow;
    SET_VECTOR_ELT(RCHK, 0, DT=allocVector(VECSXP, ncol-ndrop));
    if (ndrop==0) {
      setAttrib(DT, R_NamesSymbol, colNamesSxp);  // colNames mkChar'd in userOverride step
      if (colClassesAs) setAttrib(DT, sym_colClassesAs, colClassesAs);
    } else {
      int nprotect = 0;
      SEXP tt, ss=R_NilValue;
      setAttrib(DT, R_NamesSymbol, tt=PROTECT(allocVector(STRSXP, ncol-ndrop))); nprotect++;
      if (colClassesAs) {
        setAttrib(DT, sym_colClassesAs, ss=PROTECT(allocVector(STRSXP, ncol-ndrop))); nprotect++;
      }
      for (int i=0,resi=0; i<ncol; i++) if (type[i]!=CT_DROP) {
        if (colClassesAs) SET_STRING_ELT(ss, resi, STRING_ELT(colClassesAs,i));
        SET_STRING_ELT(tt, resi++, STRING_ELT(colNamesSxp,i));
      }
      UNPROTECT(nprotect);
    }
    if (selectRank) {
      SEXP tt = PROTECT(allocVector(INTSXP, ncol-ndrop));
      int *ttD = INTEGER(tt), *rankD = INTEGER(selectRank), rank=1;
      for (int i=0; i<ncol; ++i) if (type[i]!=CT_DROP) ttD[ rankD[i]-1 ] = rank++;
      SET_VECTOR_ELT(RCHK, 3, selectRank = tt);
      // selectRank now holds the order not the rank (so its name is now misleading). setFinalNRow passes it to setcolorder
      // we can't change column order now because they might be reallocated in the reread
      UNPROTECT(1); // tt
    }
    colClassesAs = getAttrib(DT, sym_colClassesAs);
    bool none = true;
    const int n = length(colClassesAs);
    for (int i=0; i<n; ++i) if (STRING_ELT(colClassesAs,i) != R_BlankString) { none=false; break; }
    if (none) setAttrib(DT, sym_colClassesAs, R_NilValue);
    else if (selectRank) setAttrib(DT, sym_colClassesAs, subsetVector(colClassesAs, selectRank));  // reorder the colClassesAs
  }
  // TODO: move DT size calculation into a separate function (since the final size is different from the initial size anyways)
  size_t DTbytes = SIZEOF(DT)*(ncol-ndrop)*2; // the VECSXP and its column names (exclude global character cache usage)

  // For each column we could have one of the following cases:
  //   * if the DataTable is "new", then make a new vector
  //   * if the column's type has changed, then replace it with a new vector
  //     (however if column's type[i] is negative, then it means we're skipping
  //     the column in the rerun, and its type hasn't actually changed).
  //   * if dtnrows≠allocNrow and the column's type has not changed, then that
  //     column needs to be re-alloced (using growVector).
  //   * otherwise leave the column as-is.
  for (int i=0, resi=0; i<ncol; i++) {
    if (type[i] == CT_DROP) continue;
    SEXP col = VECTOR_ELT(DT, resi);
    int oldIsInt64 = newDT? 0 : INHERITS(col, char_integer64);
    int newIsInt64 = type[i] == CT_INT64;
    int typeChanged = (type[i] > 0) && (newDT || TYPEOF(col) != typeSxp[type[i]] || oldIsInt64 != newIsInt64);
    int nrowChanged = (allocNrow != dtnrows);
    if (typeChanged || nrowChanged) {
      SEXP thiscol = typeChanged ? allocVector(typeSxp[type[i]], allocNrow)  // no need to PROTECT, passed immediately to SET_VECTOR_ELT, see R-exts 5.9.1
                                 : growVector(col, allocNrow);
      SET_VECTOR_ELT(DT,resi,thiscol);
      if (type[i]==CT_INT64) {
        SEXP tt = PROTECT(ScalarString(char_integer64));
        setAttrib(thiscol, R_ClassSymbol, tt);
        UNPROTECT(1);
      } else if (type[i] == CT_ISO8601_DATE) {
        SEXP tt = PROTECT(allocVector(STRSXP, 2));
        SET_STRING_ELT(tt, 0, char_IDate);
        SET_STRING_ELT(tt, 1, char_Date);
        setAttrib(thiscol, R_ClassSymbol, tt);
        UNPROTECT(1);
      } else if (type[i] == CT_ISO8601_TIME) {
        SEXP tt = PROTECT(allocVector(STRSXP, 2));
        SET_STRING_ELT(tt, 0, char_POSIXct);
        SET_STRING_ELT(tt, 1, char_POSIXt);
        setAttrib(thiscol, R_ClassSymbol, tt);
        UNPROTECT(1);

        setAttrib(thiscol, sym_tzone, ScalarString(char_UTC)); // see news for v1.13.0
      }
      SET_TRUELENGTH(thiscol, allocNrow);
      DTbytes += SIZEOF(thiscol)*allocNrow;
    }
    resi++;
  }
  dtnrows = allocNrow;
  return DTbytes;
}


void setFinalNrow(size_t nrow) {
  if (selectRank) setcolorder(DT, selectRank);  // selectRank was changed to contain order (not rank) in allocateDT above
  if (length(DT)) {
    if (nrow == dtnrows)
      return;
    for (int i=0; i<LENGTH(DT); i++) {
      SETLENGTH(VECTOR_ELT(DT,i), nrow);  // TODO: realloc
      SET_TRUELENGTH(VECTOR_ELT(DT,i), nrow);
    }
  }
  R_FlushConsole(); // # 2481. Just a convenient place; nothing per se to do with setFinalNrow()
}


void pushBuffer(ThreadLocalFreadParsingContext *ctx)
{
  const void *buff8 = ctx->buff8;
  const void *buff4 = ctx->buff4;
  const void *buff1 = ctx->buff1;
  const char *anchor = ctx->anchor;
  int nRows = (int) ctx->nRows;
  size_t DTi = ctx->DTi;
  int rowSize8 = (int) ctx->rowSize8;
  int rowSize4 = (int) ctx->rowSize4;
  int rowSize1 = (int) ctx->rowSize1;
  int nStringCols = ctx->nStringCols;
  int nNonStringCols = ctx->nNonStringCols;

  // Do all the string columns first so as to minimize and concentrate the time inside the single critical.
  // While the string columns are happening other threads before me can be copying their non-string buffers to the
  // final DT and other threads after me can be filling their buffers too.
  // rowSize is passed in because it will be different (much smaller) on the reread covering any type exception columns
  // locals passed in on stack so openmp knows that no synchronization is required

  // the byte position of this column in the first row of the row-major buffer
  if (nStringCols) {
    #pragma omp critical
    {
      int off8 = 0;
      int cnt8 = rowSize8 / 8;
      lenOff *buff8_lenoffs = (lenOff*) buff8;
      for (int j=0, resj=-1, done=0; done<nStringCols && j<ncol; j++) {
        if (type[j] == CT_DROP) continue;
        resj++;
        if (type[j] == CT_STRING) {
          SEXP dest = VECTOR_ELT(DT, resj);
          lenOff *source = buff8_lenoffs + off8;
          for (int i=0; i<nRows; i++) {
            int strLen = source->len;
            if (strLen<=0) {
              // stringLen == INT_MIN => NA, otherwise not a NAstring was checked inside fread_mean
              if (strLen<0) SET_STRING_ELT(dest, DTi+i, NA_STRING); // else leave the "" in place that was initialized by allocVector()
            } else {
              const char *str = anchor + source->off;
              int c=0;
              while (c<strLen && str[c]) c++;
              if (c<strLen) {
                // embedded nul found; any at the beginning or the end of the field should have already been excluded but this will strip those too if present just in case
                char *last = (char *)str+c;    // obtain write access to (const char *)anchor;
                while (c<strLen) {
                  if (str[c]) *last++=str[c];  // cow page write: saves allocation and management of a temp that would need to thread-safe in future.
                  c++;                         //   This is only thread accessing this region. For non-mmap direct input nul are not possible (R would not have accepted nul earlier).
                }
                strLen = last-str;
              }
              SET_STRING_ELT(dest, DTi+i, mkCharLenCE(str, strLen, ienc));
            }
            source += cnt8;
          }
          done++; // if just one string col near the start, don't loop over the other 10,000 cols. TODO? start on first too
        }
        off8 += (size[j] == 8);
      }
    }
  }

  int off1 = 0, off4 = 0, off8 = 0;
  for (int j=0, resj=-1, done=0; done<nNonStringCols && j<ncol; j++) {
    if (type[j]==CT_DROP) continue;
    int thisSize = size[j];
    resj++;
    if (type[j]!=CT_STRING && type[j]>0) {
      if (thisSize == 8) {
        double *dest = (double *)REAL(VECTOR_ELT(DT, resj)) + DTi;
        const char *src8 = (char*)buff8 + off8;
        for (int i=0; i<nRows; ++i) {
          *dest = *(double *)src8;
          src8 += rowSize8;
          dest++;
        }
      } else
      if (thisSize == 4) {
        int *dest = (int *)INTEGER(VECTOR_ELT(DT, resj)) + DTi;
        const char *src4 = (char*)buff4 + off4;
        // debug line for #3369 ... if (DTi>2638000) printf("freadR.c:460: thisSize==4, resj=%d, %"PRIu64", %d, %d, j=%d, done=%d\n", resj, (uint64_t)DTi, off4, rowSize4, j, done);
        for (int i=0; i<nRows; ++i) {
          *dest = *(int *)src4;
          src4 += rowSize4;
          dest++;
        }
      } else
      if (thisSize == 1) {
        if (type[j] > CT_BOOL8_L) STOP(_("Field size is 1 but the field is of type %d\n"), type[j]);
        Rboolean *dest = (Rboolean *)LOGICAL(VECTOR_ELT(DT, resj)) + DTi;
        const char *src1 = (char*)buff1 + off1;
        for (int i=0; i<nRows; ++i) {
          int8_t v = *(int8_t *)src1;
          *dest = (v==INT8_MIN ? NA_INTEGER : v);
          src1 += rowSize1;
          dest++;
        }
      } else STOP(_("Internal error: unexpected field of size %d\n"), thisSize);  // # nocov
      done++;
    }
    off8 += (size[j] & 8);
    off4 += (size[j] & 4);
    off1 += (size[j] & 1);
  }
}

// # nocov start
void progress(int p, int eta) {
  // called from thread 0 only
  // p between 0 and 100
  // eta in seconds
  // Initialized the first time it is called with p>0
  // Must be called at the end with p==100 to finish off and reset
  // If it's called twice at the end with p=100, that's ok

  // REprinf to avoid Rprintf's call to R_CheckUserInterrupt() every 100 lines, issue #2457
  // It's the R_CheckUserInterrupt() that has caused crashes before when called from OpenMP parallel region
  // even when called only from master thread. Update: can now retry within critical.
  // fwrite.c has some comments about how it might be possible to call R_CheckUserInterrupt() here so that
  // a long running fread can be stopped by user with Ctrl-C (or ESC on Windows).
  // Could try R_ProcessEvents() too as per
  // https://cran.r-project.org/bin/windows/base/rw-FAQ.html#The-console-freezes-when-my-compiled-code-is-running

  // No use of \r to avoid bug in RStudio, linked in the same issue #2457
  static int displayed = -1;  // -1 means not yet displayed, otherwise [0,50] '=' are displayed
  static char bar[] = "================================================== ";  // 50 marks for each 2%
  if (displayed==-1) {
    if (eta<3 || p>50) return;
    #pragma omp critical
    {
      REprintf("|--------------------------------------------------|\n|");
      R_FlushConsole();
    }
    displayed = 0;
  }
  p/=2;
  int toPrint = p-displayed;
  if (toPrint==0) return;
  bar[toPrint] = '\0';
  #pragma omp critical
  {
    REprintf("%s", bar);
    bar[toPrint] = '=';
    displayed = p;
    if (p==50) {
      REprintf("|\n");
      displayed = -1;
    }
    R_FlushConsole();
  }
}
// # nocov end

void __halt(bool warn, const char *format, ...) {
  // Solves: http://stackoverflow.com/questions/18597123/fread-data-table-locks-files
  // TODO: always include fnam in the STOP message. For log files etc.
  va_list args;
  va_start(args, format);
  char msg[2000];
  vsnprintf(msg, 2000, format, args);
  va_end(args);
  freadCleanup(); // this closes mmp hence why we just copied substrings from mmp to msg[] first since mmp is now invalid
  // if (warn) warning(_("%s"), msg);
  //   this warning() call doesn't seem to honor warn=2 straight away in R 3.6, so now always call error() directly to be sure
  //   we were going via warning() before to get the (converted from warning) prefix in the message (which we could mimic in future)
  error(_("%s"), msg); // include "%s" because data in msg might include '%'
}

void prepareThreadContext(ThreadLocalFreadParsingContext *ctx) {}
void postprocessBuffer(ThreadLocalFreadParsingContext *ctx) {}
void orderBuffer(ThreadLocalFreadParsingContext *ctx) {}
void freeThreadContext(ThreadLocalFreadParsingContext *ctx) {}
