
#include <stdbool.h>
#include "data.table.h"
#include "fwrite.h"

#define DATETIMEAS_EPOCH     2
#define DATETIMEAS_WRITECSV  3

static char sep2;                // '\0' if there are no list columns. Otherwise, the within-column separator.
static bool logical01=true;      // should logicals be written as 0|1 or true|false. Needed by list column writer too in case a cell is a logical vector.
static int dateTimeAs=0;         // 0=ISO(yyyy-mm-dd), 1=squash(yyyymmdd), 2=epoch, 3=write.csv
static const char *sep2start, *sep2end;
// sep2 is in main fwrite.c so that writeString can quote other fields if sep2 is present in them
// if there are no list columns, set sep2=='\0'

// Non-agnostic helpers ...

const char *getString(SEXP *col, int64_t row) {   // TODO: inline for use in fwrite.c
  SEXP x = col[row];
  return x==NA_STRING ? NULL : CHAR(x);
}

const char *getCategString(SEXP col, int64_t row) {
  // the only writer that needs to have the header of the SEXP column, to get to the levels
  int x = INTEGER(col)[row];
  return x==NA_INTEGER ? NULL : CHAR(STRING_ELT(getAttrib(col, R_LevelsSymbol), x-1));
}

writer_fun_t funs[] = {
  &writeBool8,
  &writeBool32,
  &writeBool32AsString,
  &writeInt32,
  &writeInt64,
  &writeFloat64,
  &writeITime,
  &writeDateInt32,
  &writeDateFloat64,
  &writePOSIXct,
  &writeNanotime,
  &writeString,
  &writeCategString,
  &writeList
};

typedef enum {   // same order as fun[] above
  WF_Bool8,
  WF_Bool32,
  WF_Bool32AsString,
  WF_Int32,
  WF_Int64,
  WF_Float64,
  WF_ITime,
  WF_DateInt32,
  WF_DateFloat64,
  WF_POSIXct,
  WF_Nanotime,
  WF_String,
  WF_CategString,
  WF_List
} WFs;

static int32_t whichWriter(SEXP);

void writeList(SEXP *col, int64_t row, char **pch) {
  SEXP v = col[row];
  int32_t wf = whichWriter(v);
  if (TYPEOF(v)==VECSXP || wf==INT32_MIN) {
    error("Row %d of list column is type '%s' - not yet implemented. fwrite() can write list columns containing atomic vectors of type logical, integer, integer64, double, character and factor, currently.",
           row+1, type2char(TYPEOF(v)));
  }
  char *ch = *pch;
  write_chars(sep2start, &ch);
  void *data = (void *)DATAPTR(v);
  writer_fun_t fun = funs[wf];
  for (int j=0; j<LENGTH(v); j++) {
    (*fun)(data, j, &ch);
    *ch++ = sep2;
  }
  if (LENGTH(v)) ch--; // backup over the last sep2 after the last item
  write_chars(sep2end, &ch);
  *pch = ch;
}

static int32_t whichWriter(SEXP column) {
// int32_t is returned here just so the caller can output nice context-full error message should INT32_MIN be returned
// the caller then passes uint8_t to fwriteMain
  switch(TYPEOF(column)) {
  case LGLSXP:
    return logical01 ? WF_Bool32 : WF_Bool32AsString;
  case INTSXP:
    if (isFactor(column))                return WF_CategString;
    if (dateTimeAs==DATETIMEAS_EPOCH)    return WF_Int32;
    if (INHERITS(column, char_ITime))    return WF_ITime;
    if (INHERITS(column, char_Date))     return WF_DateInt32;
    return WF_Int32;
  case REALSXP:
    if (INHERITS(column, char_nanotime) && dateTimeAs!=DATETIMEAS_EPOCH) return WF_Nanotime;
    if (INHERITS(column, char_integer64))return WF_Int64;
    if (dateTimeAs==DATETIMEAS_EPOCH)    return WF_Float64;
    if (INHERITS(column, char_Date))     return WF_DateFloat64;
    if (INHERITS(column, char_POSIXct))  return WF_POSIXct;
    return WF_Float64;
  case STRSXP:
    return WF_String;
  case VECSXP:
    return WF_List;
  default:
    return INT32_MIN;
  }
}

SEXP fwriteR(
  SEXP DF,                 // any list of same length vectors; e.g. data.frame, data.table
  SEXP filename_Arg,
  SEXP sep_Arg,
  SEXP sep2_Arg,
  SEXP eol_Arg,
  SEXP na_Arg,
  SEXP dec_Arg,
  SEXP quote_Arg,          // 'auto'=NA_LOGICAL|TRUE|FALSE
  SEXP qmethodEscape_Arg,  // TRUE|FALSE
  SEXP append_Arg,         // TRUE|FALSE
  SEXP rowNames_Arg,       // TRUE|FALSE
  SEXP colNames_Arg,       // TRUE|FALSE
  SEXP logical01_Arg,      // TRUE|FALSE
  SEXP dateTimeAs_Arg,     // 0=ISO(yyyy-mm-dd),1=squash(yyyymmdd),2=epoch,3=write.csv
  SEXP buffMB_Arg,         // [1-1024] default 8MB
  SEXP nThread_Arg,
  SEXP showProgress_Arg,
  SEXP verbose_Arg)
{
  if (!isNewList(DF)) error("fwrite must be passed an object of type list; e.g. data.frame, data.table");
  fwriteMainArgs args;
  args.verbose = LOGICAL(verbose_Arg)[0];
  args.filename = CHAR(STRING_ELT(filename_Arg, 0));
  args.ncol = length(DF);
  if (args.ncol==0) {
    warning("fwrite was passed an empty list of no columns. Nothing to write.");
    return R_NilValue;
  }
  args.nrow = length(VECTOR_ELT(DF, 0));

  SEXP DFcoerced = DF;
  int protecti = 0;
  dateTimeAs = INTEGER(dateTimeAs_Arg)[0];
  if (dateTimeAs == DATETIMEAS_WRITECSV) {
    int j=0;
    while(j<args.ncol && !INHERITS(VECTOR_ELT(DF,j), char_POSIXct)) j++;
    if (j<args.ncol) {
      // dateTimeAs=="write.csv" && there exist some POSIXct columns; coerce them
      DFcoerced = PROTECT(allocVector(VECSXP, args.ncol));
      protecti++;
      // potentially large if ncol=1e6 as reported in #1903 where using large VLA caused stack overflow
      SEXP s = PROTECT(allocList(2));
      // no protecti++ needed here as one-off UNPROTECT(1) a few lines below
      SET_TYPEOF(s, LANGSXP);
      SETCAR(s, install("format.POSIXct"));
      for (int j=0; j<args.ncol; j++) {
        SEXP column = VECTOR_ELT(DF, j);
        if (INHERITS(column, char_POSIXct)) {
          SETCAR(CDR(s), column);
          SET_VECTOR_ELT(DFcoerced, j, eval(s, R_GlobalEnv));
        } else {
          SET_VECTOR_ELT(DFcoerced, j, column);
        }
      }
      UNPROTECT(1);  // s, not DFcoerced
    }
  }

  // allocate new `columns` vector. Although this could be DATAPTR(DFcoerced) directly, it can't
  // because there's an offset on each column that points to (DATAPTR for each column) which fread.c
  // would need to know. Rather than have the complication of a new offset variable, we just alloc a
  // new vetcors of pointers directly. It won't make a difference to speed because only this new
  // vector need be used by fread.c.  It just uses a tiny bit more memory (ncol * 8 bytes).
  args.columns = (void *)R_alloc(args.ncol, sizeof(SEXP));

  args.funs = funs;  // funs declared statically at the top of this file

  // Allocate and populate lookup vector to writer function for each column, whichFun[]
  args.whichFun = (uint8_t *)R_alloc(args.ncol, sizeof(uint8_t));

  // just for use at this level to control whichWriter() when called now for each column and
  // when called later for cell items of list columns (if any)
  dateTimeAs = INTEGER(dateTimeAs_Arg)[0];
  logical01 = LOGICAL(logical01_Arg)[0];

  int firstListColumn = 0;
  for (int j=0; j<args.ncol; j++) {
    SEXP column = VECTOR_ELT(DFcoerced, j);
    if (args.nrow != length(column))
      error("Column %d's length (%d) is not the same as column 1's length (%d)", j+1, length(column), args.nrow);
    int32_t wf = whichWriter(column);
    if (wf<0) {
      error("Column %d's type is '%s' - not yet implemented in fwrite.", j+1, type2char(TYPEOF(column)));
    }
    args.columns[j] = (wf==WF_CategString ? column : (void *)DATAPTR(column));
    args.whichFun[j] = (uint8_t)wf;
    if (TYPEOF(column)==VECSXP && firstListColumn==0) firstListColumn = j+1;
  }

  SEXP cn = getAttrib(DF, R_NamesSymbol);
  args.colNames = (LOGICAL(colNames_Arg)[0] && isString(cn)) ? (void *)DATAPTR(cn) : NULL;

  // user may want row names even when they don't exist (implied row numbers as row names)
  // so we need a separate boolean flag as well as the row names should they exist (rare)
  args.doRowNames = LOGICAL(rowNames_Arg)[0];
  args.rowNames = NULL;
  if (args.doRowNames) {
    SEXP rn = PROTECT(getAttrib(DF, R_RowNamesSymbol));
    protecti++;
    args.rowNames = isString(rn) ? (void *)DATAPTR(rn) : NULL;
  }

  args.sep = *CHAR(STRING_ELT(sep_Arg, 0));  // DO NOT DO: allow multichar separator (bad idea)
  args.sep2 = sep2 = *CHAR(STRING_ELT(sep2_Arg, 1));
  args.dec = *CHAR(STRING_ELT(dec_Arg,0));

  if (!firstListColumn) {
    if (args.verbose) Rprintf("No list columns are present. Setting sep2='' otherwise quote='auto' would quote fields containing sep2.\n");
    args.sep2 = sep2 = '\0';
  } else {
    if (args.verbose) {
      Rprintf("If quote='auto', fields will be quoted if the field contains either sep ('%c') or sep2 ('%c') because column %d is a list column.\n",
              args.sep, args.sep2, firstListColumn );
    }
    if (args.dec==args.sep || args.dec==args.sep2 || args.sep==args.sep2) {
      error("sep ('%c'), sep2 ('%c') and dec ('%c') must all be different. Column %d is a list column.",
            args.sep, args.sep2, args.dec, firstListColumn);
    }
  }
  // just for writeList at this level currently
  sep2start = CHAR(STRING_ELT(sep2_Arg, 0));
  sep2end = CHAR(STRING_ELT(sep2_Arg, 2));

  args.eol = CHAR(STRING_ELT(eol_Arg, 0));
  args.na = CHAR(STRING_ELT(na_Arg, 0));
  args.doQuote = LOGICAL(quote_Arg)[0] == NA_LOGICAL ? INT8_MIN : LOGICAL(quote_Arg)[0]==1;
  args.qmethodEscape = (int8_t)(LOGICAL(qmethodEscape_Arg)[0]==1);
  args.squashDateTime = (dateTimeAs==1);
  args.append = LOGICAL(append_Arg)[0];
  args.buffMB = INTEGER(buffMB_Arg)[0];
  args.nth = INTEGER(nThread_Arg)[0];
  args.showProgress = LOGICAL(showProgress_Arg)[0];

  fwriteMain(args);

  UNPROTECT(protecti);
  return(R_NilValue);
}

