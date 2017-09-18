
#include <stdbool.h>
#include "data.table.h"
#include "fwrite.h"

#define DATETIMEAS_EPOCH     2
#define DATETIMEAS_WRITECSV  3


extern char sep2;
static bool logical01=true;
static int dateTimeAs=0;         // 0=ISO(yyyy-mm-dd), 1=squash(yyyymmdd), 2=epoch, 3=write.csv
static const char *sep2start, *sep2end;
// sep2 is in main fwrite.c so that writeString can quote other fields if sep2 is present in them
// if there are no list columns, set sep2=='\0'

// Non-agnostic helpers ...

inline const char *getString(SEXP col, int row) {
  SEXP x = STRING_ELT(col, row);
  return x==NA_STRING ? NULL : CHAR(x);
}

inline const char *getCategString(SEXP col, int row) {
  int x = INTEGER(col)[row];
  return x==NA_INTEGER ? NULL : CHAR(STRING_ELT(getAttrib(col, R_LevelsSymbol), x-1));
}

static writer_fun_t whichWriter(SEXP);

void writeList123(void *col, int64_t row, char **thCh) {
  SEXP v = VECTOR_ELT(col,row);
  writer_fun_t fun = whichWriter(v);
  if (TYPEOF(v)==VECSXP || fun==NULL) {
    error("Row %d of list column is type '%s' - not yet implemented. fwrite() can write list columns containing atomic vectors of type logical, integer, integer64, double, character and factor, currently.",
           row+1, type2char(TYPEOF(v)));
  }
  char *ch = *thCh;
  write_chars(sep2start, &ch);
  void *data = (void *)DATAPTR(v);
  for (int j=0; j<LENGTH(v); j++) {
    (*fun)(data, j, &ch);
    *ch++ = sep2;
  }
  if (LENGTH(v)) ch--; // backup over the last sep2 after the last item
  write_chars(sep2end, &ch);
  *thCh = ch;
}

static writer_fun_t whichWriter(SEXP column) {
  switch(TYPEOF(column)) {
  case LGLSXP:
    return logical01 ? writeBool32 : writeBool32AsString;
  case INTSXP:
    if (isFactor(column))                return writeCategString;
    if (dateTimeAs==DATETIMEAS_EPOCH)    return writeInt32;
    if (INHERITS(column, char_ITime))    return writeITime;
    if (INHERITS(column, char_Date))     return writeDateInt32;
    return writeInt32;
  case REALSXP:
    if (INHERITS(column, char_nanotime) && dateTimeAs!=DATETIMEAS_EPOCH) return writeNanotime;
    if (INHERITS(column, char_integer64))return writeInt64;
    if (dateTimeAs==DATETIMEAS_EPOCH)    return writeFloat64;
    if (INHERITS(column, char_Date))     return writeDateFloat64;
    if (INHERITS(column, char_POSIXct))  return writePOSIXct;
    return writeFloat64;
  case STRSXP:
    return writeString;
  case VECSXP:
    return writeList123;
  default:
    return NULL;
  }
}


SEXP writefile(SEXP DFin,               // any list of same length vectors; e.g. data.frame, data.table
               SEXP filename_Arg,
               SEXP sep_Arg,
               SEXP sep2_Arg,
               SEXP eol_Arg,
               SEXP na_Arg,
               SEXP dec_Arg,
               SEXP quote_Arg,          // 'auto'=NA_LOGICAL|TRUE|FALSE
               SEXP qmethod_escapeArg,  // TRUE|FALSE
               SEXP append,             // TRUE|FALSE
               SEXP row_names,          // TRUE|FALSE
               SEXP col_names,          // TRUE|FALSE
               SEXP logical01_Arg,      // TRUE|FALSE
               SEXP dateTimeAs_Arg,     // 0=ISO(yyyy-mm-dd),1=squash(yyyymmdd),2=epoch,3=write.csv
               SEXP buffMB_Arg,         // [1-1024] default 8MB
               SEXP nThread,
               SEXP showProgress_Arg,
               SEXP verbose_Arg)
{
  if (!isNewList(DFin)) error("fwrite must be passed an object of type list; e.g. data.frame, data.table");
  RLEN ncol = length(DFin);
  if (ncol==0) {
    warning("fwrite was passed an empty list of no columns. Nothing to write.");
    return R_NilValue;
  }
  RLEN nrow = length(VECTOR_ELT(DFin, 0));

  const bool showProgress = LOGICAL(showProgress_Arg)[0];
  const bool verbose = LOGICAL(verbose_Arg)[0];

  const char sep = *CHAR(STRING_ELT(sep_Arg, 0));  // DO NOT DO: allow multichar separator (bad idea)
  sep2start = CHAR(STRING_ELT(sep2_Arg, 0));
  sep2 = *CHAR(STRING_ELT(sep2_Arg, 1));
  sep2end = CHAR(STRING_ELT(sep2_Arg, 2));

  const char *eol = CHAR(STRING_ELT(eol_Arg, 0));
  // someone might want a trailer on every line so allow any length string as eol

  const char *na = CHAR(STRING_ELT(na_Arg, 0));
  const char dec = *CHAR(STRING_ELT(dec_Arg,0));
  int8_t quote = LOGICAL(quote_Arg)[0] == NA_LOGICAL ? INT8_MIN : LOGICAL(quote_Arg)[0]==1;
  // When NA is a non-empty string, then we must quote all string fields
  if (*na != '\0' && quote == INT8_MIN) quote = true;
  bool qmethod_escape = (int8_t)(LOGICAL(qmethod_escapeArg)[0]==1);
  const char *filename = CHAR(STRING_ELT(filename_Arg, 0));
  logical01 = LOGICAL(logical01_Arg)[0]==1;
  dateTimeAs = INTEGER(dateTimeAs_Arg)[0];
  bool squashDateTime = (dateTimeAs==1);
  int nth = INTEGER(nThread)[0];
  int firstListColumn = 0;

  SEXP DF = DFin;
  int protecti = 0;
  if (dateTimeAs == DATETIMEAS_WRITECSV) {
    int j=0; while(j<ncol && !INHERITS(VECTOR_ELT(DFin,j), char_POSIXct)) j++;
    if (j<ncol) {
      // dateTimeAs=="write.csv" && there exist some POSIXct columns; coerce them
      DF = PROTECT(allocVector(VECSXP, ncol));
      protecti++;
      // potentially large if ncol=1e6 as reported in #1903 where using large VLA caused stack overflow
      SEXP s = PROTECT(allocList(2));
      SET_TYPEOF(s, LANGSXP);
      SETCAR(s, install("format.POSIXct"));
      for (int j=0; j<ncol; j++) {
        SEXP column = VECTOR_ELT(DFin, j);
        if (INHERITS(column, char_POSIXct)) {
          SETCAR(CDR(s), column);
          SET_VECTOR_ELT(DF, j, eval(s, R_GlobalEnv));
        } else {
          SET_VECTOR_ELT(DF, j, column);
        }
      }
      UNPROTECT(1);  // s, not DF
    }
  }

  // Allocate lookup vector to writer function for each column. For simplicity and robustness via many fewer lines
  // of code and less logic need. Secondly, for efficiency to save deep switch and branches later.
  // Don't use a VLA as ncol could be > 1e6 columns
  writer_fun_t *fun = (writer_fun_t *)R_alloc(ncol, sizeof(writer_fun_t));
  for (int j=0; j<ncol; j++) {
    SEXP column = VECTOR_ELT(DF, j);
    if (nrow != length(column))
      error("Column %d's length (%d) is not the same as column 1's length (%d)", j+1, length(column), nrow);
    if ((fun[j] = whichWriter(column)) == NULL)
      error("Column %d's type is '%s' - not yet implemented.", j+1, type2char(TYPEOF(column)) );
    if (TYPEOF(column)==VECSXP && firstListColumn==0) firstListColumn = j+1;
  }

  if (!firstListColumn) {
    if (verbose) Rprintf("No list columns are present. Setting sep2='' otherwise quote='auto' would quote fields containing sep2.\n");
    sep2='\0';
  } else {
    if (verbose) Rprintf("If quote='auto', fields will be quoted if the field contains either sep ('%c') or sep2[2] ('%c') because column %d is a list column.\n", sep, sep2, firstListColumn );
    if (dec==sep) error("Internal error: dec != sep was checked at R level");
    if (dec==sep2 || sep==sep2)
      error("sep ('%c'), sep2[2L] ('%c') and dec ('%c') must all be different when list columns are present. Column %d is a list column.", sep, sep2, dec, firstListColumn);
  }

  // user may want row names even when they don't exist (implied row numbers as row names)
  Rboolean doRowNames = LOGICAL(row_names)[0];
  SEXP rowNames = NULL;
  if (doRowNames) {
    rowNames = getAttrib(DFin, R_RowNamesSymbol);
    if (!isString(rowNames)) rowNames=NULL;
  }

  fwriteMain(
    (const char *) filename,
    (void **) columns,
    (int) ncol,
    (int64_t) nrow,
    (void *) colNames,
    (writer_fun_t *) fun,
    (int8_t *) whichFun,
    (char) sep,
    (const char *) eol,
    (const char *) na,
    (char) dec,
    (char) quote,
    (bool) quotesDoubled,
    (bool) squashDateTime,
    (bool) append,
    (bool) doRowNames,
    (void *) rowNames,
    (int) buffMB,
    (int) nth,
    (bool) showProgress,
    (bool) verbose
  );

  UNPROTECT(protecti);
  return(R_NilValue);
}

