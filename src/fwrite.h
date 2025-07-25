#ifdef DTPY
  #include "py_fread.h"
#else
  #ifndef STRICT_R_HEADERS
    #define STRICT_R_HEADERS
  #endif
  #include <R.h>
  #include <Rinternals.h>  // for SEXP in writeList() prototype
  #include "po.h"
  #define STOP     error
  #define DTPRINT  Rprintf
  static char internal_error_buff[256] __attribute__((unused)); // todo: fix imports such that compiler warns correctly #6468
  #define INTERNAL_STOP(...) do {snprintf(internal_error_buff, sizeof(internal_error_buff), __VA_ARGS__); error("%s %s: %s. %s", _("Internal error in"), __func__, internal_error_buff, _("Please report to the data.table issues tracker"));} while (0)
#endif

typedef void writer_fun_t(const void *, int64_t, char **);

// in the order of writer_fun_t in fwriteR.c
writer_fun_t writeBool8;
writer_fun_t writeBool32;
writer_fun_t writeBool32AsString;
writer_fun_t writeInt32;
writer_fun_t writeInt64;
writer_fun_t writeFloat64;
writer_fun_t writeComplex;
writer_fun_t writeITime;
writer_fun_t writeDateInt32;
writer_fun_t writeDateFloat64;
writer_fun_t writePOSIXct;
writer_fun_t writeNanotime;
writer_fun_t writeString;
writer_fun_t writeCategString;
writer_fun_t writeList;

void write_chars(const char *source, char **dest);

typedef enum {   // same order as fun[] above
  WF_Bool8,
  WF_Bool32,
  WF_Bool32AsString,
  WF_Int32,
  WF_Int64,
  WF_Float64,
  WF_Complex,
  WF_ITime,
  WF_DateInt32,
  WF_DateFloat64,
  WF_POSIXct,
  WF_Nanotime,
  WF_String,
  WF_CategString,
  WF_List
} WFs;

static const int writerMaxLen[] = {  // same order as fun[] and WFs above; max field width used for calculating upper bound line length
  5,  //&writeBool8            "false"
  5,  //&writeBool32           "false"
  5,  //&writeBool32AsString   "false"
  11, //&writeInt32            "-2147483647"
  20, //&writeInt64            "-9223372036854775807"
  29, //&writeFloat64          "-3.141592653589793115998E-123" [max sf 22 consistent with options()$digits]
  60, //&writeComplex          "-3.141592653589793115998E-123+2.7182818284590450907956i" [3x writeFloat64,+,i]
  32, //&writeITime
  16, //&writeDateInt32
  16, //&writeDateFloat64
  32, //&writePOSIXct
  48, //&writeNanotime
  0,  //&writeString
  0,  //&writeCategString
  0,  //&writeList
};

typedef struct fwriteMainArgs
{
  // Name of the file to open (a \0-terminated C string). If the file name
  // contains non-ASCII characters, it should be UTF-8 encoded (however fread
  // will not validate the encoding).
  const char *filename;
  int ncol;
  int64_t nrow;
  // a vector of pointers to all-same-length column vectors
  const void **columns;
  writer_fun_t **funs;    // a vector of writer_fun_t function pointers

  // length ncol vector containing which fun[] to use for each column
  // one byte to use 8 times less cache lines than a vector of function pointers would do
  // A limit of 256 writers seems more than sufficient
  uint8_t *whichFun;

  const void *colNames;   // NULL means no header, otherwise ncol strings
  bool doRowNames;        // optional, likely false
  const void *rowNames;   // if doRowNames is true and rowNames is NULL then row numbers are output
  uint8_t rowNameFun;     // when rowNames is not NULL, which writer to use for them
  char sep;
  char sep2;
  char dec;
  const char *eol;
  const char *na;

  // The quote character is always " (ascii 34) and cannot be changed since nobody on Earth uses a different quoting character, surely
  // doQuote controls whether to quote fields or not. NA=="auto" (default) means the contents are inspected to see if sep, eol or quote
  // is present and if so, quotes the filed. Else 1=quote all fields, 0=no quoting even when sep is present
  int8_t doQuote;

  bool qmethodEscape;     // true means escape quotes using backslash, else double-up double quotes.
  int scipen;             // same as options('scipen') in R -- used to penalize scientific notation when
                          //   deciding to write scientific or full decimal format (e.g. in comparing
                          //   10000000 to 1e+07, first has width 8, second has width 5; prefer the former
                          //   iff scipen >= 3=8-5
  bool squashDateTime;
  bool append;
  int buffMB;             // [1-1024] default 8MiB
  int nth;
  bool showProgress;
  bool is_gzip;
  int gzip_level;
  bool bom;
  const char *yaml;
  bool verbose;
} fwriteMainArgs;

void fwriteMain(fwriteMainArgs args);
