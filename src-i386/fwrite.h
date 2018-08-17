#ifdef DTPY
  #include "py_fread.h"
#else
  #define STRICT_R_HEADERS
  #include <R.h>
  #define STOP     error
  #define DTPRINT  Rprintf
#endif

typedef void (*writer_fun_t)(void *, int64_t, char **);

void writeBool8();
void writeBool32();
void writeBool32AsString();
void writeInt32();
void writeInt64();
void writeFloat64();
void writeITime();
void writeDateInt32();
void writeDateFloat64();
void writePOSIXct();
void writeNanotime();
void writeString();
void writeCategString();
void writeList();

void write_chars(const char *source, char **dest);

typedef struct fwriteMainArgs
{
  // Name of the file to open (a \0-terminated C string). If the file name
  // contains non-ASCII characters, it should be UTF-8 encoded (however fread
  // will not validate the encoding).
  const char *filename;

  int ncol;

  int64_t nrow;

  // a vector of pointers to all-same-length column vectors
  void **columns;

  writer_fun_t *funs;      // a vector of writer_fun_t function pointers

  // length ncol vector containing which fun[] to use for each column
  // one byte to use 8 times less cache lines than a vector of function pointers would do
  // A limit of 256 writers seems more than sufficient
  uint8_t *whichFun;

  void *colNames;         // NULL means no header, otherwise ncol strings

  bool doRowNames;        // optional, likely false

  void *rowNames;         // if doRowNames is true and rowNames is not NULL then they're used, otherwise row numbers are output.

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

  bool squashDateTime;

  bool append;

  int buffMB;             // [1-1024] default 8MB

  int nth;

  bool showProgress;

  bool verbose;

} fwriteMainArgs;

void fwriteMain(fwriteMainArgs args);

