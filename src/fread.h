#ifndef dt_FREAD_H
#define dt_FREAD_H
#include <stdint.h>  // uint32_t
#include <stdlib.h>  // size_t

// Ordered hierarchy of types
typedef enum {
  NEG=-1,      // dummy to force signed type; sign bit used for out-of-sample type bump management
  CT_DROP,     // skip column requested by user; it is navigated as a string column with the prevailing quoteRule
  CT_BOOL8,    // signed char; first type enum value must be 1 not 0 so that it can be negated to -1.
  CT_INT32,    // signed int32_t
  CT_INT64,    // signed int64_t
  CT_FLOAT64,  // double
  CT_STRING,   // lenOff typedef below
  NUMTYPE      // placeholder for the number of types including drop; used for allocation and loop bounds
} colType;

extern size_t typeSize[NUMTYPE];
extern const char typeName[NUMTYPE][10];

// Strings are pushed by fread_main using an offset from an anchor address plus string length
// freadR.c then manages strings appropriately
typedef struct {
  int32_t len;  // signed to distinguish NA vs empty ""
  uint32_t off;
} lenOff;

#define NA_BOOL8         INT8_MIN
#define NA_INT32         INT32_MIN
#define NA_INT64         INT64_MIN
#define NA_FLOAT64_I64   0x7FF00000000007A2
#define NA_LENOFF        INT32_MIN  // lenOff.len only; lenOff.off undefined for NA

typedef struct {
  const char *filename;
  const char *input;
  char sep;
  char dec;
  char quote;
  int8_t header;          // true|false|NA_BOOL8
  uint64_t nrowLimit;     // UINT64_MAX represents no limit
  uint64_t skipNrow;
  const char *skipString;
  const char **NAstrings;
  uint32_t nNAstrings;
  _Bool stripWhite;
  _Bool skipEmptyLines;
  _Bool fill;
  _Bool showProgress;
  uint32_t nth;        // number of threads >= 1
  _Bool verbose;
} freadMainArgs;

int freadMain(freadMainArgs *args);

// Called from freadMain; implemented in freadR.c
_Bool userOverride(int8_t *type, lenOff *colNames, const char *anchor, int ncol);
double allocateDT(int8_t *type, int ncol, int ndrop, uint64_t allocNrow);
void setFinalNrow(uint64_t nrow);
void reallocColType(int col, colType newType);
void progress(double percent/*[0,1]*/, double ETA/*secs*/);
void pushBuffer(int8_t *type, int ncol, void **buff, const char *anchor,
                int nStringCols, int nNonStringCols, int nRows, uint64_t ansi);
void STOP(const char *format, ...);
void freadCleanup();

#define STRICT_R_HEADERS   // https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Error-handling
#include <R.h>
#define DTERROR error
#define DTWARN  warning
#define DTPRINT Rprintf


#endif
