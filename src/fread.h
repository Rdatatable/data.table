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

static const char typeName[NUMTYPE][10] = {"drop", "bool8", "int32", "int64", "float64", "string"};
static size_t     typeSize[NUMTYPE]     = { 0,      1,       4,       8,       8,         8      };
// size_t to prevent potential overflow of n*typeSize[i] (standard practice)

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

void freadMain(
    const char *input,
    char sepIn,
    char decIn,
    char quoteIn,
    int8_t header,
    uint64_t nrowLimit,
    uint64_t skipNrow,
    const char *skipString,
    const char **NAstringsIn,
    uint32_t nNAstringsIn,
    _Bool stripWhite,
    _Bool skipEmptyLines,
    _Bool fill,
    _Bool showProgress,
    uint32_t nth,
    _Bool verbose );

// Called from freadMain; implemented in freadR.c
_Bool userOverride(int8_t *type, lenOff *colNames, const char *anchor, int ncol);
void **allocateDT(int8_t *type, int ncol, int ndrop, uint64_t allocNrow, double *GiB, size_t *colHeaderBytes);
void setFinalNrow(uint64_t nrow);
void reallocColType(int col, colType newType);
void STOP(const char *format, ...);
void progress(double percent/*[0,1]*/, double ETA/*secs*/);
void pushAllStringCols(int8_t *type, int ncol, void **buff, const char *anchor, int howManyStringCols, int howManyRows, uint64_t ansi);

#define STRICT_R_HEADERS   // https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Error-handling
#include <R.h>
#define DTERROR error
#define DTWARN  warning
#define DTPRINT Rprintf


