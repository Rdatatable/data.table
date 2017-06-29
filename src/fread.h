#ifndef dt_FREAD_H
#define dt_FREAD_H
#include <stdint.h>  // uint32_t
#include <stdlib.h>  // size_t
#ifdef DTPY
  #include "py_fread.h"
#else
  #include "freadR.h"
#endif


// Ordered hierarchy of types
typedef enum {
  NEG = -1,       // dummy to force signed type; sign bit used for out-of-sample type bump management
  CT_DROP = 0,    // skip column requested by user; it is navigated as a string column with the prevailing quoteRule
  CT_BOOL8,       // int8_t; first type enum value must be 1 not 0 so that it can be negated to -1.
  CT_INT32_BARE,  // int32_t bare bones fast
  CT_INT32_FULL,  // int32_t if spaces or quotes can surround the value
  CT_INT64,       // int64_t
  CT_FLOAT64,     // double (64-bit IEEE 754 float)
  CT_STRING,      // lenOff struct below
  NUMTYPE         // placeholder for the number of types including drop; used for allocation and loop bounds
} colType;

extern int8_t typeSize[NUMTYPE];
extern const char typeName[NUMTYPE][10];
extern const long double pow10lookup[701];


// Strings are pushed by fread_main using an offset from an anchor address plus
// string length; fread_impl.c then manages strings appropriately
typedef struct {
  int32_t len;  // signed to distinguish NA vs empty ""
  uint32_t off;
} lenOff;


#define NA_BOOL8         INT8_MIN
#define NA_INT32         INT32_MIN
#define NA_INT64         INT64_MIN
#define NA_FLOAT64_I64   0x7FF00000000007A2
#define NA_LENOFF        INT32_MIN  // lenOff.len only; lenOff.off undefined for NA



// *****************************************************************************

typedef struct freadMainArgs
{
  // Name of the file to open (a \0-terminated C string). If the file name
  // contains non-ASCII characters, it should be UTF-8 encoded (however fread
  // will not validate the encoding).
  const char *filename;

  // Data buffer: a \0-terminated C string. When this parameter is given,
  // fread() will read from the provided string. This parameter is exclusive
  // with `filename`.
  const char *input;

  // Maximum number of rows to read, or INT64_MAX to read the entire dataset.
  // Note that even if `nrowLimit = 0`, fread() will scan a sample of rows in
  // the file to detect column names and types (and other parsing settings).
  int64_t nrowLimit;

  // Number of input lines to skip when reading the file.
  int64_t skipNrow;

  // Skip to the line containing this string. This parameter cannot be used
  // with `skipLines`.
  const char *skipString;

  // NULL-terminated list of strings that should be converted into NA values.
  // The last entry in this array is NULL (sentinel), which lets us know where
  // the array ends.
  const char * const* NAstrings;

  // Maximum number of threads (should be >= 1).
  int32_t nth;

  // Character to use for a field separator. Multi-character separators are not
  // supported. If `sep` is '\0', then fread will autodetect it. A quotation
  // mark '"' is not allowed as field separator.
  char sep;

  // Decimal separator for numbers (usually '.'). This may coincide with `sep`,
  // in which case floating-point numbers will have to be quoted. Multi-char
  // (or non-ASCII) decimal separators are not supported. A quotation mark '"'
  // is not allowed as decimal separator.
  // See: https://en.wikipedia.org/wiki/Decimal_mark
  char dec;

  // Character to use as a quotation mark (usually '"'). Pass '\0' to disable
  // field quoting. This parameter cannot be auto-detected. Multi-character,
  // non-ASCII, or different open/closing quotation marks are not supported.
  char quote;

  // Is there a header at the beginning of the file?
  // 0 = no, 1 = yes, -128 = autodetect
  int8_t header;

  // Strip the whitespace from fields (usually True).
  _Bool stripWhite;

  // If True, empty lines in the file will be skipped. Otherwise empty lines
  // will produce rows of NAs.
  _Bool skipEmptyLines;

  // If True, then rows are allowed to have variable number of columns, and
  // all ragged rows will be filled with NAs on the right.
  _Bool fill;

  // If True, then emit progress messages during the parsing.
  _Bool showProgress;

  // Emit extra debug-level information.
  _Bool verbose;

  // If true, then this field instructs `fread` to treat warnings as errors. In
  // particular in R this setting is turned on whenever `option(warn=2)` is set,
  // in which case calling the standard `warning()` raises an exception.
  // However `fread` still needs to know that the exception will be raised, so
  // that it can do proper cleanup / resource deallocation -- otherwise memory
  // leaks would occur.
  _Bool warningsAreErrors;

  char _padding[2];

  // Any additional implementation-specific parameters.
  FREAD_MAIN_ARGS_EXTRA_FIELDS

} freadMainArgs;



// *****************************************************************************

/**
 * Fast parallel reading of CSV files with intelligent guessing of parse
 * parameters.
 *
 * It should have been called just "fread", but that name is already defined in
 * the system libraries...
 */
int freadMain(freadMainArgs args);


/**
 * This callback is invoked by `freadMain` after the initial pre-scan of the
 * file, when all parsing parameters have been determined; most importantly the
 * column names and their types.
 *
 * This function serves two purposes: first, it tells the upstream code what the
 * detected column names are; and secondly what is the expected type of each
 * column. The upstream code then has an opportunity to upcast the column types
 * if requested by the user, or mark some columns as skipped.
 *
 * @param types
 *    type codes of each column in the CSV file. Possible type codes are
 *    described by the `colType` enum. The function may modify this array
 *    setting some types to 0 (CT_DROP), or upcasting the types. Downcasting is
 *    not allowed and will trigger an error from `freadMain` later on.
 *
 * @param colNames
 *    array of `lenOff` structures (offsets are relative to the `anchor`)
 *    describing the column names. If the CSV file had no header row, then this
 *    array will be filled with 0s.
 *
 * @param anchor
 *    pointer to a string buffer (usually somewhere inside the memory-mapped
 *    file) within which the column names are located, as described by the
 *    `colNames` array.
 *
 * @param ncol
 *    total number of columns. This is the length of arrays `types` and
 *    `colNames`.
 *
 * @return
 *    this function may return `false` to request that fread abort reading
 *    the CSV file. Normally, this function should return `true`.
 */
_Bool userOverride(int8_t *types, lenOff *colNames, const char *anchor,
                   int ncol);


/**
 * This function is invoked by `freadMain` right before the main scan of the
 * input file. This function should allocate the resulting `DataTable` structure
 * and prepare to receive the data in chunks.
 *
 * @param types
 *     array of type codes for each column. Same as in the `userOverride`
 *     function.
 *
 * @param sizes
 *    the size (in bytes) of each column within the buffer(s) that will be
 *    passed to `pushBuffer()` during the scan. This array should be saved for
 *    later use. It exists mostly for convenience, since the size of each
 *    non-skipped column may be determined from that column's type.
 *
 * @param ncols
 *    number of columns in the CSV file. This is the size of arrays `types` and
 *    `sizes`.
 *
 * @param ndrop
 *    count of columns with type CT_DROP. This parameter is provided for
 *    convenience, since it can always be computed from `types`. The resulting
 *    datatable will have `ncols - ndrop` columns.
 *
 * @param nrows
 *    the number of rows to allocate for the datatable. This number of rows is
 *    estimated during the initial pre-scan, and then adjusted upwards to
 *    account for possible variation. It is very unlikely that this number
 *    underestimates the final row count.
 *
 * @return
 *    this function should return the total size of the Datatable created (for
 *    reporting purposes). If the return value is 0, then it indicates an error
 *    and `fread` will abort.
 */
size_t allocateDT(int8_t *types, int8_t *sizes, int ncols, int ndrop,
                  size_t nrows);


/**
 * Request to replace column `col` with a new column of type `newType`. This
 * function is called after the entire input file has been scanned, and after
 * we discovered during the scan that some columns did not have the expected
 * types.
 * This function will be called once for each column that needs to have its
 * type upcasted. The caller does not expect that the column retains the values
 * that were already written there.
 *
 * @param col
 *    the index of the column that will be replaced. This index is within the
 *    final DataTable, not within the arrays `types` or `sizes` (i.e. `col`
 *    ranges from 0 to `ncols - ndrop - 1`).
 *
 * @param newType
 *    new type for the column.
 */
void reallocColType(int col, colType newType);


/**
 * This function transfers the scanned input data into the final DataTable
 * structure. It will be called many times, and from parallel threads (thus
 * it should not attempt to modify any global variables). Its primary job is
 * to transpose the data: convert from row-major order within each buffer
 * into the column-major order for the resulting DataTable.
 */
void pushBuffer(const void *buff8, const void *buff4, const void *buff1,
                const char *anchor, int nRows, size_t DTi,
                int rowSize8, int rowSize4, int rowSize1,
                int nStringCols, int nNonStringCols);


/**
 * Called at the end to specify what the actual number of rows in the datatable
 * was. The function should adjust the datatable, reallocing the buffers if
 * necessary.
 * If the input file needs to be rescanned due to some columns having wrong
 * column types, then this function will be called once after the file is
 * finished scanning but before any calls to `reallocColType()`, and then the
 * second time after the entire input file was scanned again.
 */
void setFinalNrow(size_t nrows);


/**
 * Progress-reporting function.
 */
void progress(double percent/*[0,1]*/, double ETA/*secs*/);


void freadCleanup(void);
double wallclock(void);

#endif
