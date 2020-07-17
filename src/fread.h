#ifndef dt_FREAD_H
#define dt_FREAD_H
#include "dt_stdio.h"  // PRId64
#include <stdint.h>    // uint32_t
#include <stdlib.h>    // size_t
#include <stdbool.h>   // bool
#include "myomp.h"
#ifdef DTPY
  #include "py_fread.h"
#else
  #include "freadR.h"
#endif

// Ordered hierarchy of types
typedef enum {
  NEG = -1,        // dummy to force signed type; sign bit used for out-of-sample type bump management
  CT_DROP = 0,     // skip column requested by user; it is navigated as a string column with the prevailing quoteRule
  CT_BOOL8_N,      // int8_t; first enum value must be 1 not 0(=CT_DROP) so that it can be negated to -1.
  CT_BOOL8_U,
  CT_BOOL8_T,
  CT_BOOL8_L,
  CT_INT32,        // int32_t
  CT_INT64,        // int64_t
  CT_FLOAT64,      // double (64-bit IEEE 754 float)
  CT_FLOAT64_EXT,  // double, with NAN/INF literals
  CT_FLOAT64_HEX,  // double, in hexadecimal format
  CT_ISO8601_DATE, // integer, as read from a date in ISO-8601 format
  CT_ISO8601_TIME, // double, as read from a timestamp in ISO-8601 time
  CT_STRING,       // lenOff struct below
  NUMTYPE          // placeholder for the number of types including drop; used for allocation and loop bounds
} colType;

extern int8_t typeSize[NUMTYPE];
extern const char typeName[NUMTYPE][10];
extern const long double pow10lookup[601];
extern const uint8_t hexdigits[256];


// Strings are pushed by fread_main using an offset from an anchor address plus
// string length; fread_impl.c then manages strings appropriately
typedef struct {
  int32_t len;  // signed to distinguish NA vs empty ""
  int32_t off;
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

  // Maximum number of threads. If 0, then fread will use the maximum possible
  // number of threads, as determined by omp_get_max_threads(). If negative,
  // then fread will use that many threads less than allowed maximum (but
  // always at least 1).
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
  bool stripWhite;

  // If True, empty lines in the file will be skipped. Otherwise empty lines
  // will produce rows of NAs.
  bool skipEmptyLines;

  // If True, then rows are allowed to have variable number of columns, and
  // all ragged rows will be filled with NAs on the right.
  bool fill;

  // If True, then emit progress messages during the parsing.
  bool showProgress;

  // Emit extra debug-level information.
  bool verbose;

  // If true, then this field instructs `fread` to treat warnings as errors. In
  // particular in R this setting is turned on whenever `option(warn=2)` is set,
  // in which case calling the standard `warning()` raises an exception.
  // However `fread` still needs to know that the exception will be raised, so
  // that it can do proper cleanup / resource deallocation -- otherwise memory
  // leaks would occur.
  bool warningsAreErrors;

  // If true, then column of 0s and 1s will be read as logical, otherwise it
  // will become integer.
  bool logical01;

  bool keepLeadingZeros;
  
  // should datetime with no Z or UTZ-offset be read as UTC?
  bool noTZasUTC;

  char _padding[1];

  // Any additional implementation-specific parameters.
  FREAD_MAIN_ARGS_EXTRA_FIELDS

} freadMainArgs;



// *****************************************************************************

typedef struct ThreadLocalFreadParsingContext
{
  // Pointer that serves as a starting point for all offsets within the `lenOff`
  // structs.
  const char *__restrict__ anchor;

  // Output buffers for values with different alignment requirements. For
  // example all `lenOff` columns, `double` columns and `int64` columns will be
  // written to buffer `buff8`; at the same time `bool` and `int8` columns will
  // be stored in memory buffer `buff1`.
  // Within each buffer the data is stored in row-major order, i.e. in the same
  // order as in the original CSV file.
  void *__restrict__ buff8;
  void *__restrict__ buff4;
  void *__restrict__ buff1;

  // Size (in bytes) for a single row of data within the buffers `buff8`,
  // `buff4` and `buff1` correspondingly.
  size_t rowSize8;
  size_t rowSize4;
  size_t rowSize1;

  // Starting row index within the output DataTable for the current data chunk.
  size_t DTi;

  // Number of rows currently being stored within the buffers. The allocation
  // size of each `buffX` is thus at least `nRows * rowSizeX`.
  size_t nRows;

  // Reference to the flag that controls the parser's execution. Setting this
  // flag to true will force parsing of the CSV file to terminate in the near
  // future.
  bool *stopTeam;

  int threadn;

  int quoteRule;

  // Any additional implementation-specific parameters.
  FREAD_PUSH_BUFFERS_EXTRA_FIELDS

} ThreadLocalFreadParsingContext;



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
bool userOverride(int8_t *types, lenOff *colNames, const char *anchor,
                  const int ncol);


/**
 * This function is invoked by `freadMain` before the main scan of the input
 * file. It should allocate the resulting `DataTable` structure and prepare
 * to receive the data in chunks.
 *
 * Additionally, this function will be invoked if the main scan was
 * unsuccessful. This may happen either because there were out-of-sample type
 * exceptions (i.e. a value was found in one of the columns that wasn't
 * acceptable for that column's type), or if the initial estimate of the file's
 * number of rows turned out to be too conservative, and more rows has to be
 * appended to the DataTable.
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
 * Called once at the beginning of each thread before it starts scanning the
 * input file. If the file needs to be rescanned because of out-of-type
 * exceptions, this will be called again before the second scan.
 */
void prepareThreadContext(ThreadLocalFreadParsingContext *ctx);


/**
 * Give upstream the chance to modify the scanned buffers after the thread
 * finished reading its chunk but before it enters the "ordered" section.
 * Variable `ctx.DTi` is not available at this moment.
 */
void postprocessBuffer(ThreadLocalFreadParsingContext *ctx);


/**
 * Callback invoked within the "ordered" section for each thread. Only
 * lightweight processing should be performed here, since this section stalls
 * execution of any other thread!
 */
void orderBuffer(ThreadLocalFreadParsingContext *ctx);


/**
 * This function transfers the scanned input data into the final DataTable
 * structure. It will be called many times, and from parallel threads (thus
 * it should not attempt to modify any global variables). Its primary job is
 * to transpose the data: convert from row-major order within each buffer
 * into the column-major order for the resulting DataTable.
 */
void pushBuffer(ThreadLocalFreadParsingContext *ctx);


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
 * Free any srtuctures associated with the thread-local parsing context.
 */
void freeThreadContext(ThreadLocalFreadParsingContext *ctx);


/**
 * Progress-reporting function.
 */
void progress(int percent/*[0,100]*/, int ETA/*secs*/);


bool freadCleanup(void);
double wallclock(void);

#endif
