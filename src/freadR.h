#ifndef dt_FREAD_R_H
#define dt_FREAD_R_H
#ifndef STRICT_R_HEADERS
  #define STRICT_R_HEADERS   // https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Error-handling
#endif
#include <R.h>
#include <Rinternals.h>
#include "po.h"

#define FREAD_MAIN_ARGS_EXTRA_FIELDS \
  bool oldNoDateTime;

#define FREAD_PUSH_BUFFERS_EXTRA_FIELDS \
  int nStringCols; \
  int nNonStringCols;

// Before error() [or warning() with options(warn=2)] call freadCleanup() to close mmp and fix :
//   http://stackoverflow.com/questions/18597123/fread-data-table-locks-files
// However, msg has to be manually constructed first (rather than simply leaving construction to snprintf inside warning()
// or error()) because the msg usually points to substrings from the mmp (which is invalid after close).
// Where no halt is happening, we can just use raw Rprintf() or warning()
void halt__(bool warn, const char *format, ...);   // see freadR.c
#define STOP(...)   halt__(0, __VA_ARGS__)
static char internal_error_buff[1000] __attribute__((unused)); // match internalErrSize // todo: fix imports such that compiler warns correctly #6468
#define INTERNAL_STOP(...) do {snprintf(internal_error_buff, sizeof(internal_error_buff), __VA_ARGS__); halt__(0, "%s %s: %s. %s", _("Internal error in"), __func__, internal_error_buff, _("Please report to the data.table issues tracker"));} while (0)
#define DTPRINT     Rprintf
#define DTWARN(...) warningsAreErrors ? halt__(1, __VA_ARGS__) : warning(__VA_ARGS__)

#endif
