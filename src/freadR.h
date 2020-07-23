#ifndef dt_FREAD_R_H
#define dt_FREAD_R_H
#define STRICT_R_HEADERS   // https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Error-handling
#include <R.h>
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
void __halt(bool warn, const char *format, ...);   // see freadR.c
#define STOP(...)   __halt(0, __VA_ARGS__)
#define DTPRINT     Rprintf
#define DTWARN(...) warningsAreErrors ? __halt(1, __VA_ARGS__) : warning(__VA_ARGS__)

#endif

