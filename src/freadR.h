#ifndef dt_FREAD_R_H
#define dt_FREAD_R_H
#define STRICT_R_HEADERS   // https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Error-handling
#include <R.h>

// Forward declarations
void STOP(const char *format, ...);
void freadLastWarning(const char *format, ...);


#define DTPRINT Rprintf


#define DTWARN(...) { \
  if (warningsAreErrors) \
    freadLastWarning(__VA_ARGS__); \
  else \
    warning(__VA_ARGS__); \
}


#define FREAD_MAIN_ARGS_EXTRA_FIELDS

#define FREAD_PUSH_BUFFERS_EXTRA_FIELDS \
    int nStringCols; \
    int nNonStringCols;

#endif
