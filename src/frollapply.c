#include "data.table.h"  // first (before Rdefines.h) for clang-13-omp, #5122
#include <Rdefines.h>

#define MEMCPY                                                                                                                                    \
switch (TYPEOF(d)) {                                                                                                                              \
case INTSXP: {                                                                                                                                    \
  memcpy(INTEGER(d), INTEGER(s)+o, nrow*sizeof(int));                                                                                             \
} break;                                                                                                                                          \
case LGLSXP: {                                                                                                                                    \
  memcpy(LOGICAL(d), LOGICAL(s)+o, nrow*sizeof(int));                                                                                             \
} break;                                                                                                                                          \
case REALSXP: {                                                                                                                                   \
  memcpy(REAL(d), REAL(s)+o, nrow*sizeof(double));                                                                                                \
} break;                                                                                                                                          \
case STRSXP: {                                                                                                                                    \
  for (int i=0; i<nrow; ++i)                                                                                                                      \
    SET_STRING_ELT(d, i, STRING_ELT(s, o + i));                                                                                                   \
} break;                                                                                                                                          \
case VECSXP: {                                                                                                                                    \
  for (int i=0; i<nrow; ++i)                                                                                                                      \
    SET_VECTOR_ELT(d, i, VECTOR_ELT(s, o + i));                                                                                                   \
} break;                                                                                                                                          \
case CPLXSXP:  {                                                                                                                                  \
  memcpy(COMPLEX(d), COMPLEX(s)+o, nrow*sizeof(Rcomplex));                                                                                        \
} break;                                                                                                                                          \
case RAWSXP : {                                                                                                                                   \
  memcpy(RAW(d), RAW(s)+o, nrow*sizeof(Rbyte));                                                                                                   \
} break;                                                                                                                                          \
default: error(_("Internal error: column type '%s' not supported in memcpyDT function. All known types are supported so please report as bug.")); \
}                                                                                                                                                 \

/*
 * memcpy src data into preallocated window
 * we don't call memcpyVector from memcpyDT because they are called in tight loop and we don't want to have extra branches inside
 */
SEXP memcpyVector(SEXP dest, SEXP src, SEXP offset, SEXP size) {
  size_t o = INTEGER(offset)[0] - INTEGER(size)[0];
  int nrow = LENGTH(dest);
  SEXP d = dest, s = src;
  MEMCPY
  return dest;
}
SEXP memcpyDT(SEXP dest, SEXP src, SEXP offset, SEXP size) {
  size_t o = INTEGER(offset)[0] - INTEGER(size)[0];
  int ncol = LENGTH(dest), nrow = LENGTH(VECTOR_ELT(dest, 0));
  SEXP d, s;
  for (int j=0; j<ncol; ++j) {
    d = VECTOR_ELT(dest, j);
    s = VECTOR_ELT(src, j);
    MEMCPY
  }
  return dest;
}

SEXP memcpyVectoradaptive(SEXP dest, SEXP src, SEXP offset, SEXP size) {
  size_t oi = INTEGER(offset)[0];
  int nrow = INTEGER(size)[oi-1];
  size_t o = oi - nrow; // oi should always be bigger than nrow because we filter out incomplete window using ansMask
  SEXP d = dest, s = src;
  SETLENGTH(d, nrow); // must be before MEMCPY because attempt to set index 1/1 in SET_STRING_ELT test 6010.150
  MEMCPY
  return dest;
}
SEXP memcpyDTadaptive(SEXP dest, SEXP src, SEXP offset, SEXP size) {
  size_t oi = INTEGER(offset)[0];
  int nrow = INTEGER(size)[oi-1];
  size_t o = oi - nrow;
  int ncol = LENGTH(dest);
  SEXP d, s;
  for (int j=0; j<ncol; ++j) {
    d = VECTOR_ELT(dest, j);
    s = VECTOR_ELT(src, j);
    SETLENGTH(d, nrow);
    MEMCPY
  }
  return dest;
}

// needed in adaptive=TRUE
SEXP setgrowable(SEXP x) {
  if (!isNewList(x)) {
    SET_GROWABLE_BIT(x);
    SET_TRUELENGTH(x, LENGTH(x)); // doesn't seems to be necessary but for future compatibility to R it is more safe to set it up
  } else {
    for (int i=0; i<LENGTH(x); ++i) {
      SEXP col = VECTOR_ELT(x, i);
      SET_GROWABLE_BIT(col);
      SET_TRUELENGTH(x, LENGTH(col));
    }
  }
  return x;
}
