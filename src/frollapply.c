#include "data.table.h"

static inline void memcpy_sexp(SEXP dest, size_t offset, SEXP src, int count) {
  switch (TYPEOF(dest)) {
  case INTSXP: {
    memcpy(INTEGER(dest), INTEGER_RO(src) + offset, count * sizeof(int));
  } break;
  case REALSXP: {
    memcpy(REAL(dest), REAL_RO(src) + offset, count * sizeof(double));
  } break;
  case STRSXP: {
    for (int i = 0; i < count; i++)
      SET_STRING_ELT(dest, i, STRING_ELT(src, offset + i));
  } break;
  case VECSXP: {
    for (int i = 0; i < count; i++)
      SET_VECTOR_ELT(dest, i, VECTOR_ELT(src, offset + i));
  } break;
  case CPLXSXP: {
    memcpy(COMPLEX(dest), COMPLEX_RO(src) + offset, count * sizeof(Rcomplex));
  } break;
  case RAWSXP: {
    memcpy(RAW(dest), RAW_RO(src) + offset, count * sizeof(Rbyte));
  } break;
  default: internal_error(__func__, "column type not supported in memcpyVector or memcpyDT function. All known types are supported so please report as bug."); // # nocov
  }
}

/*
 * memcpy src data into preallocated window
 * we don't call memcpyVector from memcpyDT because they are called in tight loop and we don't want to have extra branches inside
 */
SEXP memcpyVector(SEXP dest, SEXP src, SEXP offset, SEXP size) {
  memcpy_sexp(dest, INTEGER_RO(offset)[0] - INTEGER_RO(size)[0], src, LENGTH(dest));
  return dest;
}
// # nocov start ## does not seem to be reported to codecov most likely due to running in a fork, I manually debugged that it is being called when running froll.Rraw
SEXP memcpyDT(SEXP dest, SEXP src, SEXP offset, SEXP size) {
  //Rprintf("%d",1); // manual code coverage to confirm it is reached when marking nocov
  const int ncol = LENGTH(dest);
  const int nrow = LENGTH(VECTOR_ELT(dest, 0));
  for (int i = 0; i < ncol; i++) {
    memcpy_sexp(VECTOR_ELT(dest, i), INTEGER_RO(offset)[0] - INTEGER_RO(size)[0], VECTOR_ELT(src, i), nrow);
  }
  return dest;
}
// # nocov end

SEXP memcpyVectoradaptive(SEXP dest, SEXP src, SEXP offset, SEXP size) {
  const size_t oi = INTEGER_RO(offset)[0];
  const int nrow = INTEGER_RO(size)[oi - 1];
  const size_t o = oi - nrow; // oi should always be bigger than nrow because we filter out incomplete window using ansMask
  SETLENGTH(dest, nrow); // must be before memcpy_sexp because attempt to set index 1/1 in SET_STRING_ELT test 6010.150
  if (nrow) { // support k[i]==0
    memcpy_sexp(dest, o, src, nrow);
  }
  return dest;
}
// # nocov start ## does not seem to be reported to codecov most likely due to running in a fork, I manually debugged that it is being called when running froll.Rraw
SEXP memcpyDTadaptive(SEXP dest, SEXP src, SEXP offset, SEXP size) {
  //Rprintf("%d",2); // manual code coverage to confirm it is reached when marking nocov
  size_t oi = INTEGER_RO(offset)[0];
  const int nrow = INTEGER_RO(size)[oi - 1];
  const size_t o = oi - nrow;
  const int ncol = LENGTH(dest);
  for (int i = 0; i < ncol; i++) {
    SEXP d = VECTOR_ELT(dest, i);
    SETLENGTH(d, nrow);
    if (nrow) { // support k[i]==0
      memcpy_sexp(d, o, VECTOR_ELT(src, i), nrow);
    }
  }
  return dest;
}
// # nocov end

// needed in adaptive=TRUE
SEXP setgrowable(SEXP x) {
  if (!isNewList(x)) {
    SET_GROWABLE_BIT(x);
    SET_TRUELENGTH(x, LENGTH(x)); // important because gc() uses TRUELENGTH to keep counts
  } else {
    // # nocov start ## does not seem to be reported to codecov most likely due to running in a fork, I manually debugged that it is being called when running froll.Rraw
    for (int i = 0; i < LENGTH(x); i++) {
      //Rprintf("%d",3); // manual code coverage to confirm it is reached when marking nocov
      SEXP col = VECTOR_ELT(x, i);
      SET_GROWABLE_BIT(col);
      SET_TRUELENGTH(col, LENGTH(col));
    }
    // # nocov end
  }
  return x;
}
