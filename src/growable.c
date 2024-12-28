#include "data.table.h"

SEXP growable_allocate(SEXPTYPE type, R_xlen_t size, R_xlen_t max_size) {
  SEXP ret = PROTECT(allocVector(type, max_size));
  SET_TRUELENGTH(ret, max_size);
  SET_GROWABLE_BIT(ret);
  SETLENGTH(ret, size);
  UNPROTECT(1);
  return ret;
}

R_xlen_t growable_max_size(SEXP x) {
  return TRUELENGTH(x);
}

void growable_resize(SEXP x, R_xlen_t newsize) {
  R_xlen_t max_size;
  if (newsize > (max_size = growable_max_size(x))) internal_error(
    __func__, "newsize=%g > max_size=%g",
    (double)newsize, (double)max_size
  );
  SETLENGTH(x, newsize);
}
