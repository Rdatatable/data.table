#include "data.table.h"

SEXP growable_allocate(SEXPTYPE type, R_xlen_t size, R_xlen_t max_size) {
  SEXP ret = PROTECT(allocVector(type, max_size));
  SET_TRUELENGTH(ret, max_size);
#if R_VERSION >= R_Version(3, 4, 0)
  SET_GROWABLE_BIT(ret);
#endif // otherwise perceived memory use will end up higher
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

Rboolean is_growable(SEXP x) {
  return isVector(x) && TRUELENGTH(x) >= XLENGTH(x)
#if R_VERSION >= R_Version(3, 4, 0)
    && IS_GROWABLE(x)
#endif
  ;
}

// Assuming no ALTREP for now
SEXP make_growable(SEXP x) {
  if (TRUELENGTH(x) < XLENGTH(x)) SET_TRUELENGTH(x, XLENGTH(x));
  SET_GROWABLE_BIT(x);
  return x;
}
