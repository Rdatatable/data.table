#include "data.table.h"
#include <Rdefines.h>
#include <time.h>

SEXP rollmean(SEXP obj, SEXP k) {

  size_t size;
  R_len_t i=0;
  SEXP x, ans;
  if (isVectorAtomic(obj)) {
    x = allocVector(VECSXP, 1);
    SET_VECTOR_ELT(x, 0, obj);
  } else x = obj;
  if (!isNewList(x))
    error("x must be a list, data.frame or data.table");
  if (!isInteger(k))
    error("Internal error: n must be integer");

  return(ans);
}
