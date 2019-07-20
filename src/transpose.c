#include "data.table.h"
#include <Rdefines.h>
#include <time.h>

SEXP transpose(SEXP l, SEXP fill, SEXP ignoreArg, SEXP keepRowNamesArg) {

  if (!isNewList(l))
    error("l must be a list.");
  if (!length(l))
    return(duplicate(l));
  if (!isLogical(ignoreArg) || LOGICAL(ignoreArg)[0] == NA_LOGICAL)
    error("ignore.empty should be logical TRUE/FALSE.");
  if (!isLogical(keepRowNamesArg) || LOGICAL(keepRowNamesArg)[0] == NA_LOGICAL)
    error("keep.rownames should be logical TRUE/FALSE.");
  if (length(fill) != 1)
    error("fill must be NULL or length=1 vector.");
  R_len_t ln = LENGTH(l);
  Rboolean ignore = LOGICAL(ignoreArg)[0];
  Rboolean keepRowNames = LOGICAL(keepRowNamesArg)[0];

  // preprocessing
  R_len_t *len  = (R_len_t *)R_alloc(ln, sizeof(R_len_t));
  int maxlen=0, zerolen=0;
  SEXPTYPE maxtype=0;
  // stop incrementing maxtype as soon as STRSXP is found
  bool hasStrType=false;
  for (int i=0; i<ln; ++i) {
    SEXP li = VECTOR_ELT(l, i);
    if (!isVectorAtomic(li) && !isNull(li))
      error("Item %d of list input is not an atomic vector", i+1);
    len[i] = length(li);
    if (len[i] > maxlen)
      maxlen = len[i];
    zerolen += (len[i] == 0);
    if (!hasStrType) {
      if (isFactor(li)) {
        maxtype = STRSXP;
        hasStrType = true;
      } else {
        SEXPTYPE type = TYPEOF(li);
        if (type == STRSXP) {
          maxtype = type;
          hasStrType = true;
          continue;
        }
        if (type > maxtype)
          maxtype = type;
      }
    }
  }
  // coerce fill to maxtype
  fill = PROTECT(coerceVector(fill, maxtype));

  // allocate 'ans'
  int dataStart= (int) keepRowNames;
  SEXP ans = PROTECT(allocVector(VECSXP, keepRowNames ? maxlen+1 : maxlen));
  int anslen = (ignore) ? (ln - zerolen) : ln;
  if (keepRowNames) {
    SEXP rowNames = PROTECT(allocVector(STRSXP, anslen));
    if (anslen < ln) {
      SEXP lNames = getAttrib(l, R_NamesSymbol);
      for (int i=0, j=0; i < ln; i++) {
        if (length(VECTOR_ELT(l, i)) != 0)
          SET_STRING_ELT(rowNames, j++, STRING_ELT(lNames, i));
      }
    } else {
      SET_VECTOR_ELT(ans, 0, getAttrib(l, R_NamesSymbol));
    }
    UNPROTECT(1);
  }
  for (int i=0; i<maxlen; ++i) {
    SET_VECTOR_ELT(ans, i+dataStart, allocVector(maxtype, anslen));
  }

  // transpose
  int k=0;
  for (int i=0; i<ln; ++i) {
    if (ignore && !len[i]) continue;
    SEXP li = VECTOR_ELT(l, i);
    bool coerce = false;
    if (TYPEOF(li) != maxtype) {
      coerce = true;
      li = PROTECT(isFactor(li) ? asCharacterFactor(li) : coerceVector(li, maxtype));
    }
    switch (maxtype) { // TODO remove more macros
    case INTSXP : {
      const int *ili = INTEGER(li);
      const int *ifill = INTEGER(fill);
      for (int j=0; j<maxlen; ++j) {
        SEXP thisi = VECTOR_ELT(ans, j+dataStart);
        INTEGER(thisi)[k] = (j < len[i]) ? ili[j] : ifill[0];
      }
    }
      break;
    case LGLSXP : {
      const int *ili = LOGICAL(li);
      const int *ifill = LOGICAL(fill);
      for (int j=0; j<maxlen; ++j) {
        SEXP thisi = VECTOR_ELT(ans, j+dataStart);
        LOGICAL(thisi)[k] = (j < len[i]) ? ili[j] : ifill[0];
      }
    }
      break;
    case REALSXP : {
      const double *dli = REAL(li);
      const double *dfill = REAL(fill);
      for (int j=0; j<maxlen; ++j) {
        SEXP thisi = VECTOR_ELT(ans, j+dataStart);
        REAL(thisi)[k] = (j < len[i]) ? dli[j] : dfill[0];
      }
    }
      break;
    case STRSXP :
      for (int j=0; j<maxlen; ++j) {
        SEXP thisi = VECTOR_ELT(ans, j+dataStart);
        SET_STRING_ELT(thisi, k, (j < len[i]) ? STRING_ELT(li, j) : STRING_ELT(fill, 0));
      }
      break;
    default :
        error("Unsupported column type '%s'", type2char(maxtype));
    }
    if (coerce) UNPROTECT(1);
    k++;
  }
  UNPROTECT(2);
  return(ans);
}
