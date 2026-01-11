#include "data.table.h"
#include <Rdefines.h>
#include <time.h>

SEXP transpose(SEXP l, SEXP fill, SEXP ignoreArg, SEXP keepNamesArg, SEXP listColsArg)
{
  int nprotect = 0;
  if (!isNewList(l))
    error(_("l must be a list."));
  if (!length(l))
    return(copyAsPlain(l));
  if (!IS_TRUE_OR_FALSE(ignoreArg))
    error(_("'%s' must be TRUE or FALSE"), "ignore.empty");
  const bool ignore = LOGICAL_RO(ignoreArg)[0];
  if (!(isNull(keepNamesArg) || (isString(keepNamesArg) && LENGTH(keepNamesArg) == 1)))
    error(_("keep.names should be either NULL, or the name of the first column of the result in which to place the names of the input"));
  const bool rn = !isNull(keepNamesArg);
  if (length(fill) != 1)
    error(_("fill must be a length 1 vector, such as the default NA"));
  const R_len_t ln = LENGTH(l);
  if (!IS_TRUE_OR_FALSE(listColsArg))
    error(_("'%s' must be TRUE or FALSE"), "list.cols");
  const bool listCol = LOGICAL_RO(listColsArg)[0];

  // preprocessing
  int maxlen = 0, zerolen = 0;
  SEXPTYPE maxtype = 0;
  for (int i = 0; i < ln; i++) {
    SEXP li = VECTOR_ELT(l, i);
    if (!isVectorAtomic(li) && !isNull(li) && !isNewList(li))
      error(_("Item %d of list input is not either an atomic vector, or a list"), i + 1);
    const int len = length(li);
    if (len > maxlen) maxlen = len;
    zerolen += (len == 0);
    SEXPTYPE type = TYPEOF(li);
    if (isFactor(li)) type = STRSXP;
    if (type > maxtype) maxtype = type;
  }
  if (listCol) maxtype = VECSXP; // need to keep preprocessing for zerolen
  fill = PROTECT(coerceVector(fill, maxtype)); nprotect++;
  SEXP ans = PROTECT(allocVector(VECSXP, maxlen + rn)); nprotect++;
  const int anslen = (ignore) ? (ln - zerolen) : ln;
  if (rn) {
    SEXP tt;
    SET_VECTOR_ELT(ans, 0, tt = allocVector(STRSXP, anslen));
    SEXP lNames = PROTECT(getAttrib(l, R_NamesSymbol)); nprotect++;
    for (int i = 0, j = 0; i < ln; i++) {
      if (length(VECTOR_ELT(l, i))) SET_STRING_ELT(tt, j++, STRING_ELT(lNames, i));
    }
  }
  for (int i = 0; i < maxlen; i++) {
    SET_VECTOR_ELT(ans, i + rn, allocVector(maxtype, anslen));
  }
  const SEXP *ansp = SEXPPTR_RO(ans);
  for (int i = 0, k = 0; i < ln; i++) {
    SEXP li = VECTOR_ELT(l, i);
    const int len = length(li);
    if (ignore && len == 0) continue;
    if (TYPEOF(li) != maxtype) {
      li = PROTECT(isFactor(li) ? (listCol ? coerceVector(asCharacterFactor(li), VECSXP) : asCharacterFactor(li)) : coerceVector(li, maxtype));
    } else PROTECT(li); // extra PROTECT just to help rchk by avoiding two counter variables
    switch (maxtype) {
    case LGLSXP: {
      const int *ili = LOGICAL_RO(li);
      const int ifill = LOGICAL_RO(fill)[0];
      for (int j = 0; j < maxlen; j++) {
        LOGICAL(ansp[j + rn])[k] = j < len ? ili[j] : ifill;
      }
    } break;
    case INTSXP: {
      const int *ili = INTEGER_RO(li);
      const int ifill = INTEGER_RO(fill)[0];
      for (int j = 0; j < maxlen; j++) {
        INTEGER(ansp[j + rn])[k] = j < len ? ili[j] : ifill;
      }
    } break;
    case REALSXP: {
      const double *dli = REAL_RO(li);
      const double dfill = REAL_RO(fill)[0];
      for (int j = 0; j < maxlen; j++) {
        REAL(ansp[j + rn])[k] = j < len ? dli[j] : dfill;
      }
    } break;
    case STRSXP: {
      const SEXP sfill = STRING_ELT(fill, 0);
      for (int j = 0; j < maxlen; j++) {
        SET_STRING_ELT(ansp[j + rn], k, j < len ? STRING_ELT(li, j) : sfill);
      }
    } break;
    case VECSXP: {
      const SEXP vfill = VECTOR_ELT(fill, 0);
      for (int j = 0; j < maxlen; j++) {
        SET_VECTOR_ELT(ansp[j + rn], k, j < len ? VECTOR_ELT(li, j) : vfill);
      }
    } break;
    default:
      error(_("Unsupported column type '%s'"), type2char(maxtype));
    }
    UNPROTECT(1); // inside the loop to save the protection stack
    k++;
  }
  UNPROTECT(nprotect);
  return ans;
}
