#include "data.table.h"
#include <Rdefines.h>
#include <time.h>

SEXP transpose(SEXP l, SEXP fill, SEXP ignoreArg, SEXP keepNamesArg) {

  int nprotect=0;
  if (!isNewList(l))
    error(_("l must be a list."));
  if (!length(l))
    return(copyAsPlain(l));
  if (!isLogical(ignoreArg) || LOGICAL(ignoreArg)[0]==NA_LOGICAL)
    error(_("ignore.empty should be logical TRUE/FALSE."));
  bool ignore = LOGICAL(ignoreArg)[0];
  if (!(isNull(keepNamesArg) || (isString(keepNamesArg) && LENGTH(keepNamesArg)==1)))
    error(_("keep.names should be either NULL, or the name of the first column of the result in which to place the names of the input"));
  bool rn = !isNull(keepNamesArg);
  if (length(fill) != 1)
    error(_("fill must be a length 1 vector, such as the default NA"));
  R_len_t ln = LENGTH(l);

  // preprocessing
  int maxlen=0, zerolen=0;
  SEXPTYPE maxtype=0;
  for (int i=0; i<ln; ++i) {
    SEXP li = VECTOR_ELT(l, i);
    if (!isVectorAtomic(li) && !isNull(li))
      error(_("Item %d of list input is not an atomic vector"), i+1);
    const int len = length(li);
    if (len>maxlen) maxlen=len;
    zerolen += (len==0);
    SEXPTYPE type = TYPEOF(li);
    if (isFactor(li)) type=STRSXP;
    if (type>maxtype) maxtype=type;
  }
  fill = PROTECT(coerceVector(fill, maxtype)); nprotect++;

  SEXP ans = PROTECT(allocVector(VECSXP, maxlen+rn)); nprotect++;
  int anslen = (ignore) ? (ln - zerolen) : ln;
  if (rn) {
    SEXP tt;
    SET_VECTOR_ELT(ans, 0, tt=allocVector(STRSXP, anslen));
    SEXP lNames = PROTECT(getAttrib(l, R_NamesSymbol)); nprotect++;
    for (int i=0, j=0; i<ln; ++i) {
      if (length(VECTOR_ELT(l, i))) SET_STRING_ELT(tt, j++, STRING_ELT(lNames, i));
    }
  }
  for (int i=0; i<maxlen; ++i) {
    SET_VECTOR_ELT(ans, i+rn, allocVector(maxtype, anslen));
  }
  const SEXP *ansp = SEXPPTR_RO(ans);
  for (int i=0, k=0; i<ln; ++i) {
    SEXP li = VECTOR_ELT(l, i);
    const int len = length(li);
    if (ignore && len==0) continue;
    if (TYPEOF(li) != maxtype) {
      li = PROTECT(isFactor(li) ? asCharacterFactor(li) : coerceVector(li, maxtype));
    } else PROTECT(li); // extra PROTECT just to help rchk by avoiding two counter variables
    switch (maxtype) {
    case LGLSXP : {
      const int *ili = LOGICAL(li);
      const int ifill = LOGICAL(fill)[0];
      for (int j=0; j<maxlen; ++j) {
        LOGICAL(ansp[j+rn])[k] = j<len ? ili[j] : ifill;
      }
    } break;
    case INTSXP : {
      const int *ili = INTEGER(li);
      const int ifill = INTEGER(fill)[0];
      for (int j=0; j<maxlen; ++j) {
        INTEGER(ansp[j+rn])[k] = j<len ? ili[j] : ifill;
      }
    } break;
    case REALSXP : {
      const double *dli = REAL(li);
      const double dfill = REAL(fill)[0];
      for (int j=0; j<maxlen; ++j) {
        REAL(ansp[j+rn])[k] = j<len ? dli[j] : dfill;
      }
    } break;
    case STRSXP : {
      const SEXP sfill = STRING_ELT(fill, 0);
      for (int j=0; j<maxlen; ++j) {
        SET_STRING_ELT(ansp[j+rn], k, j<len ? STRING_ELT(li, j) : sfill);
      }
    } break;
    default :
      error(_("Unsupported column type '%s'"), type2char(maxtype));
    }
    UNPROTECT(1); // inside the loop to save the protection stack
    k++;
  }
  UNPROTECT(nprotect);
  return ans;
}

