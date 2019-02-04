#include "data.table.h"
#include <Rdefines.h>

SEXP shift(SEXP obj, SEXP k, SEXP fill, SEXP type) {

  size_t size;
  R_len_t i=0, j, nx, nk, thisk, protecti=0;
  R_xlen_t m, xrows;
  SEXP x, tmp=R_NilValue, elem, ans, thisfill, klass;
  unsigned long long *dthisfill;
  enum {LAG, LEAD/*, SHIFT, CYCLIC*/} stype = LAG; // currently SHIFT maps to LAG and CYCLIC is unimplemented (see comments in #1708)
  if (!xlength(obj)) return(obj); // NULL, list()
  if (isVectorAtomic(obj)) {
    x = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(x, 0, obj);
  } else x = obj;
  if (!isNewList(x))
    error("x must be a list, data.frame or data.table");
  if (!isInteger(k))
    error("Internal error: n must be integer"); // # nocov
  if (length(fill) != 1)
    error("fill must be a vector of length 1");
  // the following two errors should be caught by match.arg() at the R level
  if (!isString(type) || length(type) != 1)
    error("Internal error: invalid type for shift(), should have been caught before. please report to data.table issue tracker"); // # nocov

  if (!strcmp(CHAR(STRING_ELT(type, 0)), "lag")) stype = LAG;
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "lead")) stype = LEAD;
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "shift")) stype = LAG; // when we get rid of nested if branches we can use SHIFT, for now it maps to LAG
  else error("Internal error: invalid type for shift(), should have been caught before. please report to data.table issue tracker"); // # nocov

  nx = length(x); nk = length(k);

  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;
  for (i=0; i<nx; i++) {
    elem  = VECTOR_ELT(x, i);
    size  = SIZEOF(elem);
    xrows = xlength(elem);
    switch (TYPEOF(elem)) {
    case INTSXP :
      thisfill = PROTECT(coerceVector(fill, INTSXP)); protecti++;
      int ifill = INTEGER(thisfill)[0];
      for (j=0; j<nk; j++) {
        thisk = (INTEGER(k)[j] >= 0) ? INTEGER(k)[j] : -INTEGER(k)[j];
        thisk = (xrows >= thisk) ? thisk : xrows;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(INTSXP, xrows) );
        int *itmp = INTEGER(tmp);
        size_t shiftsize = thisk*size;
        size_t mvsize = (xrows-thisk)*size;
        if ((stype == LAG && INTEGER(k)[j] >= 0) || (stype == LEAD && INTEGER(k)[j] < 0)) {
          // LAG when type = 'lag' and n >= 0 _or_ type = 'lead' and n < 0
          if (xrows - thisk > 0) memmove((char *)itmp+shiftsize, (char *)INTEGER(elem), mvsize);
          for (m=0; m<thisk; m++) itmp[m] = ifill;
        } else {
          // only two possibilities left: type = 'lead', n>=0 _or_ type = 'lag', n<0
          if (xrows - thisk > 0) memmove((char *)itmp, (char *)INTEGER(elem)+shiftsize, mvsize);
          for (m=xrows-thisk; m<xrows; m++) itmp[m] = ifill;
        }
        copyMostAttrib(elem, tmp);
        if (isFactor(elem)) setAttrib(tmp, R_LevelsSymbol, getAttrib(elem, R_LevelsSymbol));
      }
      break;

    case REALSXP :
      klass = getAttrib(elem, R_ClassSymbol);
      if (isString(klass) && STRING_ELT(klass, 0) == char_integer64) {
        thisfill = PROTECT(allocVector(REALSXP, 1)); protecti++;
        dthisfill = (unsigned long long *)REAL(thisfill);
        if (INTEGER(fill)[0] == NA_INTEGER)
          dthisfill[0] = NA_INT64_LL;
        else dthisfill[0] = (unsigned long long)INTEGER(fill)[0];
      } else {
        thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
      }
      double dfill = REAL(thisfill)[0];
      for (j=0; j<nk; j++) {
        thisk = (INTEGER(k)[j] >= 0) ? INTEGER(k)[j] : -INTEGER(k)[j];
        thisk = (xrows >= thisk) ? thisk : xrows;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows) );
        double *dtmp = REAL(tmp);
        size_t shiftsize = thisk*size;
        size_t mvsize = (xrows-thisk)*size;
        if ((stype == LAG && INTEGER(k)[j] >= 0) || (stype == LEAD && INTEGER(k)[j] < 0)) {
          if (xrows - thisk > 0) memmove((char *)dtmp+shiftsize, (char *)REAL(elem), mvsize);
          for (m=0; m<thisk; m++) dtmp[m] = dfill;
        } else {
          if (xrows - thisk > 0) memmove((char *)dtmp, (char *)REAL(elem)+shiftsize, mvsize);
          for (m=xrows-thisk; m<xrows; m++) dtmp[m] = dfill;
        }
        copyMostAttrib(elem, tmp);
      }
      break;

    case LGLSXP :
      thisfill = PROTECT(coerceVector(fill, LGLSXP)); protecti++;
      int lfill = LOGICAL(thisfill)[0];
      for (j=0; j<nk; j++) {
        thisk = (INTEGER(k)[j] >= 0) ? INTEGER(k)[j] : -INTEGER(k)[j];
        thisk = (xrows >= thisk) ? thisk : xrows;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(LGLSXP, xrows) );
        int *ltmp = LOGICAL(tmp);
        size_t shiftsize = thisk*size;
        size_t mvsize = (xrows-thisk)*size;
        if ((stype == LAG && INTEGER(k)[j] >= 0) || (stype == LEAD && INTEGER(k)[j] < 0)) {
          if (xrows - thisk > 0) memmove((char *)ltmp+shiftsize, (char *)LOGICAL(elem), mvsize);
          for (m=0; m<thisk; m++) ltmp[m] = lfill;
        } else {
          if (xrows - thisk > 0) memmove((char *)ltmp, (char *)LOGICAL(elem)+shiftsize, mvsize);
          for (m=xrows-thisk; m<xrows; m++) ltmp[m] = lfill;
        }
        copyMostAttrib(elem, tmp);
      }
      break;

    case STRSXP :
      thisfill = PROTECT(coerceVector(fill, STRSXP)); protecti++;
      for (j=0; j<nk; j++) {
        thisk = (INTEGER(k)[j] >= 0) ? INTEGER(k)[j] : -INTEGER(k)[j];
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(STRSXP, xrows) );
        if ((stype == LAG && INTEGER(k)[j] >= 0) || (stype == LEAD && INTEGER(k)[j] < 0)) {
          for (m=0; m<xrows; m++) SET_STRING_ELT(tmp, m, (m < thisk) ? STRING_ELT(thisfill, 0) : STRING_ELT(elem, m - thisk));
        } else {
          for (m=0; m<xrows; m++) SET_STRING_ELT(tmp, m, (xrows-m <= thisk) ? STRING_ELT(thisfill, 0) : STRING_ELT(elem, m + thisk));
        }
        copyMostAttrib(elem, tmp);
      }
      break;

    case VECSXP :
      thisfill = PROTECT(coerceVector(fill, VECSXP)); protecti++;
      for (j=0; j<nk; j++) {
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(VECSXP, xrows) );
        thisk = (INTEGER(k)[j] >= 0) ? INTEGER(k)[j] : -INTEGER(k)[j];
        if ((stype == LAG && INTEGER(k)[j] >= 0) || (stype == LEAD && INTEGER(k)[j] < 0)) {
          for (m=0; m<xrows; m++) SET_VECTOR_ELT(tmp, m, (m < thisk) ? VECTOR_ELT(thisfill, 0) : VECTOR_ELT(elem, m - thisk));
        } else {
          for (m=0; m<xrows; m++) SET_VECTOR_ELT(tmp, m, (xrows-m <= thisk) ? VECTOR_ELT(thisfill, 0) : VECTOR_ELT(elem, m + thisk));
        }
        copyMostAttrib(elem, tmp);
      }
      break;

    default :
      error("Unsupported type '%s'", type2char(TYPEOF(elem)));
    }
  }

  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}

