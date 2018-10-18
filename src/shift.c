#include "data.table.h"
#include <Rdefines.h>
#include <time.h>

SEXP shift(SEXP obj, SEXP k, SEXP fill, SEXP type) {

  size_t size;
  R_len_t i=0, j, m, nx, nk, xrows, thisk, protecti=0;
  SEXP x, tmp=R_NilValue, this, ans, thisfill, class;
  unsigned long long *dthisfill;
  enum {LAG, LEAD} stype = LAG;
  if (!length(obj)) return(obj); // NULL, list()
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
  if (!isString(type) || length(type) != 1)
    error("type must be a character vector of length 1");

  if (!strcmp(CHAR(STRING_ELT(type, 0)), "lag"))  stype = LAG;
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "lead")) stype = LEAD;
  else error("Internal error: invalid type for shift(), should have been caught before. please report to data.table issue tracker"); // # nocov

  nx = length(x); nk = length(k);
  i=0;
  while(i < nk && INTEGER(k)[i] >= 0) i++;
  if (i != nk)
    error("n must be non-negative integer values (>= 0)");
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;
  if (stype == LAG) {
    for (i=0; i<nx; i++) {
      this  = VECTOR_ELT(x, i);
      size  = SIZEOF(this);
      xrows = length(this);
      switch (TYPEOF(this)) {
      case INTSXP :
        thisfill = PROTECT(coerceVector(fill, INTSXP)); protecti++;
        for (j=0; j<nk; j++) {
          thisk = (xrows >= INTEGER(k)[j]) ? INTEGER(k)[j] : xrows;
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(INTSXP, xrows) );
          if (xrows - INTEGER(k)[j] > 0)
            memcpy((char *)DATAPTR(tmp)+(INTEGER(k)[j]*size),
                 (char *)DATAPTR(this),
                 (xrows-INTEGER(k)[j])*size);
          for (m=0; m<thisk; m++)
            INTEGER(tmp)[m] = INTEGER(thisfill)[0];
          copyMostAttrib(this, tmp);
          if (isFactor(this))
            setAttrib(tmp, R_LevelsSymbol, getAttrib(this, R_LevelsSymbol));
        }
        break;

      case REALSXP :
        class = getAttrib(this, R_ClassSymbol);
        if (isString(class) && STRING_ELT(class, 0) == char_integer64) {
          thisfill = PROTECT(allocVector(REALSXP, 1)); protecti++;
          dthisfill = (unsigned long long *)REAL(thisfill);
          if (INTEGER(fill)[0] == NA_INTEGER)
            dthisfill[0] = NA_INT64_LL;
          else dthisfill[0] = (unsigned long long)INTEGER(fill)[0];
        } else {
          thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
        }
        for (j=0; j<nk; j++) {
          thisk = (xrows >= INTEGER(k)[j]) ? INTEGER(k)[j] : xrows;
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows) );
          if (xrows - INTEGER(k)[j] > 0) {
            memcpy((char *)DATAPTR(tmp)+(INTEGER(k)[j]*size),
                 (char *)DATAPTR(this),
                 (xrows-INTEGER(k)[j])*size);
          }
          for (m=0; m<thisk; m++) {
            REAL(tmp)[m] = REAL(thisfill)[0];
          }
          copyMostAttrib(this, tmp);
        }
        break;

      case LGLSXP :
        thisfill = PROTECT(coerceVector(fill, LGLSXP)); protecti++;
        for (j=0; j<nk; j++) {
          thisk = (xrows >= INTEGER(k)[j]) ? INTEGER(k)[j] : xrows;
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(LGLSXP, xrows) );
          if (xrows - INTEGER(k)[j] > 0)
            memcpy((char *)DATAPTR(tmp)+(INTEGER(k)[j]*size),
                 (char *)DATAPTR(this),
                 (xrows-INTEGER(k)[j])*size);
          for (m=0; m<thisk; m++)
            LOGICAL(tmp)[m] = LOGICAL(thisfill)[0];
          copyMostAttrib(this, tmp);
        }
        break;

      case STRSXP :
        thisfill = PROTECT(coerceVector(fill, STRSXP)); protecti++;
        for (j=0; j<nk; j++) {
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(STRSXP, xrows) );
          for (m=0; m<xrows; m++)
            SET_STRING_ELT(tmp, m, (m < INTEGER(k)[j]) ? STRING_ELT(thisfill, 0) : STRING_ELT(this, m - INTEGER(k)[j]));
          copyMostAttrib(this, tmp);
        }
        break;

      case VECSXP :
        thisfill = PROTECT(coerceVector(fill, VECSXP)); protecti++;
        for (j=0; j<nk; j++) {
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(VECSXP, xrows) );
          for (m=0; m<xrows; m++)
            SET_VECTOR_ELT(tmp, m, (m < INTEGER(k)[j]) ? VECTOR_ELT(thisfill, 0) : VECTOR_ELT(this, m - INTEGER(k)[j]));
          copyMostAttrib(this, tmp);
        }
        break;

      default :
        error("Unsupported type '%s'", type2char(TYPEOF(this)));
      }
      copyMostAttrib(this, tmp);
      if (isFactor(this))
        setAttrib(tmp, R_LevelsSymbol, getAttrib(this, R_LevelsSymbol));
    }
  } else if (stype == LEAD) {
    for (i=0; i<nx; i++) {
      this  = VECTOR_ELT(x, i);
      size  = SIZEOF(this);
      xrows = length(this);
      switch (TYPEOF(this)) {
      case INTSXP :
        thisfill = PROTECT(coerceVector(fill, INTSXP)); protecti++;
        for (j=0; j<nk; j++) {
          thisk = (xrows >= INTEGER(k)[j]) ? INTEGER(k)[j] : xrows;
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(INTSXP, xrows) );
          if (xrows - INTEGER(k)[j] > 0)
            memcpy((char *)DATAPTR(tmp),
                 (char *)DATAPTR(this)+(INTEGER(k)[j]*size),
                 (xrows-INTEGER(k)[j])*size);
          for (m=xrows-thisk; m<xrows; m++)
            INTEGER(tmp)[m] = INTEGER(thisfill)[0];
          copyMostAttrib(this, tmp);
          if (isFactor(this))
            setAttrib(tmp, R_LevelsSymbol, getAttrib(this, R_LevelsSymbol));
        }
        break;

      case REALSXP :
        class = getAttrib(this, R_ClassSymbol);
        if (isString(class) && STRING_ELT(class, 0) == char_integer64) {
          thisfill = PROTECT(allocVector(REALSXP, 1)); protecti++;
          dthisfill = (unsigned long long *)REAL(thisfill);
          if (INTEGER(fill)[0] == NA_INTEGER)
            dthisfill[0] = NA_INT64_LL;
          else dthisfill[0] = (unsigned long long)INTEGER(fill)[0];
        } else {
          thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
        }
        for (j=0; j<nk; j++) {
          thisk = (xrows >= INTEGER(k)[j]) ? INTEGER(k)[j] : xrows;
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows));
          if (xrows - INTEGER(k)[j] > 0)
            memcpy((char *)DATAPTR(tmp),
                 (char *)DATAPTR(this)+(INTEGER(k)[j]*size),
                 (xrows-INTEGER(k)[j])*size);
          for (m=xrows-thisk; m<xrows; m++)
            REAL(tmp)[m] = REAL(thisfill)[0];
          copyMostAttrib(this, tmp);
        }
        break;

      case LGLSXP :
        thisfill = PROTECT(coerceVector(fill, LGLSXP)); protecti++;
        for (j=0; j<nk; j++) {
          thisk = (xrows >= INTEGER(k)[j]) ? INTEGER(k)[j] : xrows;
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(LGLSXP, xrows) );
          if (xrows - INTEGER(k)[j] > 0)
            memcpy((char *)DATAPTR(tmp),
                 (char *)DATAPTR(this)+(INTEGER(k)[j]*size),
                 (xrows-INTEGER(k)[j])*size);
          for (m=xrows-thisk; m<xrows; m++)
            LOGICAL(tmp)[m] = LOGICAL(thisfill)[0];
          copyMostAttrib(this, tmp);
        }
        break;

      case STRSXP :
        thisfill = PROTECT(coerceVector(fill, STRSXP)); protecti++;
        for (j=0; j<nk; j++) {
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(STRSXP, xrows) );
          for (m=0; m<xrows; m++)
            SET_STRING_ELT(tmp, m, (xrows-m <= INTEGER(k)[j]) ? STRING_ELT(thisfill, 0) : STRING_ELT(this, m + INTEGER(k)[j]));
          copyMostAttrib(this, tmp);
        }
        break;

      case VECSXP :
        thisfill = PROTECT(coerceVector(fill, VECSXP)); protecti++;
        for (j=0; j<nk; j++) {
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(VECSXP, xrows) );
          for (m=0; m<xrows; m++)
            SET_VECTOR_ELT(tmp, m, (xrows-m <= INTEGER(k)[j]) ? VECTOR_ELT(thisfill, 0) : VECTOR_ELT(this, m + INTEGER(k)[j]));
            copyMostAttrib(this, tmp);
        }
        break;

      default :
        error("Unsupported type '%s'", type2char(TYPEOF(this)));
      }
    }
  }
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}

