#include "data.table.h"
#include <Rdefines.h>

SEXP shift(SEXP obj, SEXP k, SEXP fill, SEXP type)
{
  int nprotect=0;
  enum {LAG, LEAD/*, SHIFT, CYCLIC*/} stype = LAG; // currently SHIFT maps to LAG and CYCLIC is unimplemented (see comments in #1708)
  if (!xlength(obj)) return(obj); // NULL, list()
  SEXP x;
  if (isVectorAtomic(obj)) {
    x = PROTECT(allocVector(VECSXP, 1)); nprotect++;
    SET_VECTOR_ELT(x, 0, obj);
  } else {
    if (!isNewList(obj))
      error(_("type '%s' passed to shift(). Must be a vector, list, data.frame or data.table"), type2char(TYPEOF(obj)));
    x = obj;
  }
  if (length(fill) != 1)
    error(_("fill must be a vector of length 1"));
  // the following two errors should be caught by match.arg() at the R level
  if (!isString(type) || length(type) != 1)
    error(_("Internal error: invalid type for shift(), should have been caught before. please report to data.table issue tracker")); // # nocov
  if (!strcmp(CHAR(STRING_ELT(type, 0)), "lag")) stype = LAG;
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "lead")) stype = LEAD;
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "shift")) stype = LAG; // when we get rid of nested if branches we can use SHIFT, for now it maps to LAG
  else error(_("Internal error: invalid type for shift(), should have been caught before. please report to data.table issue tracker")); // # nocov

  int nx = length(x), nk = length(k);
  if (!isInteger(k)) error(_("Internal error: k must be integer")); // # nocov
  const int *kd = INTEGER(k);
  for (int i=0; i<nk; i++) if (kd[i]==NA_INTEGER) error(_("Item %d of n is NA"), i+1);  // NA crashed (#3354); n is called k at C level

  SEXP ans = PROTECT(allocVector(VECSXP, nk * nx)); nprotect++;
  for (int i=0; i<nx; i++) {
    SEXP elem  = VECTOR_ELT(x, i);
    size_t size  = SIZEOF(elem);
    R_xlen_t xrows = xlength(elem);
    switch (TYPEOF(elem)) {
    case INTSXP : {
      SEXP thisfill = PROTECT(coerceVector(fill, INTSXP));
      const int ifill = INTEGER(thisfill)[0];
      UNPROTECT(1);
      for (int j=0; j<nk; j++) {
        R_xlen_t thisk = MIN(abs(kd[j]), xrows);
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(INTSXP, xrows) );
        int *itmp = INTEGER(tmp);
        size_t tailk = xrows-thisk;
        if ((stype == LAG && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          // LAG when type = 'lag' and n >= 0 _or_ type = 'lead' and n < 0
          if (tailk > 0) memmove(itmp+thisk, INTEGER(elem), tailk*size);
          for (int m=0; m<thisk; m++) itmp[m] = ifill;
        } else {
          // only two possibilities left: type = 'lead', n>=0 _or_ type = 'lag', n<0
          if (tailk > 0) memmove(itmp, INTEGER(elem)+thisk, tailk*size);
          for (int m=xrows-thisk; m<xrows; m++) itmp[m] = ifill;
        }
        copyMostAttrib(elem, tmp);
        if (isFactor(elem)) setAttrib(tmp, R_LevelsSymbol, getAttrib(elem, R_LevelsSymbol));
      }
    } break;

    case REALSXP : {
      SEXP thisfill;
      if (Rinherits(elem, char_integer64)) {
        thisfill = PROTECT(allocVector(REALSXP, 1));
        unsigned long long *dthisfill = (unsigned long long *)REAL(thisfill);
        if (INTEGER(fill)[0] == NA_INTEGER)
          dthisfill[0] = NA_INT64_LL;
        else dthisfill[0] = (unsigned long long)INTEGER(fill)[0];
      } else {
        thisfill = PROTECT(coerceVector(fill, REALSXP));
      }
      const double dfill = REAL(thisfill)[0];
      UNPROTECT(1); // thisfill
      for (int j=0; j<nk; j++) {
        R_xlen_t thisk = MIN(abs(kd[j]), xrows);
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows) );
        double *dtmp = REAL(tmp);
        size_t tailk = xrows-thisk;
        if ((stype == LAG && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          if (tailk > 0) memmove(dtmp+thisk, REAL(elem), tailk*size);
          for (int m=0; m<thisk; m++) dtmp[m] = dfill;
        } else {
          if (tailk > 0) memmove(dtmp, REAL(elem)+thisk, tailk*size);
          for (int m=tailk; m<xrows; m++) dtmp[m] = dfill;
        }
        copyMostAttrib(elem, tmp);
      }
    } break;

    case CPLXSXP : {
      SEXP thisfill = PROTECT(coerceVector(fill, CPLXSXP));
      const Rcomplex cfill = COMPLEX(thisfill)[0];
      UNPROTECT(1);
      for (int j=0; j<nk; j++) {
        R_xlen_t thisk = MIN(abs(kd[j]), xrows);
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(CPLXSXP, xrows) );
        Rcomplex *ctmp = COMPLEX(tmp);
        size_t tailk = xrows-thisk;
        if ((stype == LAG && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          if (tailk > 0) memmove(ctmp+thisk, COMPLEX(elem), tailk*size);
          for (int m=0; m<thisk; m++) ctmp[m] = cfill;
        } else {
          if (tailk > 0) memmove(ctmp, COMPLEX(elem)+thisk, tailk*size);
          for (int m=tailk; m<xrows; m++) ctmp[m] = cfill;
        }
        copyMostAttrib(elem, tmp);
      }
    } break;

    case LGLSXP : {
      SEXP thisfill = PROTECT(coerceVector(fill, LGLSXP));
      const int lfill = LOGICAL(thisfill)[0];
      UNPROTECT(1);
      for (int j=0; j<nk; j++) {
        R_xlen_t thisk = MIN(abs(kd[j]), xrows);
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(LGLSXP, xrows) );
        int *ltmp = LOGICAL(tmp);
        size_t tailk = xrows-thisk;
        if ((stype == LAG && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          if (tailk > 0) memmove(ltmp+thisk, LOGICAL(elem), tailk*size);
          for (int m=0; m<thisk; m++) ltmp[m] = lfill;
        } else {
          if (tailk > 0) memmove(ltmp, LOGICAL(elem)+thisk, tailk*size);
          for (int m=tailk; m<xrows; m++) ltmp[m] = lfill;
        }
        copyMostAttrib(elem, tmp);
      }
    } break;

    case STRSXP : {
      SEXP thisfill = PROTECT(coerceVector(fill, STRSXP));
      const SEXP sfill = STRING_ELT(thisfill, 0);
      for (int j=0; j<nk; j++) {
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(STRSXP, xrows) );
        int thisk = abs(kd[j]);
        if ((stype == LAG && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          for (int m=0; m<xrows; m++) SET_STRING_ELT(tmp, m, (m < thisk) ? sfill : STRING_ELT(elem, m - thisk));
        } else {
          for (int m=0; m<xrows; m++) SET_STRING_ELT(tmp, m, (xrows-m <= thisk) ? sfill : STRING_ELT(elem, m + thisk));
        }
        copyMostAttrib(elem, tmp);
      }
      UNPROTECT(1);
    } break;

    case VECSXP : {
      SEXP thisfill = PROTECT(coerceVector(fill, VECSXP));
      const SEXP vfill = VECTOR_ELT(thisfill, 0);
      for (int j=0; j<nk; j++) {
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(VECSXP, xrows) );
        int thisk = abs(kd[j]);
        if ((stype == LAG && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          for (int m=0; m<xrows; m++) SET_VECTOR_ELT(tmp, m, (m < thisk) ? vfill : VECTOR_ELT(elem, m - thisk));
        } else {
          for (int m=0; m<xrows; m++) SET_VECTOR_ELT(tmp, m, (xrows-m <= thisk) ? vfill : VECTOR_ELT(elem, m + thisk));
        }
        copyMostAttrib(elem, tmp);
      }
      UNPROTECT(1);
    } break;

    default :
      error(_("Unsupported type '%s'"), type2char(TYPEOF(elem)));
    }
  }

  UNPROTECT(nprotect);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}

