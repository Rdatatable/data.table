#include "data.table.h"
#include <Rdefines.h>

SEXP shift(SEXP obj, SEXP k, SEXP fill, SEXP type)
{
  int nprotect=0;
  enum {LAG, LEAD/*, SHIFT*/,CYCLIC} stype = LAG; // currently SHIFT maps to LAG (see comments in #1708)
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
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "cyclic")) stype = CYCLIC;
  else error(_("Internal error: invalid type for shift(), should have been caught before. please report to data.table issue tracker")); // # nocov

  int nx = length(x), nk = length(k);
  if (!isInteger(k)) error(_("Internal error: k must be integer")); // # nocov
  const int *kd = INTEGER(k);
  for (int i=0; i<nk; i++) if (kd[i]==NA_INTEGER) error(_("Item %d of n is NA"), i+1);  // NA crashed (#3354); n is called k at C level

  const bool cycle = stype == CYCLIC;

  SEXP ans = PROTECT(allocVector(VECSXP, nk * nx)); nprotect++;
  for (int i=0; i<nx; i++) {
    SEXP elem  = VECTOR_ELT(x, i);
    size_t size  = SIZEOF(elem);
    R_xlen_t xrows = xlength(elem);
    SEXP thisfill = PROTECT(coerceAs(fill, elem, ScalarLogical(0))); nprotect++;  // #4865 use coerceAs for type coercion
    switch (TYPEOF(elem)) {
    case INTSXP : {
      const int ifill = INTEGER(thisfill)[0];
      for (int j=0; j<nk; j++) {
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(INTSXP, xrows) );
        const int *restrict ielem = INTEGER(elem);
        int *restrict itmp = INTEGER(tmp);
        size_t thisk = cycle ? abs(kd[j]) % xrows : MIN(abs(kd[j]), xrows);
        size_t tailk = xrows-thisk;
        if (((stype == LAG || stype == CYCLIC) && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          // LAG when type %in% c('lag','cyclic') and n >= 0 _or_ type = 'lead' and n < 0
          if (tailk > 0) memmove(itmp+thisk, ielem, tailk*size);
          if (cycle) {
            if (thisk > 0) memmove(itmp, ielem+tailk, thisk*size);
          } else for (int m=0; m<thisk; m++) itmp[m] = ifill;
        } else {
          // only two possibilities left: type = 'lead', n>=0 _or_ type %in% c('lag','cyclic'), n<0
          if (tailk > 0) memmove(itmp, ielem+thisk, tailk*size);
          if (cycle) {
            if (thisk > 0) memmove(itmp+tailk, ielem, thisk*size);
          } else for (int m=tailk; m<xrows; m++) itmp[m] = ifill;
        }
        copyMostAttrib(elem, tmp);
        if (isFactor(elem)) setAttrib(tmp, R_LevelsSymbol, getAttrib(elem, R_LevelsSymbol));
      }
    } break;
    case REALSXP : {
      const double dfill = REAL(thisfill)[0];
      for (int j=0; j<nk; j++) {
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows) );
        const double *restrict delem = REAL(elem);
        double *restrict dtmp = REAL(tmp);
        size_t thisk = cycle ? abs(kd[j]) % xrows : MIN(abs(kd[j]), xrows);
        size_t tailk = xrows-thisk;
        if (((stype == LAG || stype == CYCLIC) && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          if (tailk > 0) memmove(dtmp+thisk, delem, tailk*size);
          if (cycle) {
            if (thisk > 0) memmove(dtmp, delem+tailk, thisk*size);
          } else for (int m=0; m<thisk; m++) dtmp[m] = dfill;
        } else {
          if (tailk > 0) memmove(dtmp, delem+thisk, tailk*size);
          if (cycle) {
            if (thisk > 0) memmove(dtmp+tailk, delem, thisk*size);
          } else for (int m=tailk; m<xrows; m++) dtmp[m] = dfill;
        }
        copyMostAttrib(elem, tmp);
      }
    } break;
    case CPLXSXP : {
      const Rcomplex cfill = COMPLEX(thisfill)[0];
      for (int j=0; j<nk; j++) {
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(CPLXSXP, xrows) );
        const Rcomplex *restrict celem = COMPLEX(elem);
        Rcomplex *restrict ctmp = COMPLEX(tmp);
        size_t thisk = cycle ? abs(kd[j]) % xrows : MIN(abs(kd[j]), xrows);
        size_t tailk = xrows-thisk;
        if (((stype == LAG || stype == CYCLIC) && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          if (tailk > 0) memmove(ctmp+thisk, celem, tailk*size);
          if (cycle) {
            if (thisk > 0) memmove(ctmp, celem+tailk, thisk*size);
          } else for (int m=0; m<thisk; m++) ctmp[m] = cfill;
        } else {
          if (tailk > 0) memmove(ctmp, celem+thisk, tailk*size);
          if (cycle) {
            if (thisk > 0) memmove(ctmp+tailk, celem, thisk*size);
          } else for (int m=tailk; m<xrows; m++) ctmp[m] = cfill;
        }
        copyMostAttrib(elem, tmp);
      }
    } break;
    case LGLSXP : {
      const int lfill = LOGICAL(thisfill)[0];
      for (int j=0; j<nk; j++) {
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(LGLSXP, xrows) );
        const int *restrict lelem = LOGICAL(elem);
        int *restrict ltmp = LOGICAL(tmp);
        size_t thisk = cycle ? abs(kd[j]) % xrows : MIN(abs(kd[j]), xrows);
        size_t tailk = xrows-thisk;
        if (((stype == LAG || stype == CYCLIC) && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          if (tailk > 0) memmove(ltmp+thisk, lelem, tailk*size);
          if (cycle) {
            if (thisk > 0) memmove(ltmp, lelem+tailk, thisk*size);
          } else for (int m=0; m<thisk; m++) ltmp[m] = cycle ? lelem[m+tailk] : lfill;
        } else {
          if (tailk > 0) memmove(ltmp, lelem+thisk, tailk*size);
          if (cycle) {
            if (thisk > 0) memmove(ltmp+tailk, lelem, thisk*size);
          } else for (int m=tailk; m<xrows; m++) ltmp[m] = cycle ? lelem[m-tailk] : lfill;
        }
        copyMostAttrib(elem, tmp);
      }
    } break;
    case STRSXP : {
      const SEXP sfill = STRING_ELT(thisfill, 0);
      for (int j=0; j<nk; j++) {
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(STRSXP, xrows) );
        int thisk = abs(kd[j]);
        if (cycle) thisk %= xrows;
        if (((stype == LAG || stype == CYCLIC) && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          for (int m=0; m<xrows; m++) SET_STRING_ELT(tmp, m, (m < thisk) ? (cycle ? STRING_ELT(elem, m+xrows-thisk) : sfill) : STRING_ELT(elem, m - thisk));
        } else {
          for (int m=0; m<xrows; m++) SET_STRING_ELT(tmp, m, (xrows-m <= thisk) ? (cycle ? STRING_ELT(elem, m-xrows+thisk) : sfill) : STRING_ELT(elem, m + thisk));
        }
        copyMostAttrib(elem, tmp);
      }
    } break;
    case VECSXP : {
      const SEXP vfill = VECTOR_ELT(thisfill, 0);
      for (int j=0; j<nk; j++) {
        SEXP tmp;
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(VECSXP, xrows) );
        int thisk = abs(kd[j]);
        if (cycle) thisk %= xrows;
        if (((stype == LAG || stype == CYCLIC) && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          for (int m=0; m<xrows; m++) SET_VECTOR_ELT(tmp, m, (m < thisk) ? (cycle ? VECTOR_ELT(elem, m+xrows-thisk) : vfill) : VECTOR_ELT(elem, m - thisk));
        } else {
          for (int m=0; m<xrows; m++) SET_VECTOR_ELT(tmp, m, (xrows-m <= thisk) ? (cycle ? VECTOR_ELT(elem, m-xrows+thisk) : vfill) : VECTOR_ELT(elem, m + thisk));
        }
        copyMostAttrib(elem, tmp);
      }
    } break;
    default :
      error(_("Type '%s' is not supported"), type2char(TYPEOF(elem)));
    }
  }
  UNPROTECT(nprotect);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}

