#include "data.table.h"
#include <Rdefines.h>

SEXP shift(SEXP obj, SEXP k, SEXP fill, SEXP type) {

  size_t size;
  int protecti=0;
  SEXP x, tmp=R_NilValue, elem, ans, thisfill;
  unsigned long long *dthisfill;
  enum {LAG, LEAD/*, SHIFT, CYCLIC*/} stype = LAG; // currently SHIFT maps to LAG and CYCLIC is unimplemented (see comments in #1708)
  if (!xlength(obj)) return(obj); // NULL, list()
  if (isVectorAtomic(obj)) {
    x = PROTECT(allocVector(VECSXP, 1)); protecti++;
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

  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;
  for (int i=0; i<nx; i++) {
    elem  = VECTOR_ELT(x, i);
    size  = SIZEOF(elem);
    R_xlen_t xrows = xlength(elem);
    switch (TYPEOF(elem)) {
    case INTSXP :
      thisfill = PROTECT(coerceVector(fill, INTSXP)); protecti++;
      int ifill = INTEGER(thisfill)[0];
      for (int j=0; j<nk; j++) {
        R_xlen_t thisk = MIN(abs(kd[j]), xrows);
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
      break;

    case REALSXP :
      if (Rinherits(elem, char_integer64)) {
        thisfill = PROTECT(allocVector(REALSXP, 1)); protecti++;
        dthisfill = (unsigned long long *)REAL(thisfill);
        if (INTEGER(fill)[0] == NA_INTEGER)
          dthisfill[0] = NA_INT64_LL;
        else dthisfill[0] = (unsigned long long)INTEGER(fill)[0];
      } else {
        thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
      }
      double dfill = REAL(thisfill)[0];
      for (int j=0; j<nk; j++) {
        R_xlen_t thisk = MIN(abs(kd[j]), xrows);
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
      break;

    case CPLXSXP :
      thisfill = PROTECT(coerceVector(fill, CPLXSXP)); protecti++;
      Rcomplex cfill = COMPLEX(thisfill)[0];
      for (int j=0; j<nk; j++) {
        R_xlen_t thisk = MIN(abs(kd[j]), xrows);
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
      break;

    case LGLSXP :
      thisfill = PROTECT(coerceVector(fill, LGLSXP)); protecti++;
      int lfill = LOGICAL(thisfill)[0];
      for (int j=0; j<nk; j++) {
        R_xlen_t thisk = MIN(abs(kd[j]), xrows);
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
      break;

    case STRSXP :
      thisfill = PROTECT(coerceVector(fill, STRSXP)); protecti++;
      for (int j=0; j<nk; j++) {
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(STRSXP, xrows) );
        int thisk = abs(kd[j]);
        if ((stype == LAG && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          for (int m=0; m<xrows; m++) SET_STRING_ELT(tmp, m, (m < thisk) ? STRING_ELT(thisfill, 0) : STRING_ELT(elem, m - thisk));
        } else {
          for (int m=0; m<xrows; m++) SET_STRING_ELT(tmp, m, (xrows-m <= thisk) ? STRING_ELT(thisfill, 0) : STRING_ELT(elem, m + thisk));
        }
        copyMostAttrib(elem, tmp);
      }
      break;

    case VECSXP :
      thisfill = PROTECT(coerceVector(fill, VECSXP)); protecti++;
      for (int j=0; j<nk; j++) {
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(VECSXP, xrows) );
        int thisk = abs(kd[j]);
        if ((stype == LAG && kd[j] >= 0) || (stype == LEAD && kd[j] < 0)) {
          for (int m=0; m<xrows; m++) SET_VECTOR_ELT(tmp, m, (m < thisk) ? VECTOR_ELT(thisfill, 0) : VECTOR_ELT(elem, m - thisk));
        } else {
          for (int m=0; m<xrows; m++) SET_VECTOR_ELT(tmp, m, (xrows-m <= thisk) ? VECTOR_ELT(thisfill, 0) : VECTOR_ELT(elem, m + thisk));
        }
        copyMostAttrib(elem, tmp);
      }
      break;

    default :
      error(_("Unsupported type '%s'"), type2char(TYPEOF(elem)));
    }
  }

  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}

SEXP lagby(SEXP x, SEXP g, SEXP fill) {
  R_xlen_t nr = xlength(VECTOR_ELT(x, 0));
  R_len_t nc = length(x);
  SEXP el, f, sg, tmp=R_NilValue, ans;
  int protecti=0, i, j;

  ans = PROTECT(allocVector(VECSXP, nc)); protecti++;
  sg = PROTECT(coerceVector(g, INTSXP)); protecti++;
  for (j=0; j<nc; j++) {
    f = VECTOR_ELT(fill, j);
		el = VECTOR_ELT(x, j);
		R_xlen_t xrows = xlength(el);
		
		switch (TYPEOF(f)) {
		  case INTSXP :
        SET_VECTOR_ELT(ans, j, tmp=allocVector(INTSXP, xrows));
        int *itmp = INTEGER(tmp);
        int *ix = INTEGER(el);
				int ifill = INTEGER(f)[0];
				itmp[0] = ifill;
				for (i=1; i<nr; i++) {
					if (INTEGER(sg)[i] != INTEGER(sg)[i-1]) {
					  ifill = ix[i - 1];	
					}
					itmp[i] = ifill;
				}
        copyMostAttrib(el, tmp);
        if (isFactor(el)) setAttrib(tmp, R_LevelsSymbol, getAttrib(el, R_LevelsSymbol));
				break;
				
			case REALSXP :
			  if (Rinherits(f, char_integer64)) {
					unsigned long long *ullfill = (unsigned long long *)REAL(f);
					if (INTEGER(f)[0] == NA_INTEGER)
						ullfill[0] = NA_INT64_LL;
					else
						ullfill[0] = (unsigned long long)INTEGER(f)[0];
				}
				SET_VECTOR_ELT(ans, j, tmp=allocVector(REALSXP, xrows));
				double *dtmp = REAL(tmp);
				double *dx = REAL(el);
				double dfill = REAL(f)[0];
				dtmp[0] = dfill;
				for (i=1; i<nr; i++) {
					if (INTEGER(sg)[i] != INTEGER(sg)[i-1]) {
					  dfill = dx[i - 1];	
					}
					dtmp[i] = dfill;					
				}
        copyMostAttrib(el, tmp);
				break;
			
			case CPLXSXP :
				SET_VECTOR_ELT(ans, j, tmp=allocVector(CPLXSXP, xrows));
				Rcomplex *ctmp = COMPLEX(tmp);
				Rcomplex *cx = COMPLEX(el);
				Rcomplex cfill = COMPLEX(f)[0];
				ctmp[0] = cfill;
				for (i=1; i<nr; i++) {
					if (INTEGER(sg)[i] != INTEGER(sg)[i-1]) {
					  cfill = cx[i - 1];	
					}
					ctmp[i] = cfill;					
				}
        copyMostAttrib(el, tmp);
				break;
			
			case LGLSXP :
				SET_VECTOR_ELT(ans, j, tmp=allocVector(LGLSXP, xrows));
				int *ltmp = LOGICAL(tmp);
				int *lx = LOGICAL(el);
				int lfill = LOGICAL(f)[0];
				ltmp[0] = lfill;
				for (i=1; i<nr; i++) {
					if (INTEGER(sg)[i] != INTEGER(sg)[i-1]) {
					  lfill = lx[i - 1];	
					}
					ltmp[i] = lfill;					
				}
        copyMostAttrib(el, tmp);
				break;
			
			case STRSXP :
				SET_STRING_ELT(ans, j, tmp=allocVector(STRSXP, xrows));
				const char *sfill = CHAR(STRING_ELT(f, 0));
				SET_STRING_ELT(tmp, 0, STRING_ELT(f, 0));
				for (i=1; i<nr; i++) {
					if (INTEGER(sg)[i] != INTEGER(sg)[i-1]) {
					  sfill = CHAR(STRING_ELT(el, i - 1));	
					}
					SET_STRING_ELT(tmp, i, mkChar(sfill));					
				}
        copyMostAttrib(el, tmp);
				break;
			
			case VECSXP :
				SET_VECTOR_ELT(ans, j, tmp=allocVector(VECSXP, xrows));
				SEXP vfill = VECTOR_ELT(f, 0);
				SET_VECTOR_ELT(tmp, 0, VECTOR_ELT(f, 0));
				for (i=1; i<nr; i++) {
					if (INTEGER(sg)[i] != INTEGER(sg)[i-1]) {
					  vfill = VECTOR_ELT(el, i - 1);	
					}
					SET_VECTOR_ELT(tmp, i, vfill);					
				}
        copyMostAttrib(el, tmp);
				break;
			
			default:
			  error("Unsupported type '%s'", type2char(TYPEOF(el)));
		}
  }		

  UNPROTECT(protecti);
  return isVectorAtomic(x) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;	
}