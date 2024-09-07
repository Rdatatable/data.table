#include "data.table.h"

SEXP lagby(SEXP obj, SEXP g, SEXP objfill) {
  SEXP x, fill, el, f, thisfill, ig, tmp=R_NilValue, ans;
  int protecti=0;
	R_xlen_t i, j;
  if (isVectorAtomic(obj)) {
    x = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(x, 0, obj);
  } else {
    if (!isNewList(obj))
      error(_("type '%s' passed to x in lagbyv(). Must be a vector, list, data.frame or data.table"), type2char(TYPEOF(obj)));
    x = obj;
  }
  if (isVectorAtomic(objfill)) {
    fill = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(fill, 0, objfill);
  } else {
    if (!isNewList(objfill))
      error(_("type '%s' passed to fill in lagbyv(). Must be a list"), type2char(TYPEOF(objfill)));
    fill = objfill;
  }
  R_xlen_t nr = xlength(VECTOR_ELT(x, 0));
  R_len_t nc = length(x);
  ans = PROTECT(allocVector(VECSXP, nc)); protecti++;
  ig = PROTECT(coerceVector(g, INTSXP)); protecti++;
  for (j=0; j<nc; j++) {
    f = VECTOR_ELT(fill, j);
		el = VECTOR_ELT(x, j);
		R_xlen_t xrows = xlength(el);

		switch (TYPEOF(el)) {
		  case INTSXP :
				thisfill = PROTECT(coerceVector(f, INTSXP)); protecti++;
        SET_VECTOR_ELT(ans, j, tmp=allocVector(INTSXP, xrows));
        int *itmp = INTEGER(tmp);
        int *ix = INTEGER(el);
				int ifill = INTEGER(thisfill)[0];
				itmp[0] = ifill;
				for (i=1; i<nr; i++) {
					if (INTEGER(ig)[i] != INTEGER(ig)[i-1]) {
					  ifill = ix[i - 1];
					}
					itmp[i] = ifill;
				}
        copyMostAttrib(el, tmp);
        if (isFactor(el)) setAttrib(tmp, R_LevelsSymbol, getAttrib(el, R_LevelsSymbol));
				break;

			case REALSXP :
				thisfill = PROTECT(coerceVector(f, REALSXP)); protecti++;
				if (Rf_inherits(el, "integer64")) {
					int64_t *vd = (int64_t *)REAL(thisfill);
					if (INTEGER(f)[0] == NA_INTEGER)
						vd[0] = NA_INTEGER64;
					else
						vd[0] = (int64_t)REAL(thisfill)[0];
				} else {
				}
				SET_VECTOR_ELT(ans, j, tmp=allocVector(REALSXP, xrows));
				double *dtmp = REAL(tmp);
				double *dx = REAL(el);
				double dfill = REAL(thisfill)[0];
				dtmp[0] = dfill;
				for (i=1; i<nr; i++) {
					if (INTEGER(ig)[i] != INTEGER(ig)[i-1]) {
					  dfill = dx[i - 1];
					}
					dtmp[i] = dfill;
				}
        copyMostAttrib(el, tmp);
				break;

			case CPLXSXP :
				thisfill = PROTECT(coerceVector(f, CPLXSXP)); protecti++;
				SET_VECTOR_ELT(ans, j, tmp=allocVector(CPLXSXP, xrows));
				Rcomplex *ctmp = COMPLEX(tmp);
				Rcomplex *cx = COMPLEX(el);
				Rcomplex cfill = COMPLEX(thisfill)[0];
				ctmp[0] = cfill;
				for (i=1; i<nr; i++) {
					if (INTEGER(ig)[i] != INTEGER(ig)[i-1]) {
					  cfill = cx[i - 1];
					}
					ctmp[i] = cfill;
				}
        copyMostAttrib(el, tmp);
				break;

			case LGLSXP :
				thisfill = PROTECT(coerceVector(f, LGLSXP)); protecti++;
				SET_VECTOR_ELT(ans, j, tmp=allocVector(LGLSXP, xrows));
				int *ltmp = LOGICAL(tmp);
				int *lx = LOGICAL(el);
				int lfill = LOGICAL(thisfill)[0];
				ltmp[0] = lfill;
				for (i=1; i<nr; i++) {
					if (INTEGER(ig)[i] != INTEGER(ig)[i-1]) {
					  lfill = lx[i - 1];
					}
					ltmp[i] = lfill;
				}
        copyMostAttrib(el, tmp);
				break;

			case STRSXP :
				thisfill = PROTECT(coerceVector(f, STRSXP)); protecti++;
				SET_VECTOR_ELT(ans, j, tmp=allocVector(STRSXP, xrows));
				SET_STRING_ELT(tmp, 0, STRING_ELT(thisfill, 0));
				R_xlen_t k = 0;
				for (i=1; i<nr; i++) {
					if (k == 0 && INTEGER(ig)[i] == INTEGER(ig)[i-1]) {
						SET_STRING_ELT(tmp, i, STRING_ELT(thisfill, 0));
						continue;
					}
					if (INTEGER(ig)[i] != INTEGER(ig)[i-1]) {
					  k = i - 1;
					}
					SET_STRING_ELT(tmp, i, STRING_ELT(el, k));
				}
        copyMostAttrib(el, tmp);
				break;

			case VECSXP :
				thisfill = PROTECT(coerceVector(f, VECSXP)); protecti++;
				SET_VECTOR_ELT(ans, j, tmp=allocVector(VECSXP, xrows));
				SEXP vfill = VECTOR_ELT(thisfill, 0);
				SET_VECTOR_ELT(tmp, 0, VECTOR_ELT(thisfill, 0));
				for (i=1; i<nr; i++) {
					if (INTEGER(ig)[i] != INTEGER(ig)[i-1]) {
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
  return length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
