#include "data.table.h"
#include <Rdefines.h>

SEXP colnamesInt(SEXP x, SEXP cols) {
  if (!isNewList(x)) error("'x' argument must be data.table");
  int protecti=0;
  R_len_t nx = length(x);
  SEXP ricols = R_NilValue;
  if (isNull(cols)) { // seq_along(x)
    ricols = PROTECT(allocVector(INTSXP, nx)); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<nx; i++) icols[i] = i+1;
    UNPROTECT(protecti);
    return ricols;
  }
  if (length(cols)==0) { // integer(0)
    ricols = PROTECT(allocVector(INTSXP, 0)); protecti++;
    UNPROTECT(protecti);
    return ricols;
  }
  if (isInteger(cols) || isReal(cols)) {
    if (isInteger(cols)) {
      ricols = cols;
    } else if (isReal(cols)) {
      ricols = PROTECT(coerceVector(cols, INTSXP)); protecti++;
    }
    int *icols = INTEGER(ricols);
    for (int i=0; i<length(ricols); i++) if ((icols[i]>nx) || (icols[i]<1)) error("'cols' argument specify non existing column(s)"); // handles NAs also
  } else if (isString(cols)) {
    SEXP xnames = PROTECT(getAttrib(x, R_NamesSymbol)); protecti++;
    if (isNull(xnames)) error("'x' argument data.table has no names");
    ricols = PROTECT(chmatch(cols, xnames, 0)); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<length(ricols); i++) if (icols[i]==0) error("'cols' argument specify non existing column(s)"); // handles NAs also
  } else {
    error("'cols' argument must be character or numeric");
  }
  if (any_duplicated(ricols, FALSE)) error("'cols' argument specify duplicated column(s)");
  UNPROTECT(protecti);
  return ricols;
}

void nafillDouble(double *x, uint_fast64_t nx, unsigned int type, double fill, ans_t *ans, bool verbose) {
  double tic=0.0;
  if (verbose) tic = omp_get_wtime();
  if (type==0) { // const
    for (uint_fast64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = ISNA(x[i]) ? fill : x[i];
    }
  } else if (type==1) { // locf
    ans->dbl_v[0] = x[0];
    for (uint_fast64_t i=1; i<nx; i++) {
      ans->dbl_v[i] = ISNA(x[i]) ? ans->dbl_v[i-1] : x[i];
    }
  } else if (type==2) { // nocb
    ans->dbl_v[nx-1] = x[nx-1];
    for (int_fast64_t i=nx-2; i>=0; i--) {
      ans->dbl_v[i] = ISNA(x[i]) ? ans->dbl_v[i+1] : x[i];
    }
  }
  if (verbose) snprintf(ans->message[0], 500, "%s: took %.3fs\n", __func__, omp_get_wtime()-tic);
}

void nafillInteger(int32_t *x, uint_fast64_t nx, unsigned int type, int32_t fill, ans_t *ans, bool verbose) {
  double tic=0.0;
  if (verbose) tic = omp_get_wtime();
  if (type==0) { // const
    for (uint_fast64_t i=0; i<nx; i++) {
      ans->int_v[i] = x[i]==NA_INTEGER ? fill : x[i];
    }
  } else if (type==1) { // locf
    ans->int_v[0] = x[0];
    for (uint_fast64_t i=1; i<nx; i++) {
      ans->int_v[i] = x[i]==NA_INTEGER ? ans->int_v[i-1] : x[i];
    }
  } else if (type==2) { // nocb
    ans->int_v[nx-1] = x[nx-1];
    for (int_fast64_t i=nx-2; i>=0; i--) {
      ans->int_v[i] = x[i]==NA_INTEGER ? ans->int_v[i+1] : x[i];
    }
  }
  if (verbose) snprintf(ans->message[0], 500, "%s: took %.3fs\n", __func__, omp_get_wtime()-tic);
}

SEXP nafillR(SEXP obj, SEXP type, SEXP fill, SEXP inplace, SEXP cols, SEXP verbose) {
  int protecti=0;
  if (!isLogical(verbose) || length(verbose)!=1 || LOGICAL(verbose)[0]==NA_LOGICAL)
    error("verbose must be TRUE or FALSE");
  bool bverbose = LOGICAL(verbose)[0];

  if (!xlength(obj)) return(obj);

  bool binplace = LOGICAL(inplace)[0];
  SEXP x = R_NilValue;
  if (isVectorAtomic(obj)) {
    if (binplace) {
      error("'x' argument is atomic vector, in-place update is supported only for list/data.table");
    } else if (!(isReal(obj) || isInteger(obj))) {
      error("'x' argument must be numeric type, or list/data.table of numeric types");
    }
    x = PROTECT(allocVector(VECSXP, 1)); protecti++; // wrap into list
    SET_VECTOR_ELT(x, 0, obj);
  } else {
    SEXP ricols = PROTECT(colnamesInt(obj, cols)); protecti++; // nafill cols=NULL which turns into seq_along(obj)
    x = PROTECT(allocVector(VECSXP, length(ricols))); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<length(ricols); i++) {
      if (!(isReal(VECTOR_ELT(obj, icols[i]-1)) || isInteger(VECTOR_ELT(obj, icols[i]-1)))) {
        error("'x' argument must be numeric type, or list/data.table of numeric types");
      }
      SET_VECTOR_ELT(x, i, VECTOR_ELT(obj, icols[i]-1));
    }
  }
  R_len_t nx = length(x);

  double* dx[nx];
  int32_t* ix[nx];
  uint_fast64_t inx[nx];
  SEXP ans = R_NilValue;
  ans_t *vans = malloc(sizeof(ans_t)*nx);
  if (!vans) error("%s: Unable to allocate memory answer", __func__); // # nocov
  for (R_len_t i=0; i<nx; i++) {
    inx[i] = xlength(VECTOR_ELT(x, i));
    dx[i] = REAL(VECTOR_ELT(x, i));
    ix[i] = INTEGER(VECTOR_ELT(x, i));
  }
  if (!binplace) {
    ans = PROTECT(allocVector(VECSXP, nx)); protecti++;
    for (R_len_t i=0; i<nx; i++) {
      SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(VECTOR_ELT(x, i)), inx[i]));
      vans[i] = ((ans_t) { .dbl_v=REAL(VECTOR_ELT(ans, i)), .int_v=INTEGER(VECTOR_ELT(ans, i)), .status=0, .message={"\0","\0","\0","\0"} });
    }
  } else {
    for (R_len_t i=0; i<nx; i++) {
      vans[i] = ((ans_t) { .dbl_v=dx[i], .int_v=ix[i], .status=0, .message={"\0","\0","\0","\0"} });
    }
  }

  unsigned int itype;
  if (!strcmp(CHAR(STRING_ELT(type, 0)), "const")) itype = 0;
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "locf")) itype = 1;
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "nocb")) itype = 2;
  else error("Internal error: invalid type argument in nafillR function, should have been caught before. please report to data.table issue tracker."); // # nocov

  if (itype==0 && length(fill)!=1)
    error("fill must be a vector of length 1");

  double dfill=NA_REAL;
  int32_t ifill=NA_INTEGER;
  if (itype==0) {
    if (isInteger(fill)) {
      ifill = INTEGER(fill)[0];
      dfill = INTEGER(fill)[0]==NA_INTEGER ? NA_REAL : (double)INTEGER(fill)[0];
    } else if (isReal(fill)) {
      ifill = ISNA(REAL(fill)[0]) ? NA_INTEGER : (int32_t)REAL(fill)[0];
      dfill = REAL(fill)[0];
    } else if (isLogical(fill) && LOGICAL(fill)[0]==NA_LOGICAL){
      ifill = NA_INTEGER;
      dfill = NA_REAL;
    } else {
      error("fill must be numeric");
    }
  }

  double tic=0.0, toc=0.0;
  if (bverbose) tic = omp_get_wtime();
  #pragma omp parallel for if (nx>1) schedule(auto) num_threads(getDTthreads())
  for (R_len_t i=0; i<nx; i++) {
    switch (TYPEOF(VECTOR_ELT(x, i))) {
    case REALSXP :
      nafillDouble(dx[i], inx[i], itype, dfill, &vans[i], bverbose);
      break;
    case INTSXP :
      nafillInteger(ix[i], inx[i], itype, ifill, &vans[i], bverbose);
      break;
    }
  }
  if (bverbose) toc = omp_get_wtime();

  for (R_len_t i=0; i<nx; i++) {
    if (bverbose && (vans[i].message[0][0] != '\0')) Rprintf("%s: %d: %s", __func__, i+1, vans[i].message[0]);
    if (vans[i].message[1][0] != '\0') REprintf("%s: %d: %s", __func__, i+1, vans[i].message[1]); // # nocov start
    if (vans[i].message[2][0] != '\0') warning("%s: %d: %s", __func__, i+1, vans[i].message[2]);
    if (vans[i].status == 3) error("%s: %d: %s", __func__, i+1, vans[i].message[3]); // # nocov end
  }

  if (bverbose) Rprintf("%s: parallel processing of %d column(s) took %.3fs\n", __func__, nx, toc-tic);

  UNPROTECT(protecti);
  if (binplace) {
    return obj;
  } else {
    return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
  }
}
