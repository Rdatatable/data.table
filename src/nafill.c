#include "data.table.h"

void nafillDouble(double *x, uint_fast64_t nx, unsigned int type, double fill, ans_t *ans, bool verbose) {
  double tic=0.0;
  if (verbose)
    tic = omp_get_wtime();
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
  if (verbose)
    snprintf(ans->message[0], 500, "%s: took %.3fs\n", __func__, omp_get_wtime()-tic);
}
void nafillInteger(int32_t *x, uint_fast64_t nx, unsigned int type, int32_t fill, ans_t *ans, bool verbose) {
  double tic=0.0;
  if (verbose)
    tic = omp_get_wtime();
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
  if (verbose)
    snprintf(ans->message[0], 500, "%s: took %.3fs\n", __func__, omp_get_wtime()-tic);
}
void nafillInteger64(int64_t *x, uint_fast64_t nx, unsigned int type, int64_t fill, ans_t *ans, bool verbose) {
  double tic=0.0;
  if (verbose)
    tic = omp_get_wtime();
  if (type==0) { // const
    for (uint_fast64_t i=0; i<nx; i++) {
      ans->int64_v[i] = x[i]==NA_INTEGER64 ? fill : x[i];
    }
  } else if (type==1) { // locf
    ans->int64_v[0] = x[0];
    for (uint_fast64_t i=1; i<nx; i++) {
      ans->int64_v[i] = x[i]==NA_INTEGER64 ? ans->int64_v[i-1] : x[i];
    }
  } else if (type==2) { // nocb
    ans->int64_v[nx-1] = x[nx-1];
    for (int_fast64_t i=nx-2; i>=0; i--) {
      ans->int64_v[i] = x[i]==NA_INTEGER64 ? ans->int64_v[i+1] : x[i];
    }
  }
  if (verbose)
    snprintf(ans->message[0], 500, "%s: took %.3fs\n", __func__, omp_get_wtime()-tic);
}

SEXP nafillR(SEXP obj, SEXP type, SEXP fill, SEXP inplace, SEXP cols, SEXP verbose) {
  int protecti=0;
  if (!IS_TRUE_OR_FALSE(verbose))
    error("verbose must be TRUE or FALSE");
  bool bverbose = LOGICAL(verbose)[0];

  if (!xlength(obj))
    return(obj);

  bool binplace = LOGICAL(inplace)[0];
  SEXP x = R_NilValue;
  if (isVectorAtomic(obj)) {
    if (binplace)
      error("'x' argument is atomic vector, in-place update is supported only for list/data.table");
    else if (!isReal(obj) && !isInteger(obj))
      error("'x' argument must be numeric type, or list/data.table of numeric types");
    x = PROTECT(allocVector(VECSXP, 1)); protecti++; // wrap into list
    SET_VECTOR_ELT(x, 0, obj);
  } else {
    SEXP ricols = PROTECT(colnamesInt(obj, cols, ScalarLogical(TRUE))); protecti++; // nafill cols=NULL which turns into seq_along(obj)
    x = PROTECT(allocVector(VECSXP, length(ricols))); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<length(ricols); i++) {
      SEXP this_col = VECTOR_ELT(obj, icols[i]-1);
      if (!isReal(this_col) && !isInteger(this_col))
        error("'x' argument must be numeric type, or list/data.table of numeric types");
      SET_VECTOR_ELT(x, i, this_col);
    }
  }
  R_len_t nx = length(x);

  double* dx[nx];
  int32_t* ix[nx];
  int64_t* i64x[nx];
  uint_fast64_t inx[nx];
  SEXP ans = R_NilValue;
  ans_t *vans = (ans_t *)R_alloc(nx, sizeof(ans_t));
  for (R_len_t i=0; i<nx; i++) {
    inx[i] = xlength(VECTOR_ELT(x, i));
    dx[i] = REAL(VECTOR_ELT(x, i));
    ix[i] = INTEGER(VECTOR_ELT(x, i));
    i64x[i] = (int64_t *)REAL(VECTOR_ELT(x, i));
  }
  if (!binplace) {
    ans = PROTECT(allocVector(VECSXP, nx)); protecti++;
    for (R_len_t i=0; i<nx; i++) {
      SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(VECTOR_ELT(x, i)), inx[i]));
      vans[i] = ((ans_t) { .dbl_v=REAL(VECTOR_ELT(ans, i)), .int_v=INTEGER(VECTOR_ELT(ans, i)), .int64_v=(int64_t *)REAL(VECTOR_ELT(ans, i)), .status=0, .message={"\0","\0","\0","\0"} });
    }
  } else {
    for (R_len_t i=0; i<nx; i++) {
      vans[i] = ((ans_t) { .dbl_v=dx[i], .int_v=ix[i], .int64_v=i64x[i], .status=0, .message={"\0","\0","\0","\0"} });
    }
  }

  unsigned int itype;
  if (!strcmp(CHAR(STRING_ELT(type, 0)), "const"))
    itype = 0;
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "locf"))
    itype = 1;
  else if (!strcmp(CHAR(STRING_ELT(type, 0)), "nocb"))
    itype = 2;
  else
    error("Internal error: invalid type argument in nafillR function, should have been caught before. Please report to data.table issue tracker."); // # nocov

  if (itype==0 && length(fill)!=1)
    error("fill must be a vector of length 1");

  double dfill=NA_REAL;
  int32_t ifill=NA_INTEGER;
  int64_t i64fill=NA_INTEGER64;
  if (itype==0)
    coerceFill(fill, &dfill, &ifill, &i64fill);

  double tic=0.0, toc=0.0;
  if (bverbose)
    tic = omp_get_wtime();
  #pragma omp parallel for if (nx>1) num_threads(getDTthreads())
  for (R_len_t i=0; i<nx; i++) {
    SEXP this_x = VECTOR_ELT(x, i);
    switch (TYPEOF(this_x)) {
    case REALSXP : {
      if (INHERITS(this_x, char_integer64) || INHERITS(this_x, char_nanotime)) {  // inside parallel region so can't call Rinherits()
        nafillInteger64(i64x[i], inx[i], itype, i64fill, &vans[i], bverbose);
      } else {
        nafillDouble(dx[i], inx[i], itype, dfill, &vans[i], bverbose);
      }
    } break;
    case INTSXP : {
      nafillInteger(ix[i], inx[i], itype, ifill, &vans[i], bverbose);
    } break;
    default: error("Internal error: invalid type argument in nafillR function, should have been caught before. Please report to data.table issue tracker."); // # nocov
    }
  }
  if (bverbose)
    toc = omp_get_wtime();

  if (!binplace) {
    for (R_len_t i=0; i<nx; i++) {
      if (!isNull(ATTRIB(VECTOR_ELT(x, i))))
        copyMostAttrib(VECTOR_ELT(x, i), VECTOR_ELT(ans, i));
    }
  }

  ansMsg(vans, nx, bverbose, __func__);

  if (bverbose)
    Rprintf("%s: parallel processing of %d column(s) took %.3fs\n", __func__, nx, toc-tic);

  UNPROTECT(protecti);
  if (binplace) {
    return obj;
  } else {
    return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
  }
}
