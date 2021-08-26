#include "data.table.h"

void nafillDouble(double *x, uint_fast64_t nx, unsigned int type, double fill, bool nan_is_na, ans_t *ans, bool verbose) {
  double tic=0.0;
  if (verbose)
    tic = omp_get_wtime();
  if (type==0) { // const
    if (nan_is_na) {
      for (uint_fast64_t i=0; i<nx; i++) {
        ans->dbl_v[i] = ISNAN(x[i]) ? fill : x[i];
      }
    } else {
      for (uint_fast64_t i=0; i<nx; i++) {
        ans->dbl_v[i] = ISNA(x[i]) ? fill : x[i];
      }
    }
  } else if (type==1) { // locf
    if (nan_is_na) {
      ans->dbl_v[0] = ISNAN(x[0]) ? fill : x[0];
      for (uint_fast64_t i=1; i<nx; i++) {
        ans->dbl_v[i] = ISNAN(x[i]) ? ans->dbl_v[i-1] : x[i];
      }
    } else {
      ans->dbl_v[0] = ISNA(x[0]) ? fill : x[0];
      for (uint_fast64_t i=1; i<nx; i++) {
        ans->dbl_v[i] = ISNA(x[i]) ? ans->dbl_v[i-1] : x[i];
      }
    }
  } else if (type==2) { // nocb
    if (nan_is_na) {
      ans->dbl_v[nx-1] = ISNAN(x[nx-1]) ? fill : x[nx-1];
      for (int_fast64_t i=nx-2; i>=0; i--) {
        ans->dbl_v[i] = ISNAN(x[i]) ? ans->dbl_v[i+1] : x[i];
      }
    } else {
      ans->dbl_v[nx-1] = ISNA(x[nx-1]) ? fill : x[nx-1];
      for (int_fast64_t i=nx-2; i>=0; i--) {
        ans->dbl_v[i] = ISNA(x[i]) ? ans->dbl_v[i+1] : x[i];
      }
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
    ans->int_v[0] = x[0]==NA_INTEGER ? fill : x[0];
    for (uint_fast64_t i=1; i<nx; i++) {
      ans->int_v[i] = x[i]==NA_INTEGER ? ans->int_v[i-1] : x[i];
    }
  } else if (type==2) { // nocb
    ans->int_v[nx-1] = x[nx-1]==NA_INTEGER ? fill : x[nx-1];
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
    ans->int64_v[0] = x[0]==NA_INTEGER64 ? fill : x[0];
    for (uint_fast64_t i=1; i<nx; i++) {
      ans->int64_v[i] = x[i]==NA_INTEGER64 ? ans->int64_v[i-1] : x[i];
    }
  } else if (type==2) { // nocb
    ans->int64_v[nx-1] = x[nx-1]==NA_INTEGER64 ? fill : x[nx-1];
    for (int_fast64_t i=nx-2; i>=0; i--) {
      ans->int64_v[i] = x[i]==NA_INTEGER64 ? ans->int64_v[i+1] : x[i];
    }
  }
  if (verbose)
    snprintf(ans->message[0], 500, _("%s: took %.3fs\n"), __func__, omp_get_wtime()-tic);
}

SEXP nafillR(SEXP obj, SEXP type, SEXP fill, SEXP nan_is_na_arg, SEXP inplace, SEXP cols) {
  int protecti=0;
  const bool verbose = GetVerbose();

  if (!xlength(obj))
    return(obj);

  double tic=0.0;
  if (verbose)
    tic = omp_get_wtime();

  bool binplace = LOGICAL(inplace)[0];
  if (!IS_TRUE_OR_FALSE(nan_is_na_arg))
    error(_("nan_is_na must be TRUE or FALSE")); // # nocov
  bool nan_is_na = LOGICAL(nan_is_na_arg)[0];

  SEXP x = R_NilValue;
  bool obj_scalar = isVectorAtomic(obj);
  if (obj_scalar) {
    if (binplace)
      error(_("'x' argument is atomic vector, in-place update is supported only for list/data.table"));
    else if (!isReal(obj) && !isInteger(obj))
      error(_("'x' argument must be numeric type, or list/data.table of numeric types"));
    SEXP obj1 = obj;
    obj = PROTECT(allocVector(VECSXP, 1)); protecti++; // wrap into list
    SET_VECTOR_ELT(obj, 0, obj1);
  }
  SEXP ricols = PROTECT(colnamesInt(obj, cols, ScalarLogical(TRUE))); protecti++; // nafill cols=NULL which turns into seq_along(obj)
  x = PROTECT(allocVector(VECSXP, length(ricols))); protecti++;
  int *icols = INTEGER(ricols);
  for (int i=0; i<length(ricols); i++) {
    SEXP this_col = VECTOR_ELT(obj, icols[i]-1);
    if (!isReal(this_col) && !isInteger(this_col))
      error(_("'x' argument must be numeric type, or list/data.table of numeric types"));
    SET_VECTOR_ELT(x, i, this_col);
  }
  R_len_t nx = length(x);

  double **dx = (double**)R_alloc(nx, sizeof(double*));
  int32_t **ix = (int32_t**)R_alloc(nx, sizeof(int32_t*));
  int64_t **i64x = (int64_t**)R_alloc(nx, sizeof(int64_t*));
  uint_fast64_t *inx = (uint_fast64_t*)R_alloc(nx, sizeof(uint_fast64_t));
  SEXP ans = R_NilValue;
  ans_t *vans = (ans_t *)R_alloc(nx, sizeof(ans_t));
  for (R_len_t i=0; i<nx; i++) {
    const SEXP xi = VECTOR_ELT(x, i);
    inx[i] = xlength(xi);
    // not sure why these pointers are being constructed like this; TODO: simplify structure // A: because they are used in `ans_t` struct, strictly speaking, one of them, the expected one, is used in that struct.
    if (isReal(xi)) {
      dx[i] = REAL(xi);
      i64x[i] = (int64_t *)REAL(xi);
      ix[i] = NULL;
    } else {
      ix[i] = INTEGER(xi);
      dx[i] = NULL;
      i64x[i] = NULL;
    }
  }
  if (!binplace) {
    ans = PROTECT(allocVector(VECSXP, nx)); protecti++;
    for (R_len_t i=0; i<nx; i++) {
      SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(VECTOR_ELT(x, i)), inx[i]));
      const SEXP ansi = VECTOR_ELT(ans, i);
      const void *p = isReal(ansi) ? (void *)REAL(ansi) : (void *)INTEGER(ansi);
      vans[i] = ((ans_t) { .dbl_v=(double *)p, .int_v=(int *)p, .int64_v=(int64_t *)p, .status=0, .message={"\0","\0","\0","\0"} });
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
    error(_("Internal error: invalid %s argument in %s function should have been caught earlier. Please report to the data.table issue tracker."), "type", "nafillR"); // # nocov

  bool hasFill = !isLogical(fill) || LOGICAL(fill)[0]!=NA_LOGICAL;
  bool *isInt64 = (bool *)R_alloc(nx, sizeof(bool));
  for (R_len_t i=0; i<nx; i++)
    isInt64[i] = INHERITS(VECTOR_ELT(x, i), char_integer64);
  const void **fillp = (const void **)R_alloc(nx, sizeof(void*)); // fill is (or will be) a list of length nx of matching types, scalar values for each column, this pointer points to each of those columns data pointers
  if (hasFill) {
    if (nx!=length(fill) && length(fill)!=1)
      error(_("fill must be a vector of length 1 or a list of length of x"));
    if (!isNewList(fill)) {
      SEXP fill1 = fill;
      fill = PROTECT(allocVector(VECSXP, nx)); protecti++;
      for (int i=0; i<nx; ++i)
        SET_VECTOR_ELT(fill, i, fill1);
    }
    if (!isNewList(fill))
      error(_("internal error: 'fill' should be recycled as list already")); // # nocov
    for (R_len_t i=0; i<nx; i++) {
      SET_VECTOR_ELT(fill, i, coerceAs(VECTOR_ELT(fill, i), VECTOR_ELT(x, i), ScalarLogical(TRUE)));
      fillp[i] = SEXPPTR_RO(VECTOR_ELT(fill, i)); // do like this so we can use in parallel region
    }
  }
  #pragma omp parallel for if (nx>1) num_threads(getDTthreads(nx, true))
  for (R_len_t i=0; i<nx; i++) {
    switch (TYPEOF(VECTOR_ELT(x, i))) {
    case REALSXP : {
      if (isInt64[i]) {
        nafillInteger64(i64x[i], inx[i], itype, hasFill ? ((int64_t *)fillp[i])[0] : NA_INTEGER64, &vans[i], verbose);
      } else {
        nafillDouble(dx[i], inx[i], itype, hasFill ? ((double *)fillp[i])[0] : NA_REAL, nan_is_na, &vans[i], verbose);
      }
    } break;
    case INTSXP : {
      nafillInteger(ix[i], inx[i], itype, hasFill ? ((int32_t *)fillp[i])[0] : NA_INTEGER, &vans[i], verbose);
    } break;
    }
  }

  if (!binplace) {
    for (R_len_t i=0; i<nx; i++) {
      if (!isNull(ATTRIB(VECTOR_ELT(x, i))))
        copyMostAttrib(VECTOR_ELT(x, i), VECTOR_ELT(ans, i));
    }
    SEXP obj_names = getAttrib(obj, R_NamesSymbol); // copy names
    if (!isNull(obj_names)) {
      SEXP ans_names = PROTECT(allocVector(STRSXP, length(ans))); protecti++;
      for (int i=0; i<length(ricols); i++)
        SET_STRING_ELT(ans_names, i, STRING_ELT(obj_names, icols[i]-1));
      setAttrib(ans, R_NamesSymbol, ans_names);
    }
  }

  ansMsg(vans, nx, verbose, __func__);

  if (verbose)
    Rprintf(_("%s: parallel processing of %d column(s) took %.3fs\n"), __func__, nx, omp_get_wtime()-tic);

  UNPROTECT(protecti);
  if (binplace) {
    return obj;
  } else {
    return obj_scalar && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
  }
}
