#include "data.table.h"
#include <Rdefines.h>

void nafillDouble(double *x, uint_fast64_t nx, unsigned int type, double fill, ans_t *ans) {
  if (type==0) { // const
    for (uint_fast64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = ISNA(x[i]) ? fill : x[i];
    }
  } else if (type==1) { // locf
    double last_x=NA_REAL;
    for (uint_fast64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = ISNA(x[i]) ? last_x : x[i];
      last_x = ans->dbl_v[i];
    }
  } else if (type==2) { // nocb
    double next_x=NA_REAL;
    for (int_fast64_t i=nx-1; i>=0; i--) {
      ans->dbl_v[i] = ISNA(x[i]) ? next_x : x[i];
      next_x = ans->dbl_v[i];
    }
  }
}
void nafillInteger(int32_t *x, uint_fast64_t nx, unsigned int type, int32_t fill, ans_t *ans) {
  if (type==0) { // const
    for (uint_fast64_t i=0; i<nx; i++) {
      ans->int_v[i] = x[i]==NA_INTEGER ? fill : x[i];
    }
  } else if (type==1) { // locf
    int32_t last_x=NA_INTEGER;
    for (uint_fast64_t i=0; i<nx; i++) {
      ans->int_v[i] = x[i]==NA_INTEGER ? last_x : x[i];
      last_x = ans->int_v[i];
    }
  } else if (type==2) { // nocb
    int32_t next_x=NA_INTEGER;
    for (int_fast64_t i=nx-1; i>=0; i--) {
      ans->int_v[i] = x[i]==NA_INTEGER ? next_x : x[i];
      next_x = ans->int_v[i];
    }
  }
}

SEXP nafillR(SEXP obj, SEXP type, SEXP fill) {
  int protecti=0;
  
  if (!xlength(obj)) return(obj);
  SEXP x;
  if (isVectorAtomic(obj)) {
    x = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(x, 0, obj);
  } else {
    x = obj;
  }
  R_len_t nx=length(x);
  
  SEXP ans;
  ans = PROTECT(allocVector(VECSXP, nx)); protecti++;
  ans_t vans[nx];
  double* dx[nx];
  int32_t* ix[nx];
  uint_fast64_t inx[nx];
  for (R_len_t i=0; i<nx; i++) {
    inx[i] = xlength(VECTOR_ELT(x, i));
    SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(VECTOR_ELT(x, i)), inx[i]));
    vans[i] = ((ans_t) { .dbl_v=REAL(VECTOR_ELT(ans, i)), .int_v=INTEGER(VECTOR_ELT(ans, i)), .status=0, .message={"\0","\0","\0","\0"} });
    dx[i] = REAL(VECTOR_ELT(x, i));
    ix[i] = INTEGER(VECTOR_ELT(x, i));
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
      dfill = INTEGER(fill)[0]==NA_LOGICAL ? NA_REAL : (double)INTEGER(fill)[0];
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
  
  #pragma omp parallel for num_threads(getDTthreads())
  for (R_len_t i=0; i<nx; i++) {
    switch (TYPEOF(VECTOR_ELT(x, i))) {
    case REALSXP :
      nafillDouble(dx[i], inx[i], itype, dfill, &vans[i]);
      break;
    case INTSXP :
      nafillInteger(ix[i], inx[i], itype, ifill, &vans[i]);
      break;
    }
  }
  
  for (R_len_t i=0; i<nx; i++) { // # nocov start
    if (vans[i].status == 3) {
      error(vans[i].message[3]);
    } else if (vans[i].status == 2) {
      warning(vans[i].message[2]);
    } else if (vans[i].status == 1) {
      //message(vans[i].message[1]);
    } else if (vans[i].status == 1) {
      //Rprintf(vans[i].message[0]);
    }
  } // # nocov end
  
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
