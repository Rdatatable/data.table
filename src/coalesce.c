#include "data.table.h"

SEXP coalesce(SEXP x, SEXP values) {
  int JJ = LENGTH(values), protecti = 0;
  uint64_t NN = LENGTH(x), na_count = 0;;

  switch(TYPEOF(x)) { // pass over x in parallel to count NA
  case LGLSXP: {
    int *restrict x_ptr = LOGICAL(x);
    #pragma omp parallel for schedule(dynamic) reduction(+:na_count)
    for (uint64_t i = 0; i < NN; i++) na_count += (int) (x_ptr[i] == NA_LOGICAL);
  } break;
  case INTSXP: {
    int *restrict x_ptr = INTEGER(x);
    #pragma omp parallel for schedule(dynamic) reduction(+:na_count)
    for (uint64_t i = 0; i < NN; i++) na_count += (int) (x_ptr[i] == NA_INTEGER);
  } break;
  case REALSXP: {
    double *restrict x_ptr = REAL(x);
    #pragma omp parallel for schedule(dynamic) reduction(+:na_count)
    for (uint64_t i = 0; i < NN; i++) na_count += (int) (ISNA(x_ptr[i]));
  } break;
  case STRSXP: {
    for (uint64_t i = 0; i < NN; i++) na_count += (int) (STRING_ELT(x, i) == NA_STRING);
  } break;
  default: error("Incompatible type");
  }

  switch(TYPEOF(x)) {
  case LGLSXP: {
    int *x_ptr = LOGICAL(x);
    SEXP out = PROTECT(allocVector(LGLSXP, NN)); protecti++;
    int *out_ptr = LOGICAL(out);
    for (uint64_t i = 0; i < NN; i++) {
      if (na_count > 0 && x_ptr[i] == NA_LOGICAL) {
        int j = 0;
        while (j < JJ) {
          SEXP this_j = VECTOR_ELT(values, j);
          int this_replacement = LOGICAL(this_j)[LENGTH(this_j)==1 ? 0 : i];
          if (this_replacement != NA_LOGICAL) {
            out_ptr[i] = this_replacement;
            na_count--;
            break;
          }
          j++;
        }
        if (j == JJ) out_ptr[i] = NA_LOGICAL;
      } else out_ptr[i] = x_ptr[i];
    }
    UNPROTECT(protecti);
    return(out);
  }
  case INTSXP: {
    int *x_ptr = INTEGER(x);
    SEXP out = PROTECT(allocVector(INTSXP, NN)); protecti++;
    int *out_ptr = INTEGER(out);
    for (uint64_t i = 0; i < NN; i++) {
      if (na_count > 0 && x_ptr[i] == NA_INTEGER) {
        int j = 0;
        while (j < JJ) {
          SEXP this_j = VECTOR_ELT(values, j);
          int this_replacement = INTEGER(this_j)[LENGTH(this_j)==1 ? 0 : i];
          if (this_replacement != NA_INTEGER) {
            out_ptr[i] = this_replacement;
            na_count--;
            break;
          }
          j++;
        }
        if (j == JJ) out_ptr[i] = NA_INTEGER;
      } else out_ptr[i] = x_ptr[i];
    }
    UNPROTECT(protecti);
    return(out);
  }
  case REALSXP: {
    double *x_ptr = REAL(x);
    SEXP out = PROTECT(allocVector(REALSXP, NN)); protecti++;
    double *out_ptr = REAL(out);
    for (uint64_t i = 0; i < NN; i++) {
      if (na_count > 0 && ISNA(x_ptr[i])) {
        int j = 0;
        while (j < JJ) {
          SEXP this_j = VECTOR_ELT(values, j);
          double this_replacement = REAL(this_j)[LENGTH(this_j)==1 ? 0 : i];
          if (!ISNA(this_replacement)) {
            out_ptr[i] = this_replacement;
            na_count--;
            break;
          }
          j++;
        }
        if (j == JJ) out_ptr[i] = NA_REAL;
      } else out_ptr[i] = x_ptr[i];
    }
    UNPROTECT(protecti);
    return(out);
  }
  case STRSXP: {
    SEXP out = PROTECT(allocVector(STRSXP, NN)); protecti++;
    for (uint64_t i = 0; i < NN; i++) {
      if (na_count > 0 && STRING_ELT(x, i) == NA_STRING) {
        int j = 0;
        while (j < JJ) {
          SEXP this_j = VECTOR_ELT(values, j);
          SEXP this_replacement = STRING_ELT(this_j, LENGTH(this_j)==1 ? 0 : i);
          if (this_replacement != NA_STRING) {
            SET_STRING_ELT(out, i, this_replacement);
            na_count--;
            break;
          }
          j++;
        }
        if (j == JJ) SET_STRING_ELT(out, i, NA_STRING);
      } else SET_STRING_ELT(out, i, STRING_ELT(x, i));
    }
    UNPROTECT(protecti);
    return(out);
  }
  default:
    error("Unrecognized type.");
  }
}
