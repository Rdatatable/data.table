#include "data.table.h"

SEXP cj(SEXP base_list) {
  int JJ = LENGTH(base_list), nprotect = 0;
  SEXP out = PROTECT(allocVector(VECSXP, JJ)); nprotect++;
  int NN = 1;
  for (int j = 0; j < JJ; j++) NN *= LENGTH(VECTOR_ELT(base_list, j));
  int div = NN, modulo;
  
  for (int j = 0; j < JJ; j++) {
    SEXP this_v = VECTOR_ELT(base_list, j);
    modulo = div;
    div = modulo/LENGTH(VECTOR_ELT(base_list, j));
    switch(TYPEOF(this_v)) {
    case LGLSXP: {
      SEXP this_col = PROTECT(allocVector(LGLSXP, NN)); nprotect++;
      int *this_v_bool = LOGICAL(this_v);
      for (int i = 0; i < NN; i++) {
        LOGICAL(this_col)[i] = this_v_bool[(i % modulo) / div];
      }
      SET_VECTOR_ELT(out, j, this_col);
    }
      break;
    case INTSXP: {
      SEXP this_col = PROTECT(allocVector(INTSXP, NN)); nprotect++;
      int *this_v_int = INTEGER(this_v);
      for (int i = 0; i < NN; i++) {
        INTEGER(this_col)[i] = this_v_int[(i % modulo) / div];
      }
      SET_VECTOR_ELT(out, j, this_col);
    }
      break;
    case REALSXP: {
      SEXP this_col = PROTECT(allocVector(REALSXP, NN)); nprotect++;
      double *this_v_dbl = REAL(this_v);
      for (int i = 0; i < NN; i++) {
        REAL(this_col)[i] = this_v_dbl[(i % modulo) / div];
      }
      SET_VECTOR_ELT(out, j, this_col);
    }
      break;
    case STRSXP: {
      SEXP this_col = PROTECT(allocVector(STRSXP, NN)); nprotect++;
      SEXP *this_v_str = STRING_PTR(this_v);
      for (int i = 0; i < NN; i++) {
        SET_STRING_ELT(this_col, i, STRING_ELT(this_v, (i % modulo) / div));
      }
      SET_VECTOR_ELT(out, j, this_col);
    } break;
    case VECSXP: {
      SEXP this_col = PROTECT(allocVector(VECSXP, NN)); nprotect++;
      for (int i = 0; i < NN; i++) {
        SET_VECTOR_ELT(this_col, i, VECTOR_ELT(this_v, (i % modulo) / div));
      }
      SET_VECTOR_ELT(out, j, this_col);
    } break;
    default:
      error("Type '%s' not supported by CJ.", type2char(TYPEOF(this_v)));
    }
  }
  
  UNPROTECT(nprotect);
  return(out);
}
