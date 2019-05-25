#include "data.table.h"

SEXP cj(SEXP base_list) {
  int JJ = LENGTH(base_list), nprotect = 0;
  SEXP out = PROTECT(allocVector(VECSXP, JJ)); nprotect++;
  int NN = 1;
  for (int j = 0; j < JJ; j++) NN *= LENGTH(VECTOR_ELT(base_list, j));
  int div = NN, modulo;
  
  //#pragma omp parallel for num_threads(getDTthreads())
  for (int j = 0; j < JJ; j++) {
    SEXP this_v = VECTOR_ELT(base_list, j);
    modulo = div;
    div = modulo/LENGTH(VECTOR_ELT(base_list, j));
    switch(TYPEOF(this_v)) {
    case LGLSXP: {
      SEXP this_col = PROTECT(allocVector(LGLSXP, NN)); nprotect++;
      int *restrict this_v_ptr = LOGICAL(this_v);
      int *this_col_ptr = LOGICAL(this_col);
      if (j == 0) {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i = 0; i < NN; i++) {
          this_col_ptr[i] = this_v_ptr[i / div];
        }
      } else if (j == JJ - 1) {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i = 0; i < NN; i++) {
          this_col_ptr[i] = this_v_ptr[i % modulo];
        }
      } else {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i = 0; i < NN; i++) {
          this_col_ptr[i] = this_v_ptr[(i % modulo) / div];
        }
      }
      SET_VECTOR_ELT(out, j, this_col);
    }
      break;
    case INTSXP: {
      SEXP this_col = PROTECT(allocVector(INTSXP, NN)); nprotect++;
      int *restrict this_v_ptr = INTEGER(this_v);
      int *this_col_ptr = INTEGER(this_col);
      if (j == 0) {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i = 0; i < NN; i++) {
          this_col_ptr[i] = this_v_ptr[i / div];
        }
      } else if (j == JJ - 1) {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i = 0; i < NN; i++) {
          this_col_ptr[i] = this_v_ptr[i % modulo];
        }
      } else {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i = 0; i < NN; i++) {
          this_col_ptr[i] = this_v_ptr[(i % modulo) / div];
        }
      }
      SET_VECTOR_ELT(out, j, this_col);
    }
      break;
    case REALSXP: {
      SEXP this_col = PROTECT(allocVector(REALSXP, NN)); nprotect++;
      double *restrict this_v_ptr = REAL(this_v);
      double *this_col_ptr = REAL(this_col);
      if (j == 0) {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i = 0; i < NN; i++) {
          this_col_ptr[i] = this_v_ptr[i / div];
        }
      } else if (j == JJ - 1) {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i = 0; i < NN; i++) {
          this_col_ptr[i] = this_v_ptr[i % modulo];
        }
      } else {
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i = 0; i < NN; i++) {
          this_col_ptr[i] = this_v_ptr[(i % modulo) / div];
        }
      }
      SET_VECTOR_ELT(out, j, this_col);
    } 
      break;
    case STRSXP: {
      SEXP this_col = PROTECT(allocVector(STRSXP, NN)); nprotect++;
      if (j == 0) {
        for (int i = 0; i < NN; i++) {
          SET_STRING_ELT(this_col, i, STRING_ELT(this_v, i / div));
        }
      } else if (j == JJ - 1) {
        for (int i = 0; i < NN; i++) {
          SET_STRING_ELT(this_col, i, STRING_ELT(this_v, i % modulo));
        }
      } else {
        for (int i = 0; i < NN; i++) {
          SET_STRING_ELT(this_col, i, STRING_ELT(this_v, (i % modulo) / div));
        }
      }
      SET_VECTOR_ELT(out, j, this_col);
    } break;
    case VECSXP: {
      SEXP this_col = PROTECT(allocVector(VECSXP, NN)); nprotect++;
      if (j == 0) {
        for (int i = 0; i < NN; i++) {
          SET_VECTOR_ELT(this_col, i, VECTOR_ELT(this_v, i / div));
        }
      } else if (j == JJ - 1) {
        for (int i = 0; i < NN; i++) {
          SET_VECTOR_ELT(this_col, i, VECTOR_ELT(this_v, i % modulo));
        }
      } else {
        for (int i = 0; i < NN; i++) {
          SET_VECTOR_ELT(this_col, i, VECTOR_ELT(this_v, (i % modulo) / div));
        }
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
