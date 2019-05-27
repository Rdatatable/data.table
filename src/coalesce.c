#include "data.table.h"

SEXP coalesce(SEXP x, SEXP values, SEXP n, SEXP count_na) {
  int JJ = LENGTH(values), nprotect = 0, NN = asInteger(n), KK = asInteger(count_na);
  
  switch(TYPEOF(x)) {
  case LGLSXP: {
    int *base_ptr = LOGICAL(x);
    
    SEXP out = PROTECT(allocVector(LGLSXP, NN)); nprotect++;
    int *out_ptr = LOGICAL(out);
    
    for (int i = 0; i < NN; i++) {
      Rprintf("KK: %d\ti: %d\n", KK, i);
      if (KK > 0 && base_ptr[i] == NA_LOGICAL) {
        int j = 0;
        while (j < JJ) {
          Rprintf("LOOPING values");
          SEXP this_j = VECTOR_ELT(values, j);
          int take_idx;
          if (LENGTH(this_j) == 1) take_idx = 0; else take_idx = i;
          int this_replacement = LOGICAL(this_j)[take_idx];
          if (this_replacement != NA_LOGICAL) {
            out_ptr[i] = this_replacement;
            KK--;
            break;
          }
          j++;
        }
        if (j == JJ) out_ptr[i] = NA_LOGICAL;
      } else out_ptr[i] = base_ptr[i];
    }
    UNPROTECT(nprotect);
    return(out);
  }
  case INTSXP: {
    int *base_ptr = INTEGER(x);
    
    SEXP out = PROTECT(allocVector(INTSXP, NN)); nprotect++;
    int *out_ptr = INTEGER(out);
    
    int i = 0;
    
    Rprintf("starting loop\n");
    while (KK > 0 && i < NN) {
      Rprintf("Current NA count: %d\tCurrent row: %d\n", KK, i);
      if (base_ptr[i] == NA_LOGICAL) {
        Rprintf("Found NA!\n");
        int j = 0;
        while (j < JJ) {
          int this_replacement = LOGICAL(VECTOR_ELT(values, j))[i];
          if (this_replacement != NA_LOGICAL) {
            Rprintf("Found replacement!\n");
            out_ptr[i] = this_replacement;
            KK--;
            break;
          }
          j++;
        }
        if (j == JJ) {
          Rprintf("Giving up & remaining NA\n");
          out_ptr[i] = NA_LOGICAL;
        }
      } else {
        Rprintf("Not missing\n");
        out_ptr[i] = base_ptr[i];
      }
      i++;
    }
    UNPROTECT(nprotect);
    return(out);
  }
  default:
    error("Unrecognized type.");
  }
}
