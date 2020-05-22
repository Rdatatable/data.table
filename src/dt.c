#include "data.table.h"

int NROW(SEXP x) {
  if (!LENGTH(x))
    return 0; // # nocov # not yet reached from anywhere, cbindlist uses it but escapes for !NCOL(x)
  return length(VECTOR_ELT(x, 0));
}

// c("data.table","data.frame")
static SEXP char2_dtdf() {
  SEXP char2_dtdf = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(char2_dtdf, 0, char_datatable);
  SET_STRING_ELT(char2_dtdf, 1, char_dataframe);
  UNPROTECT(1);
  return char2_dtdf;
}
// .set_row_names(x)
static SEXP set_row_names(int n) {
  SEXP ans = R_NilValue;
  if (n) {
    ans = PROTECT(allocVector(INTSXP, 2));
    INTEGER(ans)[0] = NA_INTEGER;
    INTEGER(ans)[1] = -n;
  } else {
    ans = PROTECT(allocVector(INTSXP, 0));
  }
  UNPROTECT(1);
  return ans;
}

// like setDT(x) but not in-place
SEXP makeDT(SEXP x) {
  if (!isNewList(x))
    error("Argument 'x' must be a 'list', 'data.frame' or 'data.table'");
  R_len_t n = length(x)+INTEGER(GetOption(sym_alloccol, R_NilValue))[0];
  Rboolean verbose = (Rboolean)LOGICAL(GetOption(sym_verbose, R_NilValue))[0];
  SEXP ans = PROTECT(alloccol(x, n, verbose));
  setAttrib(ans, R_ClassSymbol, char2_dtdf());
  setAttrib(ans, sym_rownames, set_row_names(NROW(x)));
  UNPROTECT(1);
  return ans;
}
