#include "data.table.h"

void replace_names(SEXP expr, SEXP env) {
  R_len_t len = length(expr);
  if (!isNull(expr) && len && isLanguage(expr)) { // isLanguage is R's is.call
    SEXP exprnames = getAttrib(expr, R_NamesSymbol);
    if (!isNull(exprnames)) {
      SEXP envnames = getAttrib(env, R_NamesSymbol);
      SEXP matches = PROTECT(chmatch(exprnames, envnames, 0));
      int *imatches = INTEGER(matches);
      const SEXP *sexpr = SEXPPTR_RO(exprnames);
      const SEXP *senv = SEXPPTR_RO(env);
      SEXP tmp = expr;
      for (int i=0; i<len; i++) {
        if (imatches[i]) {
          Rprintf("substitute: %s -> %s\n", CHAR(sexpr[i]), CHAR(PRINTNAME(senv[imatches[i]-1])));
          SET_TAG(tmp, senv[imatches[i]-1]);
        }
        tmp = CDR(tmp);
      }
      UNPROTECT(1); // matches
      // update also nested calls
      for (SEXP t=expr; t!=R_NilValue; t=CDR(t))
        replace_names(CADR(t), env);
    }
  }
}
SEXP replace_namesR(SEXP expr, SEXP env) {
  // move R's checks here, escape for 0 length, etc
  SEXP ans = PROTECT(MAYBE_REFERENCED(expr) ? duplicate(expr) : expr);
  replace_names(ans, env); // updates in-place
  UNPROTECT(1);
  return ans;
}
