#include <Rdefines.h>
#include "data.table.h"

/*
 * find end of a string, used to append verbose messages or warnings
 */
char *end(char *start) {
  return strchr(start, 0);
}

/*
 * function to print verbose messages, stderr messages, warnings and errors stored in ans_t struct
 */
void ansMsg(ans_t *ans, int n, bool verbose, const char *func) {
  for (int i=0; i<n; i++) {
    if (verbose && (ans[i].message[0][0] != '\0'))
      Rprintf("%s: %d:\n%s", func, i+1, ans[i].message[0]);
    if (ans[i].message[1][0] != '\0')
      REprintf("%s: %d:\n%s", func, i+1, ans[i].message[1]);
    if (ans[i].message[2][0] != '\0')
      warning("%s: %d:\n%s", func, i+1, ans[i].message[2]);
    if (ans[i].status == 3)
      error("%s: %d:\n%s:", func, i+1, ans[i].message[3]);
  }
}

/*
 * R interface to test ansMsg function
 * see inst/tests/types.Rraw
 */
void testRaiseMsg(ans_t *ans, int istatus, bool verbose) {
  if (verbose) {
    snprintf(end(ans->message[0]), 500, "%s: stdout 1 message\n", __func__);
    snprintf(end(ans->message[0]), 500, "%s: stdout 2 message\n", __func__);
  }
  if (istatus == 1 || istatus == 12 || istatus == 13 || istatus == 123) {
    snprintf(end(ans->message[1]), 500, "%s: stderr 1 message\n", __func__);
    snprintf(end(ans->message[1]), 500, "%s: stderr 2 message\n", __func__);
    ans->status = 1;
  }
  if (istatus == 2 || istatus == 12 || istatus == 23 || istatus == 123) {
    snprintf(end(ans->message[2]), 500, "%s: stderr 1 warning\n", __func__);
    snprintf(end(ans->message[2]), 500, "%s: stderr 2 warning\n", __func__);
    ans->status = 2;
  }
  if (istatus == 3 || istatus == 13 || istatus == 23 || istatus == 123) {
    snprintf(end(ans->message[3]), 500, "%s: stderr 1 error\n", __func__);
    snprintf(end(ans->message[3]), 500, "%s: stderr 2 error\n", __func__); // printed too because errors appended and raised from ansMsg later on
    ans->status = 3;
  }
  ans->int_v[0] = ans->status;
}
SEXP testMsgR(SEXP status, SEXP x, SEXP k) {
  if (!isInteger(status) || !isInteger(x) || !isInteger(k))
    error(_("internal error: status, nx, nk must be integer")); // # nocov
  int protecti = 0;
  const bool verbose = GetVerbose();
  int istatus = INTEGER(status)[0], nx = INTEGER(x)[0], nk = INTEGER(k)[0];

  // TODO below chunk into allocansList helper, not for 1.12.4
  SEXP ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;
  ans_t *vans = (ans_t *)R_alloc(nx*nk, sizeof(ans_t));
  if (verbose)
    Rprintf(_("%s: allocating memory for results %dx%d\n"), __func__, nx, nk);
  for (R_len_t i=0; i<nx; i++) {
    for (R_len_t j=0; j<nk; j++) {
      SET_VECTOR_ELT(ans, i*nk+j, allocVector(INTSXP, 1));
      vans[i*nk+j] = ((ans_t) { .int_v=INTEGER(VECTOR_ELT(ans, i*nk+j)), .status=0, .message={"\0","\0","\0","\0"} });
    }
  }

  #pragma omp parallel for schedule(dynamic) collapse(2) num_threads(getDTthreads(nx*nk, false))
  for (R_len_t i=0; i<nx; i++) {
    for (R_len_t j=0; j<nk; j++) {
      testRaiseMsg(&vans[i*nk+j], istatus, verbose);
    }
  }

  ansMsg(vans, nx*nk, verbose, __func__);
  UNPROTECT(protecti);
  return ans;
}
