#include "data.table.h"  // first (before Rdefines.h) for clang-13-omp, #5122
#include <Rdefines.h>

/*
 * find end of a string, used to append verbose messages or warnings
 */
char *end(char *start) {
  return strchr(start, 0);
}

/*
 * logging status and messages, warnings, errors to ans_t
 */
void ansSetMsg(ans_t *ans, uint8_t status, const char *msg, const char *func) {
  if (status > ans->status)
    ans->status = status;
  snprintf(end(ans->message[status]), 500, _(msg), func); // func should be passed via ... really, thus this helper cannot replace all cases we need
  // implicit n_message limit discussed here: https://github.com/Rdatatable/data.table/issues/3423#issuecomment-487722586
}

/*
 * function to print verbose messages, stderr messages, warnings and errors stored in ans_t struct
 */
void ansGetMsgs(ans_t *ans, int n, bool verbose, const char *func) {
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
 * R interface to test ansGetMsgs function
 * see inst/tests/types.Rraw
 */
void testRaiseMsg(ans_t *ans, int istatus, bool verbose) {
  if (verbose) {
    ansSetMsg(ans, 0, "%s: stdout 1 message\n", __func__);
    ansSetMsg(ans, 0, "%s: stdout 2 message\n", __func__);
  }
  if (istatus == 1 || istatus == 12 || istatus == 13 || istatus == 123) {
    ansSetMsg(ans, 1, "%s: stderr 1 message\n", __func__);
    ansSetMsg(ans, 1, "%s: stderr 2 message\n", __func__);
  }
  if (istatus == 2 || istatus == 12 || istatus == 23 || istatus == 123) {
    ansSetMsg(ans, 2, "%s: stderr 1 warning\n", __func__);
    ansSetMsg(ans, 2, "%s: stderr 2 warning\n", __func__);
  }
  if (istatus == 3 || istatus == 13 || istatus == 23 || istatus == 123) {
    ansSetMsg(ans, 3, "%s: stderr 1 error\n", __func__);
    ansSetMsg(ans, 3, "%s: stderr 2 error\n", __func__); // printed too because errors appended and raised from ansGetMsgs later on
  }
  ans->int_v[0] = ans->status; // just a return value of status
}
SEXP testMsgR(SEXP status, SEXP x, SEXP k) {
  if (!isInteger(status) || !isInteger(x) || !isInteger(k))
    error(_("internal error: status, nx, nk must be integer")); // # nocov
  int protecti = 0;
  const bool verbose = GetVerbose();
  int istatus = INTEGER(status)[0], nx = INTEGER(x)[0], nk = INTEGER(k)[0];

  // TODO below chunk into allocAnsList helper - not easy for variable length of inner vectors
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

  ansGetMsgs(vans, nx*nk, verbose, __func__);
  UNPROTECT(protecti);
  return ans;
}
