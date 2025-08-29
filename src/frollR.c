#include "data.table.h"  // first (before Rdefines.h) for clang-13-omp, #5122
#include <Rdefines.h>

// validate and coerce to list of real
SEXP coerceX(SEXP obj) {
  // accept atomic/list of integer/logical/real returns list of real
  int protecti = 0;
  if (isVectorAtomic(obj)) {
    SEXP obj1 = obj;
    obj = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(obj, 0, obj1);
  }
  R_len_t nobj = length(obj);
  SEXP x = PROTECT(allocVector(VECSXP, nobj)); protecti++;
  for (R_len_t i=0; i<nobj; i++) {
    SEXP this_obj = VECTOR_ELT(obj, i);
    if (!(isReal(this_obj) || isInteger(this_obj) || isLogical(this_obj)))
      error(_("x must be of type numeric or logical, or a list, data.frame or data.table of such"));
    SET_VECTOR_ELT(x, i, coerceAs(this_obj, PROTECT(ScalarReal(NA_REAL)), /*copyArg=*/ScalarLogical(false))); // copyArg=false will make type-class match to return as-is, no copy
    UNPROTECT(1); // as= input to coerceAs()
  }
  UNPROTECT(protecti);
  return x;
}
// validate and coerce to integer or list of integer
SEXP coerceK(SEXP obj, bool adaptive) {
  int protecti = 0;
  SEXP ans = R_NilValue;
  if (!adaptive) {
    if (isNewList(obj))
      error(_("n must be integer, list is accepted for adaptive TRUE"));
    if (isInteger(obj)) {
      ans = obj;
    } else if (isReal(obj)) {
      ans = PROTECT(coerceVector(obj, INTSXP)); protecti++;
    } else {
      error(_("n must be integer"));
    }
    int nk = length(obj);
    R_len_t i = 0;
    int *iik = INTEGER(ans);
    while (i < nk && iik[i] > 0) i++;
    if (i != nk)
      error(_("n must be positive integer values (> 0)"));
  } else {
    if (isVectorAtomic(obj)) {
      ans = PROTECT(allocVector(VECSXP, 1)); protecti++;
      if (isInteger(obj)) {
        SET_VECTOR_ELT(ans, 0, obj);
      } else if (isReal(obj)) {
        SET_VECTOR_ELT(ans, 0, coerceVector(obj, INTSXP));
      } else {
        error(_("n must be integer vector or list of integer vectors"));
      }
    } else {
      int nk = length(obj);
      ans = PROTECT(allocVector(VECSXP, nk)); protecti++;
      for (int i=0; i<nk; i++) {
        if (isInteger(VECTOR_ELT(obj, i))) {
          SET_VECTOR_ELT(ans, i, VECTOR_ELT(obj, i));
        } else if (isReal(VECTOR_ELT(obj, i))) {
          SET_VECTOR_ELT(ans, i, coerceVector(VECTOR_ELT(obj, i), INTSXP));
        } else {
          error(_("n must be integer vector or list of integer vectors"));
        }
      }
    }
    int nx = length(VECTOR_ELT(ans, 0));
    for (int i=0; i<length(ans); i++) {
      int *iik = INTEGER(VECTOR_ELT(ans, i));
      R_len_t ii = 0;
      while (ii < nx && iik[ii] > 0) ii++;
      if (ii != nx)
        error(_("n must be positive integer values (> 0)"));
    }
  }
  UNPROTECT(protecti);
  return ans;
}

SEXP frollfunR(SEXP fun, SEXP xobj, SEXP kobj, SEXP fill, SEXP algo, SEXP align, SEXP narm, SEXP hasnf, SEXP adaptive) {
  int protecti = 0;
  const bool verbose = GetVerbose();

  if (!xlength(xobj))
    return(xobj);                                                // empty input: NULL, list()
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  SEXP x = PROTECT(coerceX(xobj)); protecti++;
  R_len_t nx = length(x);                                       // number of columns to roll on

  if (xlength(kobj) == 0)                                       // check that window is non zero length
    error(_("n must be non 0 length"));

  if (!IS_TRUE_OR_FALSE(adaptive))
    error(_("%s must be TRUE or FALSE"), "adaptive");
  bool badaptive = LOGICAL(adaptive)[0];

  SEXP k = PROTECT(coerceK(kobj, badaptive)); protecti++;
  int nk = length(k);
  int *ik = NULL; int **lk = NULL;
  if (!badaptive) {
    ik = INTEGER(k);
  } else {
    lk = (int**)R_alloc(nk, sizeof(int*));
    for (int j=0; j<nk; j++)
      lk[j] = INTEGER(VECTOR_ELT(k, j));
  }

  if (!IS_TRUE_OR_FALSE(narm))
    error(_("%s must be TRUE or FALSE"), "na.rm");

  if (!isLogical(hasnf) || length(hasnf)!=1)
    error(_("has.nf must be TRUE, FALSE or NA"));
  if (LOGICAL(hasnf)[0]==FALSE && LOGICAL(narm)[0])
    error(_("using has.nf FALSE and na.rm TRUE does not make sense, if you know there are non-finite values then use has.nf TRUE, otherwise leave it as default NA"));

  int ialign=-2;                                                   // decode align to integer
  if (!strcmp(CHAR(STRING_ELT(align, 0)), "right"))
    ialign = 1;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "center"))
    ialign = 0;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "left"))
    ialign = -1;
  else
    internal_error(__func__, "invalid %s argument in %s function should have been caught earlier.", "align", "rolling"); // # nocov

  if (badaptive && ialign==0) // support for left added in #5441
    error(_("using adaptive TRUE and align 'center' is not implemented"));

  SEXP ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++; // allocate list to keep results
  if (verbose)
    Rprintf(_("%s: allocating memory for results %dx%d\n"), __func__, nx, nk);
  ans_t *dans = (ans_t *)R_alloc(nx*nk, sizeof(*dans));         // answer columns as array of ans_t struct
  double** dx = (double**)R_alloc(nx, sizeof(*dx));         // pointers to source columns
  uint64_t* inx = (uint64_t*)R_alloc(nx, sizeof(*inx));     // to not recalculate `length(x[[i]])` we store it in extra array
  for (R_len_t i=0; i<nx; i++) {
    inx[i] = xlength(VECTOR_ELT(x, i));                         // for list input each vector can have different length
    for (R_len_t j=0; j<nk; j++) {
      if (badaptive) {                                          // extra input validation
        if (i > 0 && (inx[i]!=inx[i-1]))                        // variable length list input not allowed for adaptive roll
          error(_("adaptive rolling function can only process 'x' having equal length of elements, like data.table or data.frame; If you want to call rolling function on list having variable length of elements call it for each field separately"));
        if (xlength(VECTOR_ELT(k, j))!=inx[0])                 // check that length of integer vectors in n list match to xrows[0] ([0] and not [i] because there is above check for equal xrows)
          error(_("length of integer vector(s) provided as list to 'n' argument must be equal to number of observations provided in 'x'"));
      }
      SET_VECTOR_ELT(ans, i*nk+j, allocVector(REALSXP, inx[i]));// allocate answer vector for this column-window
      dans[i*nk+j] = ((ans_t) { .dbl_v=REAL(VECTOR_ELT(ans, i*nk+j)), .status=0, .message={"\0","\0","\0","\0"} });
    }
    dx[i] = REAL(VECTOR_ELT(x, i));                             // assign source columns to C pointers
  }

  rollfun_t rfun = MEAN; // adding fun needs to be here and data.table.h, initialize to keep compiler happy
  if (!strcmp(CHAR(STRING_ELT(fun, 0)), "mean")) {
    rfun = MEAN;
  } else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "sum")) {
    rfun = SUM;
  } else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "max")) {
    rfun = MAX;
  } else {
    internal_error(__func__, "invalid %s argument in %s function should have been caught earlier", "fun", "rolling"); // # nocov
  }

  if (length(fill) != 1)
    error(_("fill must be a vector of length 1"));
  if (!isInteger(fill) && !isReal(fill) && !isLogical(fill))
    error(_("fill must be numeric or logical"));
  double dfill = REAL(PROTECT(coerceAs(fill, PROTECT(ScalarReal(NA_REAL)), ScalarLogical(true))))[0]; protecti++;
  UNPROTECT(1); // as= input to coerceAs()

  bool bnarm = LOGICAL(narm)[0];

  int ihasnf =                                                  // plain C tri-state boolean as integer
    LOGICAL(hasnf)[0]==NA_LOGICAL ? 0 :                         // hasnf NA, default, no info about NA
    LOGICAL(hasnf)[0]==TRUE ? 1 :                               // hasnf TRUE, might be some NAs
    -1;                                                         // hasnf FALSE, there should be no NAs // or there must be no NAs for rollmax #5441

  unsigned int ialgo=-1;                                           // decode algo to integer
  if (!strcmp(CHAR(STRING_ELT(algo, 0)), "fast"))
    ialgo = 0;                                                  // fast = 0
  else if (!strcmp(CHAR(STRING_ELT(algo, 0)), "exact"))
    ialgo = 1;                                                  // exact = 1
  else
    internal_error(__func__, "invalid %s argument in %s function should have been caught earlier", "algo", "rolling"); // # nocov

  if (verbose) {
    if (ialgo==0)
      Rprintf(_("%s: %d column(s) and %d window(s), if product > 1 then entering parallel execution\n"), __func__, nx, nk);
    else if (ialgo==1)
      Rprintf(_("%s: %d column(s) and %d window(s), not entering parallel execution here because algo='exact' will compute results in parallel\n"), __func__, nx, nk);
  }
  #pragma omp parallel for if (ialgo==0) schedule(dynamic) collapse(2) num_threads(getDTthreads(nx*nk, false))
  for (R_len_t i=0; i<nx; i++) {                                // loop over multiple columns
    for (R_len_t j=0; j<nk; j++) {                              // loop over multiple windows
      if (!badaptive) {
        frollfun(rfun, ialgo, dx[i], inx[i], &dans[i*nk+j], ik[j], ialign, dfill, bnarm, ihasnf, verbose);
      } else {
        frolladaptivefun(rfun, ialgo, dx[i], inx[i], &dans[i*nk+j], lk[j], dfill, bnarm, ihasnf, verbose);
      }
    }
  }

  ansGetMsgs(dans, nx*nk, verbose, __func__);                    // raise errors and warnings, as of now messages are not being produced

  if (verbose)
    Rprintf(_("%s: processing of %d column(s) and %d window(s) took %.3fs\n"), __func__, nx, nk, omp_get_wtime()-tic);

  UNPROTECT(protecti);
  return isVectorAtomic(xobj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}

// helper to find biggest window width for adaptive frollapply
int maxk(int *k, uint64_t len) {
  int mk = k[0];
  for (uint64_t i=1; i<len; i++)
    if (k[i] > mk)
      mk = k[i];
  return mk;
}
SEXP frollapplyR(SEXP fun, SEXP xobj, SEXP kobj, SEXP fill, SEXP align, SEXP adaptive, SEXP rho) {
  int protecti = 0;
  const bool verbose = GetVerbose();

  if (!isFunction(fun))
    internal_error(__func__, "'fun' must be a function"); // # nocov
  if (!isEnvironment(rho))
    internal_error(__func__, "'rho' should be an environment"); // # nocov

  if (!xlength(xobj))
    return(xobj);
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  SEXP x = PROTECT(coerceX(xobj)); protecti++;
  R_len_t nx = length(x);

  if (xlength(kobj) == 0)
    error(_("n must be non 0 length"));

  if (!IS_TRUE_OR_FALSE(adaptive))
    error(_("%s must be TRUE or FALSE"), "adaptive");
  bool badaptive = LOGICAL(adaptive)[0];

  SEXP k = PROTECT(coerceK(kobj, badaptive)); protecti++;
  int nk = length(k);
  int *ik = NULL; int **lk = NULL;
  if (!badaptive) {
    ik = INTEGER(k);
  } else {
    lk = (int**)R_alloc(nk, sizeof(int*));
    for (int j=0; j<nk; j++)
      lk[j] = INTEGER(VECTOR_ELT(k, j));
  }

  int ialign=-2;
  if (!strcmp(CHAR(STRING_ELT(align, 0)), "right")) {
    ialign = 1;
  } else if (!strcmp(CHAR(STRING_ELT(align, 0)), "center")) {
    ialign = 0;
  } else if (!strcmp(CHAR(STRING_ELT(align, 0)), "left")) {
    ialign = -1;
  } else {
    internal_error(__func__, "invalid %s argument in %s function should have been caught earlier", "align", "rolling"); // # nocov
  }

  if (badaptive && ialign==0)
    error(_("using adaptive TRUE and align 'center' is not implemented"));

  if (length(fill) != 1)
    error(_("fill must be a vector of length 1"));
  if (!isInteger(fill) && !isReal(fill) && !isLogical(fill))
    error(_("fill must be numeric or logical"));
  double dfill = REAL(PROTECT(coerceAs(fill, PROTECT(ScalarReal(NA_REAL)), ScalarLogical(true))))[0]; protecti++;
  UNPROTECT(1); // as= input to coerceAs()

  SEXP ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;
  if (verbose)
    Rprintf(_("%s: allocating memory for results %dx%d\n"), __func__, nx, nk);
  ans_t *dans = (ans_t *)R_alloc(nx*nk, sizeof(*dans));
  double** dx = (double**)R_alloc(nx, sizeof(*dx));
  uint64_t* inx = (uint64_t*)R_alloc(nx, sizeof(*inx));
  for (R_len_t i=0; i<nx; i++) {
    inx[i] = xlength(VECTOR_ELT(x, i));
    for (R_len_t j=0; j<nk; j++) {
      if (badaptive) {
        if (i > 0 && (inx[i]!=inx[i-1]))
          error(_("adaptive rolling function can only process 'x' having equal length of elements, like data.table or data.frame; If you want to call rolling function on list having variable length of elements call it for each field separately"));
        if (xlength(VECTOR_ELT(k, j))!=inx[0])
          error(_("length of integer vector(s) provided as list to 'n' argument must be equal to number of observations provided in 'x'"));
      }
      SET_VECTOR_ELT(ans, i*nk+j, allocVector(REALSXP, inx[i]));
      dans[i*nk+j] = ((ans_t) { .dbl_v=REAL(VECTOR_ELT(ans, i*nk+j)), .status=0, .message={"\0","\0","\0","\0"} });
    }
    dx[i] = REAL(VECTOR_ELT(x, i));
  }

  SEXP pw, pc;

  // in the outer loop we handle vectorized k argument
  // for each k we need to allocate a width window object: pw
  // we also need to construct distinct R call pointing to that window
  if (!badaptive) {
    for (R_len_t j=0; j<nk; j++) {
      pw = PROTECT(allocVector(REALSXP, ik[j]));
      pc = PROTECT(LCONS(fun, LCONS(pw, LCONS(R_DotsSymbol, R_NilValue))));
      for (R_len_t i=0; i<nx; i++) {
        frollapply(dx[i], inx[i], REAL(pw), ik[j], &dans[i*nk+j], ialign, dfill, pc, rho, verbose);
      }
      UNPROTECT(2);
    }
  } else {
    for (R_len_t j=0; j<nk; j++) {
      pw = PROTECT(allocVector(REALSXP, maxk(lk[j], inx[0]))); // max window size, inx[0] because inx is constant for adaptive
      SET_GROWABLE_BIT(pw); // so we can set length of window for each observation
      pc = PROTECT(LCONS(fun, LCONS(pw, LCONS(R_DotsSymbol, R_NilValue))));
      for (R_len_t i=0; i<nx; i++) {
        frolladaptiveapply(dx[i], inx[i], pw, lk[j], &dans[i*nk+j], dfill, pc, rho, verbose);
      }
      UNPROTECT(2);
    }
  }

  if (verbose)
    Rprintf(_("%s: processing of %d column(s) and %d window(s) took %.3fs\n"), __func__, nx, nk, omp_get_wtime()-tic);

  UNPROTECT(protecti);
  return isVectorAtomic(xobj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
