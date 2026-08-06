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
      error(_("'x' must be of type numeric or logical, or a list, data.frame or data.table of such"));
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
      error(_("'n' must be an integer, list is accepted for adaptive TRUE"));
    if (isInteger(obj)) {
      ans = obj;
    } else if (isReal(obj)) {
      ans = PROTECT(coerceVector(obj, INTSXP)); protecti++;
    } else {
      error(_("'n' must be an integer"));
    }
    int nk = length(obj);
    R_len_t i = 0;
    int *iik = INTEGER(ans);
    while (i < nk && iik[i] >= 0) i++;
    if (i != nk)
      error(_("'n' must be non-negative integer values (>= 0)"));
  } else {
    if (isVectorAtomic(obj)) {
      ans = PROTECT(allocVector(VECSXP, 1)); protecti++;
      if (isInteger(obj)) {
        SET_VECTOR_ELT(ans, 0, obj);
      } else if (isReal(obj)) {
        SET_VECTOR_ELT(ans, 0, coerceVector(obj, INTSXP));
      } else {
        error(_("'n' must be an integer vector or list of integer vectors"));
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
          error(_("'n' must be an integer vector or list of integer vectors"));
        }
      }
    }
    int nx = length(VECTOR_ELT(ans, 0));
    for (int i=0; i<length(ans); i++) {
      int *iik = INTEGER(VECTOR_ELT(ans, i));
      R_len_t ii = 0;
      while (ii < nx && iik[ii] >= 0) ii++;
      if (ii != nx)
        error(_("'n' must be non-negative integer values (>= 0)"));
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
    error(_("'%s' must be TRUE or FALSE"), "adaptive");
  bool badaptive = LOGICAL(adaptive)[0];

  SEXP k = PROTECT(coerceK(kobj, badaptive)); protecti++;
  int nk = length(k);
  const int *ik = NULL; const int **lk = NULL;
  if (!badaptive) {
    ik = INTEGER_RO(k);
  } else {
    lk = (const int **)R_alloc(nk, sizeof(*lk));
    for (int j=0; j<nk; j++)
      lk[j] = INTEGER_RO(VECTOR_ELT(k, j));
  }

  if (!IS_TRUE_OR_FALSE(narm))
    error(_("'%s' must be TRUE or FALSE"), "na.rm");

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
  const double** dx = (const double**)R_alloc(nx, sizeof(*dx));  // pointers to source columns
  uint64_t* inx = (uint64_t*)R_alloc(nx, sizeof(*inx));         // to not recalculate `length(x[[i]])` we store it in extra array
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
    dx[i] = REAL_RO(VECTOR_ELT(x, i));                             // assign source columns to C pointers
  }

  rollfun_t rfun = MEAN; // adding fun needs to be here and data.table.h, initialize to keep compiler happy
  if (!strcmp(CHAR(STRING_ELT(fun, 0)), "mean")) {
    rfun = MEAN;
  } else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "sum")) {
    rfun = SUM;
  } else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "max")) {
    rfun = MAX;
  } else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "min")) {
    rfun = MIN;
  } else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "prod")) {
    rfun = PROD;
  } else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "median")) {
    rfun = MEDIAN;
  } else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "var")) {
    rfun = VAR;
  } else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "sd")) {
    rfun = SD;
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

  bool par = nx*nk>1 && ialgo==0 && !badaptive; // for algo=exact and !badaptive we parallelize inside
  if (verbose) {
    if (par) {
      Rprintf(_("%s: computing %d column(s) and %d window(s) in parallel\n"), __func__, nx, nk);
    } else if (ialgo==1) {
      Rprintf(_("%s: computing %d column(s) and %d window(s) sequentially because algo='exact' is already parallelised within each rolling computation\n"), __func__, nx, nk);
    } else if (badaptive) {
      Rprintf(_("%s: computing %d column(s) and %d window(s) sequentially because adaptive=TRUE is already parallelised within each rolling computation\n"), __func__, nx, nk);
    } else if (nx*nk==1) {
      Rprintf(_("%s: computing %d column(s) and %d window(s) sequentially as there is only single rolling computation\n"), __func__, nx, nk);
    }
  }
  #pragma omp parallel for if (par) schedule(dynamic) collapse(2) num_threads(getDTthreads(nx*nk, false))
  for (R_len_t i=0; i<nx; i++) {                                // loop over multiple columns
    for (R_len_t j=0; j<nk; j++) {                              // loop over multiple windows
      if (!badaptive) {
        frollfun(rfun, ialgo, dx[i], inx[i], &dans[i*nk+j], ik[j], ialign, dfill, bnarm, ihasnf, verbose, /*par=*/!par); // par tells medianFast if it can use openmp so we avoid nested parallelism
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

// helper called from R to generate adaptive window for irregularly spaced time series
SEXP frolladapt(SEXP xobj, SEXP kobj, SEXP partial) {

  bool p = LOGICAL(partial)[0];
  int n = INTEGER(kobj)[0];
  if (n == NA_INTEGER)
    error(_("'n' must not have NAs"));
  if (n < 1L)
    error(_("'n' must be positive integer values (>= 1)"));
  const int *x = INTEGER_RO(xobj);
  int64_t len = XLENGTH(xobj); // can be 0

  if (len && x[0] == NA_INTEGER)
    error(_("index provided to 'x' must: be sorted, have no duplicates, have no NAs")); // error text for consistency to the one below
  for (int64_t i=1; i<len; i++) {
    if (x[i] <= x[i-1L])
      error(_("index provided to 'x' must: be sorted, have no duplicates, have no NAs"));
  }

  SEXP ans = PROTECT(allocVector(INTSXP, len));
  int *ians = INTEGER(ans);

  int64_t i = 0, j = 0;
  int first;
  if (len)
    first = x[0]+n-1;
  while (i < len) {
    int lhs = x[i], rhs = x[j];
    int an = i-j+1;                 // window we are currently looking at in this iteration
    if (an > n) {
      internal_error(__func__, "an > n, should not increment i in the first place"); // # nocov
    } else if (an == n) {           // an is same size as n, so we either have no gaps or will need to shrink an by j++
      if (lhs == rhs+n-1) {         // no gaps - or a n gaps and a n dups?
        ians[i] = n;                // could skip if pre-fill
        i++;
        j++;
      } else if (lhs > rhs+n-1) {   // need to shrink an
        j++;
      } else {
        internal_error(__func__, "not sorted, should be been detected by now"); // # nocov
      }
    } else if (an < n) {            // there are some gaps
      if (lhs == rhs+n-1) {         // gap and rhs matches the bound, so increment i and j
        ians[i] = an;
        i++;
        j++;
      } else if (lhs > rhs+n-1L) {  // need to shrink an
        ians[i] = an;               // likely to be overwritten by smaller an if shrinking continues because i is not incremented in this iteration
        j++;
      } else if (lhs < rhs+n-1L) {
        ians[i] = !p && lhs<first ? n : an; // for i==j ans=1L, unless !partial, then ans=n
        i++;
      }
    }
  }
  UNPROTECT(1);
  return ans;
}
