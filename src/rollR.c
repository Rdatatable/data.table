#include "rollR.h"

SEXP rollfun(SEXP fun, SEXP obj, SEXP k, SEXP fill, SEXP exact, SEXP align, SEXP partial, SEXP narm, SEXP hasna, SEXP adaptive, SEXP verbose) {
  int protecti=0;
  int iverbose = INTEGER(verbose)[0];
  
  if (!xlength(obj)) return(obj);                               // empty input: NULL, list()
  SEXP x;                                                       // holds input data as list
  if (isVectorAtomic(obj)) {                                    // wrap atomic vector into list
    x = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(x, 0, obj);
  } else {
    x = obj;
  }

  if (!isNewList(x))
    error("x must be a list, data.frame or data.table");

  R_len_t i=0, nx=length(x);                                    // check that every column is double/integer type, it will be coerced to double from integer anyway
  while (i < nx && (isReal(VECTOR_ELT(x, i)) || isInteger(VECTOR_ELT(x, i)))) i++;
  if (i != nx)
    error("x must be list, data.frame or data.table of numeric types");

  R_len_t nk=length(k);
  if (nk == 0)                                                  // check that window is non zero length
    error("n must be non 0 length");

  if (!isLogical(adaptive) || length(adaptive) != 1 || LOGICAL(adaptive)[0] == NA_LOGICAL)
    error("adaptive must be logical TRUE or FALSE");
  bool badaptive = LOGICAL(adaptive)[0];
  
  SEXP kl = R_NilValue;                                         // holds adaptive window width, if doing adaptive roll fun
  if (!badaptive) {                                             // validating n input for adaptive=FALSE
    if (isNewList(k))
      error("n must be integer, list is accepted for adaptive TRUE");
    //TODO move R's n=as.integer(n) to C
    if (!isInteger(k))                                          // check that k is integer vector
      error("n must be integer");
    R_len_t i=0;                                                // check that all window values positive
    while (i < nk && INTEGER(k)[i] > 0) i++;
    if (i != nk)
      error("n must be positive integer values (> 0)");
  } else if (badaptive) {                                       // validating n input for adaptive=TRUE
    if (isVectorAtomic(k)) {                                    // if not-list then wrap into list
      kl = PROTECT(allocVector(VECSXP, 1)); protecti++;
      SET_VECTOR_ELT(kl, 0, k);
      nk = 1;
    } else {
      kl = k;
    }
    //TODO move R's n=as.integer(n) to C
    R_len_t i=0;                                                // check that every column is integer type
    while (i < nk && isInteger(VECTOR_ELT(kl, i))) i++;
    if (i != nk)
      error("n must be list of integer vectors when adaptive TRUE");
  }
  int* ikl[nk];                                                 // pointers to adaptive window width
  if (badaptive)
    for (int j=0; j<nk; j++) ikl[j] = INTEGER(VECTOR_ELT(kl, j));
  
  if (length(fill) != 1)
    error("fill must be a vector of length 1");

  if (!isLogical(partial) || length(partial)!=1 || LOGICAL(partial)[0]==NA_LOGICAL)
    error("partial must be TRUE or FALSE");
  
  if (!isLogical(exact) || length(exact)!=1 || LOGICAL(exact)[0]==NA_LOGICAL)
    error("exact must be TRUE or FALSE");
  
  if (!isLogical(narm) || length(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL)
    error("na.rm must be TRUE or FALSE");

  if (!isLogical(hasna) || length(hasna)!=1)
    error("hasNA must be TRUE, FALSE or NA");
  if (LOGICAL(hasna)[0]==FALSE && LOGICAL(narm)[0])
    error("using hasNA FALSE and na.rm TRUE does not make sense, if you know there are NA values use hasNA TRUE, otherwise leave it as default NA");

  SEXP ans;
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;      // allocate list to keep results
  double* dans[nx*nk];                                          // pointers to answer columns
  double* dx[nx];                                               // pointers to source columns
  uint_fast64_t inx[nx];                                        // to not recalculate `length(x[[i]])` we store it in extra array
  if (iverbose>0) Rprintf("rollfun: allocating memory for results\n");
  for (R_len_t i=0; i<nx; i++) {
    inx[i] = xlength(VECTOR_ELT(x, i));                         // for list input each vector can have different length
    for (R_len_t j=0; j<nk; j++) {
      if (badaptive) {                                          // extra input validation
        if (i > 0 && (inx[i]!=inx[i-1]))                        // variable length list input not allowed for adaptive roll
          error("Adaptive rolling function can only process 'x' having equal length of elements, like data.table or data.frame. If you want to call rolling function on list having variable length of elements call it for each field separately.");
        if (xlength(VECTOR_ELT(kl, j))!=inx[0])                 // check that length of integer vectors in n list match to xrows[0] ([0] and not [i] because there is above check for equal xrows)
          error("Length of integer vector(s) provided as list to 'n' argument must be equal to number of observations provided in 'x'.");
      }
      SET_VECTOR_ELT(ans, i*nk+j, allocVector(REALSXP, inx[i]));// allocate answer vector for this column-window
      dans[i*nk+j] = REAL(VECTOR_ELT(ans, i*nk+j));
    }
    dx[i] = REAL(AS_NUMERIC(VECTOR_ELT(x, i)));
  }

  enum {MEAN, SUM} sfun;
  if (!strcmp(CHAR(STRING_ELT(fun, 0)), "mean")) sfun = MEAN;
  else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "sum")) sfun = SUM;
  else error("Internal error: invalid fun argument in rolling function, should have been caught before. please report to data.table issue tracker.");

  int* ik = INTEGER(k);                                         // pointer to non-adaptive window width, still can be vector when doing multiple windows
  
  int ialign;                                                   // decode align to integer
  if (!strcmp(CHAR(STRING_ELT(align, 0)), "right")) ialign = 1;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "center")) ialign = 0;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "left")) ialign = -1;
  else error("Internal error: invalid align argument in rolling function, should have been caught before. please report to data.table issue tracker.");

  SEXP rdfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
  double dfill = REAL(rdfill)[0];

  bool bpartial = LOGICAL(partial)[0];
  bool bexact = LOGICAL(exact)[0];
  bool bnarm = LOGICAL(narm)[0];
  
  int ihasna =                                                  // plain C tri-state boolean as integer
    LOGICAL(hasna)[0]==NA_LOGICAL ? 0 :                         // hasna NA, default, no info about NA
    LOGICAL(hasna)[0]==TRUE ? 1 :                               // hasna TRUE, might be some NAs
    -1;                                                         // hasna FALSE, there should be no NAs

  if (nx==1 && nk==1) {                                         // no need to init openmp for single thread call
    if (iverbose>0) Rprintf("rollfun: single window and single column, skipping parallel execution\n");
    switch (sfun) {
      case MEAN :
        if (!badaptive) rollmeanVector(dx[0], inx[0], dans[0], ik[0], ialign, dfill, bpartial, bexact, bnarm, ihasna, iverbose);
        else rollmeanVectorAdaptive(dx[0], inx[0], dans[0], ikl[0], dfill, bexact, bnarm, ihasna, iverbose);
        break;
      case SUM :
        if (!badaptive) rollsumVector(dx[0], inx[0], dans[0], ik[0], ialign, dfill, bpartial, bexact, bnarm, ihasna, iverbose);
        else rollsumVectorAdaptive(dx[0], inx[0], dans[0], ikl[0], dfill, bexact, bnarm, ihasna, iverbose);
        break;
    }
  } else {
    if (iverbose>0) Rprintf("rollfun: multiple window or columns, entering parallel execution, but actually single threaded due to enabled verbose which is not thread safe\n");
    for (R_len_t i=0; i<nx; i++) {                              // loop over columns
      #pragma omp parallel num_threads(iverbose==0 ? MIN(getDTthreads(), nx) : 1)
      {
        #pragma omp for schedule(dynamic)
        for (R_len_t j=0; j<nk; j++) {                          // loop over multiple windows
          switch (sfun) {
            case MEAN :
              if (!badaptive) rollmeanVector(dx[i], inx[i], dans[i*nk+j], ik[j], ialign, dfill, bpartial, bexact, bnarm, ihasna, iverbose);
              else rollmeanVectorAdaptive(dx[i], inx[i], dans[i*nk+j], ikl[j], dfill, bexact, bnarm, ihasna, iverbose);
              break;
            case SUM :
              if (!badaptive) rollsumVector(dx[i], inx[i], dans[i*nk+j], ik[j], ialign, dfill, bpartial, bexact, bnarm, ihasna, iverbose);
              else rollsumVectorAdaptive(dx[i], inx[i], dans[i*nk+j], ikl[j], dfill, bexact, bnarm, ihasna, iverbose);
              break;
          }
        } // end of pragma omp for schedule
      } // end of pragma omp parallel num_threads
    }
  }
  
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
