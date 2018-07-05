#include "frollR.h"

SEXP frollfunR(SEXP fun, SEXP obj, SEXP k, SEXP fill, SEXP exact, SEXP align, SEXP narm, SEXP hasna, SEXP adaptive, SEXP verbose) {
  int protecti=0;
  if (!isLogical(verbose) || length(verbose)!=1 || LOGICAL(verbose)[0]==NA_LOGICAL)
    error("verbose must be TRUE or FALSE");
  bool bverbose = LOGICAL(verbose)[0];
  
  if (!xlength(obj)) return(obj);                               // empty input: NULL, list()
  SEXP x;                                                       // holds input data as list
  if (isVectorAtomic(obj)) {                                    // wrap atomic vector into list
    x = PROTECT(allocVector(VECSXP, 1)); protecti++;
    if (isReal(obj)) {
      SET_VECTOR_ELT(x, 0, obj);
    } else if (isInteger(obj)) {
      SET_VECTOR_ELT(x, 0, coerceVector(obj, REALSXP));
    } else {
      error("x must be of type numeric");
    }
  } else {
    R_len_t nobj=length(obj);
    x = PROTECT(allocVector(VECSXP, nobj)); protecti++;
    for (R_len_t i=0; i<nobj; i++) {
      if (isReal(VECTOR_ELT(obj, i))) {
        SET_VECTOR_ELT(x, i, VECTOR_ELT(obj, i));
      } else if (isInteger(VECTOR_ELT(obj, i))) {               // coerce integer types to double
        SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(obj, i), REALSXP));
      } else {
        error("x must be list, data.frame or data.table of numeric types");
      }
    }
  }
  R_len_t nx=length(x);
  
  R_len_t nk=length(k);
  if (nk == 0)                                                  // check that window is non zero length
    error("n must be non 0 length");

  if (!isLogical(adaptive) || length(adaptive) != 1 || LOGICAL(adaptive)[0] == NA_LOGICAL)
    error("adaptive must be logical TRUE or FALSE");
  bool badaptive = LOGICAL(adaptive)[0];
  
  SEXP ik = R_NilValue;                                         // holds integer window width, if doing non-adaptive roll fun
  SEXP kl = R_NilValue;                                         // holds adaptive window width, if doing adaptive roll fun
  if (!badaptive) {                                             // validating n input for adaptive=FALSE
    if (isNewList(k))
      error("n must be integer, list is accepted for adaptive TRUE");
    
    if (isInteger(k)) {                                        // check that k is integer vector
      ik = k;
    } else if (isReal(k)) {                                    // if n is double then convert to integer
      ik = PROTECT(coerceVector(k, INTSXP)); protecti++;
    } else {
      error("n must be integer");
    }
    
    R_len_t i=0;                                                // check that all window values positive
    while (i < nk && INTEGER(ik)[i] > 0) i++;
    if (i != nk)
      error("n must be positive integer values (> 0)");
  } else if (badaptive) {                                       // validating n input for adaptive=TRUE
    if (isVectorAtomic(k)) {                                    // if not-list then wrap into list
      kl = PROTECT(allocVector(VECSXP, 1)); protecti++;
      if (isInteger(k)) {                                       // check that k is integer vector
        SET_VECTOR_ELT(kl, 0, k);
      } else if (isReal(k)) {                                   // if n is double then convert to integer
        SET_VECTOR_ELT(kl, 0, coerceVector(k, INTSXP));
      } else {
        error("n must be integer vector or list of integer vectors");
      }
      nk = 1;
    } else {
      kl = PROTECT(allocVector(VECSXP, nk)); protecti++;
      for (R_len_t i=0; i<nk; i++) {
        if (isInteger(VECTOR_ELT(k, i))) {
          SET_VECTOR_ELT(kl, i, VECTOR_ELT(k, i));
        } else if (isReal(VECTOR_ELT(k, i))) {                  // coerce double types to integer
          SET_VECTOR_ELT(kl, i, coerceVector(VECTOR_ELT(k, i), INTSXP));
        } else {
          error("n must be integer vector or list of integer vectors");
        }
      }
    }
  }
  int* ikl[nk];                                                 // pointers to adaptive window width
  if (badaptive)
    for (int j=0; j<nk; j++) ikl[j] = INTEGER(VECTOR_ELT(kl, j));
  
  if (!isLogical(exact) || length(exact)!=1 || LOGICAL(exact)[0]==NA_LOGICAL)
    error("exact must be TRUE or FALSE");
  
  if (!isLogical(narm) || length(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL)
    error("na.rm must be TRUE or FALSE");

  if (!isLogical(hasna) || length(hasna)!=1)
    error("hasNA must be TRUE, FALSE or NA");
  if (LOGICAL(hasna)[0]==FALSE && LOGICAL(narm)[0])
    error("using hasNA FALSE and na.rm TRUE does not make sense, if you know there are NA values use hasNA TRUE, otherwise leave it as default NA");

  int ialign;                                                   // decode align to integer
  if (!strcmp(CHAR(STRING_ELT(align, 0)), "right")) ialign = 1;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "center")) ialign = 0;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "left")) ialign = -1;
  else error("Internal error: invalid align argument in rolling function, should have been caught before. please report to data.table issue tracker.");

  if (badaptive && ialign!=1)
    error("using adaptive TRUE and align argument different than 'right' is not implemented");
  
  SEXP ans;
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;      // allocate list to keep results
  double* dans[nx*nk];                                          // pointers to answer columns
  double* dx[nx];                                               // pointers to source columns
  uint_fast64_t inx[nx];                                        // to not recalculate `length(x[[i]])` we store it in extra array
  if (bverbose) Rprintf("frollfunR: allocating memory for results %dx%d\n", nx, nk);
  for (R_len_t i=0; i<nx; i++) {
    inx[i] = xlength(VECTOR_ELT(x, i));                         // for list input each vector can have different length
    for (R_len_t j=0; j<nk; j++) {
      if (badaptive) {                                          // extra input validation
        if (i > 0 && (inx[i]!=inx[i-1]))                        // variable length list input not allowed for adaptive roll
          error("adaptive rolling function can only process 'x' having equal length of elements, like data.table or data.frame; If you want to call rolling function on list having variable length of elements call it for each field separately");
        if (xlength(VECTOR_ELT(kl, j))!=inx[0])                 // check that length of integer vectors in n list match to xrows[0] ([0] and not [i] because there is above check for equal xrows)
          error("length of integer vector(s) provided as list to 'n' argument must be equal to number of observations provided in 'x'");
      }
      SET_VECTOR_ELT(ans, i*nk+j, allocVector(REALSXP, inx[i]));// allocate answer vector for this column-window
      dans[i*nk+j] = REAL(VECTOR_ELT(ans, i*nk+j));
    }
    dx[i] = REAL(VECTOR_ELT(x, i));
  }

  enum {MEAN, SUM} sfun;
  if (!strcmp(CHAR(STRING_ELT(fun, 0)), "mean")) sfun = MEAN;
  else if (!strcmp(CHAR(STRING_ELT(fun, 0)), "sum")) sfun = SUM;
  else error("Internal error: invalid fun argument in rolling function, should have been caught before. please report to data.table issue tracker.");

  int* iik = INTEGER(ik);                                       // pointer to non-adaptive window width, still can be vector when doing multiple windows
  
  if (length(fill) != 1)
    error("fill must be a vector of length 1");
  
  double dfill;
  if (isInteger(fill)) {
    if (INTEGER(fill)[0]==NA_LOGICAL) {
      dfill = NA_REAL;
    } else {
      dfill = (double)INTEGER(fill)[0];
    }
  } else if (isReal(fill)) {
    dfill = REAL(fill)[0];
  } else if (isLogical(fill) && LOGICAL(fill)[0]==NA_LOGICAL){
    dfill = NA_REAL;
  } else {
    error("fill must be numeric");
  }
  
  bool bexact = LOGICAL(exact)[0];
  bool bnarm = LOGICAL(narm)[0];
  
  int ihasna =                                                  // plain C tri-state boolean as integer
    LOGICAL(hasna)[0]==NA_LOGICAL ? 0 :                         // hasna NA, default, no info about NA
    LOGICAL(hasna)[0]==TRUE ? 1 :                               // hasna TRUE, might be some NAs
    -1;                                                         // hasna FALSE, there should be no NAs

  if (nx==1 && nk==1) {                                         // no need to init openmp for single thread call
    if (bverbose) Rprintf("frollfunR: single column and single window, parallel processing by multiple answer vectors skipped\n");
    switch (sfun) {
      case MEAN :
        if (!badaptive) {
          if (!bexact) frollmean(dx[0], inx[0], dans[0], iik[0], ialign, dfill, bnarm, ihasna, bverbose);
          else frollmeanExact(dx[0], inx[0], dans[0], iik[0], ialign, dfill, bnarm, ihasna, bverbose);
        } else {
          if (!bexact) frollmeanAdaptive(dx[0], inx[0], dans[0], ikl[0], dfill, bnarm, ihasna, bverbose);
          else frollmeanExactAdaptive(dx[0], inx[0], dans[0], ikl[0], dfill, bnarm, ihasna, bverbose);
        }
        break;
      case SUM :
        if (!badaptive) {
          //if (!bexact) frollsum(dx[0], inx[0], dans[0], iik[0], ialign, dfill, bnarm, ihasna, bverbose);
          //else frollsumExact(dx[0], inx[0], dans[0], iik[0], ialign, dfill, bnarm, ihasna, bverbose);
        } else {
          //if (!bexact) frollsumAdaptive(dx[0], inx[0], dans[0], ikl[0], dfill, bnarm, ihasna, bverbose);
          //else frollsumExactAdaptive(dx[0], inx[0], dans[0], ikl[0], dfill, bnarm, ihasna, bverbose);
        }
        break;
    }
  } else {
    if (bverbose>0) Rprintf("frollfunR: %d column(s) and %d window(s), entering parallel execution, but actually single threaded due to enabled verbose which is not thread safe\n", nx, nk);
    #pragma omp parallel num_threads(bverbose ? 1 : MIN(getDTthreads(), nx*nk))
    {
      #pragma omp for schedule(auto) collapse(2)
      for (R_len_t i=0; i<nx; i++) {                              // loop over multiple columns
        for (R_len_t j=0; j<nk; j++) {                            // loop over multiple windows
          switch (sfun) {
            case MEAN :
              if (!badaptive) {
                if (!bexact) frollmean(dx[i], inx[i], dans[i*nk+j], iik[j], ialign, dfill, bnarm, ihasna, bverbose);
                else frollmeanExact(dx[i], inx[i], dans[i*nk+j], iik[j], ialign, dfill, bnarm, ihasna, bverbose);
              } else {
                if (!bexact) frollmeanAdaptive(dx[i], inx[i], dans[i*nk+j], ikl[j], dfill, bnarm, ihasna, bverbose);
                else frollmeanExactAdaptive(dx[i], inx[i], dans[i*nk+j], ikl[j], dfill, bnarm, ihasna, bverbose);
              }
              break;
            case SUM :
              if (!badaptive) {
                //if (!bexact) frollsum(dx[i], inx[i], dans[i*nk+j], iik[j], ialign, dfill, bnarm, ihasna, bverbose);
                //else frollsumExact(dx[i], inx[i], dans[i*nk+j], iik[j], ialign, dfill, bnarm, ihasna, bverbose);
              } else {
                //if (!bexact) frollsumAdaptive(dx[i], inx[i], dans[i*nk+j], ikl[j], dfill, bnarm, ihasna, bverbose);
                //else frollsumExactAdaptive(dx[i], inx[i], dans[i*nk+j], ikl[j], dfill, bnarm, ihasna, bverbose);
              }
              break;
          }
        } // end of j-windows loop
      } // end of i-columns loop
    } // end of omp parallel region
  }
  
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
