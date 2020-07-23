#include "data.table.h"

/* fast rolling mean - router
 * early stopping for window bigger than input
 * also handles 'align' in single place
 * algo = 0: frollmeanFast
 *   adding/removing in/out of sliding window of observations
 * algo = 1: frollmeanExact
 *   recalculate whole mean for each observation, roundoff correction is adjusted, also support for NaN and Inf
 */
void frollmean(unsigned int algo, double *x, uint64_t nx, ans_t *ans, int k, int align, double fill, bool narm, int hasna, bool verbose) {
  if (nx < k) {                                                 // if window width bigger than input just return vector of fill values
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width longer than input vector, returning all NA vector\n"), __func__);
    // implicit n_message limit discussed here: https://github.com/Rdatatable/data.table/issues/3423#issuecomment-487722586
    for (int i=0; i<nx; i++) {
      ans->dbl_v[i] = fill;
    }
    return;
  }
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  if (algo==0) {
    frollmeanFast(x, nx, ans, k, fill, narm, hasna, verbose);
  } else if (algo==1) {
    frollmeanExact(x, nx, ans, k, fill, narm, hasna, verbose);
  }
  if (ans->status < 3 && align < 1) {                           // align center or left, only when no errors occurred
    int k_ = align==-1 ? k-1 : floor(k/2);                      // offset to shift
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: align %d, shift answer by %d\n"), __func__, align, -k_);
    memmove((char *)ans->dbl_v, (char *)ans->dbl_v + (k_*sizeof(double)), (nx-k_)*sizeof(double)); // apply shift to achieve expected align
    for (uint64_t i=nx-k_; i<nx; i++) {                         // fill from right side
      ans->dbl_v[i] = fill;
    }
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: processing algo %u took %.3fs\n"), __func__, algo, omp_get_wtime()-tic);
}
/* fast rolling mean - fast
 * when no info on NA (hasNA argument) then assume no NAs run faster version
 * rollmean implemented as single pass sliding window for align="right"
 * if NAs detected re-run rollmean implemented as single pass sliding window with NA support
 */
void frollmeanFast(double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", window %d, hasna %d, narm %d\n"), "frollmeanFast", (uint64_t)nx, k, hasna, (int)narm);
  long double w = 0.0;                                          // sliding window aggregate
  bool truehasna = hasna>0;                                     // flag to re-run with NA support if NAs detected
  if (!truehasna) {
    int i;                                                      // iterator declared here because it is being used after foor loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window only
      w += x[i];                                                // add current row to sliding window
      ans->dbl_v[i] = fill;                                     // answers are fill for partial window
    }
    w += x[i];                                                  // i==k-1
    ans->dbl_v[i] = (double) (w / k);                           // first full sliding window, non-fill rollfun answer
    if (R_FINITE((double) w)) {                                 // proceed only if no NAs detected in first k obs, otherwise early stopping
      for (uint64_t i=k; i<nx; i++) {                           // loop over obs, complete window, all remaining after partial window
        w -= x[i-k];                                            // remove leaving row from sliding window
        w += x[i];                                              // add current row to sliding window
        ans->dbl_v[i] = (double) (w / k);                       // rollfun to answer vector
      }
      if (!R_FINITE((double) w)) {                              // mark to re-run with NA care
        if (hasna==-1) {                                        // raise warning
          ans->status = 2;
          snprintf(end(ans->message[2]), 500, _("%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning"), __func__);
        }
        if (verbose)
          snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n"), __func__);
        w = 0.0;
        truehasna = true;
      }
    } else {                                                    // early stopping branch when NAs detected in first k obs
      if (hasna==-1) {                                          // raise warning
        ans->status = 2;
        snprintf(end(ans->message[2]), 500, _("%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning"), __func__);
      }
      if (verbose)
        snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, skip non-NA attempt and run with extra care for NAs\n"), __func__);
      w = 0.0;
      truehasna = true;
    }
  }
  if (truehasna) {
    int nc = 0;                                                 // NA counter within sliding window
    int i;                                                      // iterator declared here because it is being used after foor loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window only
      if (R_FINITE(x[i])) {
        w += x[i];                                              // add only finite values to window aggregate
      } else {
        nc++;                                                   // increment NA count in current window
      }
      ans->dbl_v[i] = fill;                                     // partial window fill all
    }
    if (R_FINITE(x[i])) {
      w += x[i];                                                // i==k-1
    } else {
      nc++;
    }
    if (nc == 0) {
      ans->dbl_v[i] = (double) (w / k);                         // no NAs in first full window
    } else if (nc == k) {
      ans->dbl_v[i] = narm ? R_NaN : NA_REAL;                   // all values in sliding window are NA, expected output for fun(NA, na.rm=T/F)
    } else {
      ans->dbl_v[i] = narm ? (double) (w / (k - nc)) : NA_REAL; // some values in window are NA
    }
    for (uint64_t i=k; i<nx; i++) {                             // loop over obs, complete window, all remaining after partial window
      if (R_FINITE(x[i])) {
        w += x[i];                                              // add only finite to window aggregate
      } else {
        nc++;                                                   // increment NA count in current window
      }
      if (R_FINITE(x[i-k])) {
        w -= x[i-k];                                            // remove only finite from window aggregate
      } else {
        nc--;                                                   // decrement NA count in current window
      }
      if (nc == 0) {
        ans->dbl_v[i] = (double) (w / k);                       // no NAs in sliding window for present observation
      } else if (nc == k) {
        ans->dbl_v[i] = narm ? R_NaN : NA_REAL;                 // all values in window are NA, expected output for fun(NA, na.rm=T/F)
      } else {
        ans->dbl_v[i] = narm ? (double) (w / (k - nc)) : NA_REAL; // some values in window are NA
      }
    }
  }
}
/* fast rolling mean - exact
 * when no info on NA (hasNA argument) then assume no NAs run faster version, also when na.rm=FALSE faster version can proceed
 * rollmean implemented as mean of k obs for each observation for align="right"
 * if NAs detected and na.rm=TRUE then re-run rollmean implemented as mean of k bos for each observation with NA support
 */
void frollmeanExact(double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", window %d, hasna %d, narm %d\n"), "frollmeanExact", (uint64_t)nx, k, hasna, (int)narm);
  for (int i=0; i<k-1; i++) {                                   // fill partial window only
    ans->dbl_v[i] = fill;
  }
  bool truehasna = hasna>0;                                     // flag to re-run with NA support if NAs detected
  if (!truehasna || !narm) {
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {                           // loop on every observation with complete window, partial already filled in single threaded section
      if (narm && truehasna) {
        continue;                                               // if NAs detected no point to continue
      }
      long double w = 0.0;
      for (int j=-k+1; j<=0; j++) {                             // sub-loop on window width
        w += x[i+j];                                            // sum of window for particular observation
      }
      if (R_FINITE((double) w)) {                               // no need to calc roundoff correction if NAs detected as will re-call all below in truehasna==1
        long double res = w / k;                                // keep results as long double for intermediate processing
        long double err = 0.0;                                  // roundoff corrector
        for (int j=-k+1; j<=0; j++) {                           // nested loop on window width
          err += x[i+j] - res;                                  // measure difference of obs in sub-loop to calculated fun for obs
        }
        ans->dbl_v[i] = (double) (res + (err / k));             // adjust calculated rollfun with roundoff correction
      } else {
        if (!narm) {
          ans->dbl_v[i] = (double) (w / k);                     // NAs should be propagated
        }
        truehasna = true;                                       // NAs detected for this window, set flag so rest of windows will not be re-run
      }
    }
    if (truehasna) {
      if (hasna==-1) {                                          // raise warning
        ans->status = 2;
        snprintf(end(ans->message[2]), 500, _("%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning"), __func__);
      }
      if (verbose) {
        if (narm) {
          snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n"), __func__);
        } else {
          snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run\n"), __func__);
        }
      }
    }
  }
  if (truehasna && narm) {
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {                           // loop on every observation with complete window, partial already filled in single threaded section
      long double w = 0.0;
      int nc = 0;                                               // NA counter within sliding window
      for (int j=-k+1; j<=0; j++) {                             // nested loop on window width
        if (ISNAN(x[i+j])) {
          nc++;                                                 // increment NA count in current window
        } else {
          w += x[i+j];                                          // add observation to current window
        }
      }
      if (w > DBL_MAX) {
        ans->dbl_v[i] = R_PosInf;                               // handle Inf for na.rm=TRUE consistently to base R
      } else if (w < -DBL_MAX) {
        ans->dbl_v[i] = R_NegInf;
      } else {
        long double res = w / k;                                // keep results as long double for intermediate processing
        long double err = 0.0;                                  // roundoff corrector
        if (nc == 0) {                                          // no NAs in current window
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            err += x[i+j] - res;                                // measure roundoff for each obs in window
          }
          ans->dbl_v[i] = (double) (res + (err / k));           // adjust calculated fun with roundoff correction
        } else if (nc < k) {
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            if (!ISNAN(x[i+j])) {
              err += x[i+j] - res;                              // measure roundoff for each non-NA obs in window
            }
          }
          ans->dbl_v[i] = (double) (res + (err / (k - nc)));    // adjust calculated fun with roundoff correction
        } else {                                                // nc == k
          ans->dbl_v[i] = R_NaN;                                // all values NAs and narm so produce expected values
        }
      }
    }
  }
}

/* fast rolling sum */
void frollsum(unsigned int algo, double *x, uint64_t nx, ans_t *ans, int k, int align, double fill, bool narm, int hasna, bool verbose) {
  if (nx < k) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width longer than input vector, returning all NA vector\n"), __func__);
    for (int i=0; i<nx; i++) {
      ans->dbl_v[i] = fill;
    }
    return;
  }
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  if (algo==0) {
    frollsumFast(x, nx, ans, k, fill, narm, hasna, verbose);
  } else if (algo==1) {
    frollsumExact(x, nx, ans, k, fill, narm, hasna, verbose);
  }
  if (ans->status < 3 && align < 1) {
    int k_ = align==-1 ? k-1 : floor(k/2);
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: align %d, shift answer by %d\n"), __func__, align, -k_);
    memmove((char *)ans->dbl_v, (char *)ans->dbl_v + (k_*sizeof(double)), (nx-k_)*sizeof(double));
    for (uint64_t i=nx-k_; i<nx; i++) {
      ans->dbl_v[i] = fill;
    }
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: processing algo %u took %.3fs\n"), __func__, algo, omp_get_wtime()-tic);
}
void frollsumFast(double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", window %d, hasna %d, narm %d\n"), "frollsumFast", (uint64_t)nx, k, hasna, (int)narm);
  long double w = 0.0;
  bool truehasna = hasna>0;
  if (!truehasna) {
    int i;
    for (i=0; i<k-1; i++) {
      w += x[i];
      ans->dbl_v[i] = fill;
    }
    w += x[i];
    ans->dbl_v[i] = (double) w;
    if (R_FINITE((double) w)) {
      for (uint64_t i=k; i<nx; i++) {
        w -= x[i-k];
        w += x[i];
        ans->dbl_v[i] = (double) w;
      }
      if (!R_FINITE((double) w)) {
        if (hasna==-1) {
          ans->status = 2;
          snprintf(end(ans->message[2]), 500, _("%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning"), __func__);
        }
        if (verbose)
          snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n"), __func__);
        w = 0.0;
        truehasna = true;
      }
    } else {
      if (hasna==-1) {
        ans->status = 2;
        snprintf(end(ans->message[2]), 500, _("%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning"), __func__);
      }
      if (verbose)
        snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, skip non-NA attempt and run with extra care for NAs\n"), __func__);
      w = 0.0;
      truehasna = true;
    }
  }
  if (truehasna) {
    int nc = 0;
    int i;
    for (i=0; i<k-1; i++) {
      if (R_FINITE(x[i])) {
        w += x[i];
      } else {
        nc++;
      }
      ans->dbl_v[i] = fill;
    }
    if (R_FINITE(x[i])) {
      w += x[i];
    } else {
      nc++;
    }
    if (nc == 0) {
      ans->dbl_v[i] = (double) w;
    } else if (nc == k) {
      ans->dbl_v[i] = narm ? 0.0 : NA_REAL;
    } else {
      ans->dbl_v[i] = narm ? (double) w : NA_REAL;
    }
    for (uint64_t i=k; i<nx; i++) {
      if (R_FINITE(x[i])) {
        w += x[i];
      } else {
        nc++;
      }
      if (R_FINITE(x[i-k])) {
        w -= x[i-k];
      } else {
        nc--;
      }
      if (nc == 0) {
        ans->dbl_v[i] = (double) w;
      } else if (nc == k) {
        ans->dbl_v[i] = narm ? 0.0 : NA_REAL;
      } else {
        ans->dbl_v[i] = narm ? (double) w : NA_REAL;
      }
    }
  }
}
void frollsumExact(double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", window %d, hasna %d, narm %d\n"), "frollsumExact", (uint64_t)nx, k, hasna, (int)narm);
  for (int i=0; i<k-1; i++) {
    ans->dbl_v[i] = fill;
  }
  bool truehasna = hasna>0;
  if (!truehasna || !narm) {
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {
      if (narm && truehasna) {
        continue;
      }
      long double w = 0.0;
      for (int j=-k+1; j<=0; j++) {
        w += x[i+j];
      }
      if (R_FINITE((double) w)) {
        ans->dbl_v[i] = (double) w;
      } else {
        if (!narm) {
          ans->dbl_v[i] = (double) w;
        }
        truehasna = true;
      }
    }
    if (truehasna) {
      if (hasna==-1) {
        ans->status = 2;
        snprintf(end(ans->message[2]), 500, _("%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning"), __func__);
      }
      if (verbose) {
        if (narm) {
          snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n"), __func__);
        } else {
          snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run\n"), __func__);
        }
      }
    }
  }
  if (truehasna && narm) {
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {
      long double w = 0.0;
      int nc = 0;
      for (int j=-k+1; j<=0; j++) {
        if (ISNAN(x[i+j])) {
          nc++;
        } else {
          w += x[i+j];
        }
      }
      if (w > DBL_MAX) {
        ans->dbl_v[i] = R_PosInf;
      } else if (w < -DBL_MAX) {
        ans->dbl_v[i] = R_NegInf;
      } else {
        if (nc < k) {
          ans->dbl_v[i] = (double) w;
        } else {
          ans->dbl_v[i] = 0.0;
        }
      }
    }
  }
}

/* fast rolling any R function
 * not plain C, not thread safe
 * R eval() allocates
 */
void frollapply(double *x, int64_t nx, double *w, int k, ans_t *ans, int align, double fill, SEXP call, SEXP rho, bool verbose) {
  if (nx < k) {
    if (verbose)
      Rprintf(_("%s: window width longer than input vector, returning all NA vector\n"), __func__);
    for (int i=0; i<nx; i++) {
      ans->dbl_v[i] = fill;
    }
    return;
  }
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  for (int i=0; i<k-1; i++) {
    ans->dbl_v[i] = fill;
  }
  // this is i=k-1 iteration - first full window - taken out from the loop
  // we use it to add extra check that results of a FUN are length 1 numeric
  memcpy(w, x, k*sizeof(double));
  SEXP eval0 = PROTECT(eval(call, rho));
  if (xlength(eval0) != 1)
    error(_("%s: results from provided FUN are not length 1"), __func__);
  SEXPTYPE teval0 = TYPEOF(eval0);
  if (teval0 == REALSXP) {
    ans->dbl_v[k-1] = REAL(eval0)[0];
  } else {
    if (teval0==INTSXP || teval0==LGLSXP) {
      if (verbose)
        Rprintf(_("%s: results from provided FUN are not of type double, coercion from integer or logical will be applied on each iteration\n"), __func__);
      ans->dbl_v[k-1] = REAL(coerceVector(eval0, REALSXP))[0];
    } else {
      error(_("%s: results from provided FUN are not of type double"), __func__);
    }
  }
  UNPROTECT(1); // eval0
  // for each row it copies expected window data into w
  // evaluate call which has been prepared to point into w
  if (teval0 == REALSXP) {
    for (int64_t i=k; i<nx; i++) {
      memcpy(w, x+(i-k+1), k*sizeof(double));
      ans->dbl_v[i] = REAL(eval(call, rho))[0]; // this may fail with for a not type-stable fun
    }
  } else {
    for (int64_t i=k; i<nx; i++) {
      memcpy(w, x+(i-k+1), k*sizeof(double));
      SEXP evali = PROTECT(eval(call, rho));
      ans->dbl_v[i] = REAL(coerceVector(evali, REALSXP))[0];
      UNPROTECT(1); // evali
    }
  }
  if (ans->status < 3 && align < 1) {
    int k_ = align==-1 ? k-1 : floor(k/2);
    if (verbose)
      Rprintf(_("%s: align %d, shift answer by %d\n"), __func__, align, -k_);
    memmove((char *)ans->dbl_v, (char *)ans->dbl_v + (k_*sizeof(double)), (nx-k_)*sizeof(double));
    for (int64_t i=nx-k_; i<nx; i++) {
      ans->dbl_v[i] = fill;
    }
  }
  if (verbose)
    Rprintf(_("%s: took %.3fs\n"), __func__, omp_get_wtime()-tic);
}
