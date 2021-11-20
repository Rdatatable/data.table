#include "data.table.h"

/* fast adaptive rolling mean - router
 * algo = 0: fadaptiverollmeanFast
 *   first pass cumsum based solution, second pass uses cumsum to calculate answer
 * algo = 1: fadaptiverollmeanExact
 *   recalculate whole mean for each observation, roundoff correction is adjusted, also support for NaN and Inf
 */
void fadaptiverollmean(unsigned int algo, double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose) {
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  if (algo==0) {
    fadaptiverollmeanFast(x, nx, ans, k, fill, narm, hasna, verbose);
  } else if (algo==1) {
    fadaptiverollmeanExact(x, nx, ans, k, fill, narm, hasna, verbose);
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: processing algo %u took %.3fs\n"), __func__, algo, omp_get_wtime()-tic);
  // implicit n_message limit discussed here: https://github.com/Rdatatable/data.table/issues/3423#issuecomment-487722586
}
/* fast adaptive rolling mean - fast
 * when no info on NA (hasNA argument) then assume no NAs run faster version
 * adaptive rollmean implemented as cumsum first pass, then diff cumsum by indexes `i` to `i-k[i]`
 * if NAs detected re-run rollmean implemented as cumsum with NA support
 */
void fadaptiverollmeanFast(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", hasna %d, narm %d\n"), "fadaptiverollmeanFast", (uint64_t)nx, hasna, (int) narm);
  bool truehasna = hasna>0;                                     // flag to re-run if NAs detected
  long double w = 0.0;
  double *cs = malloc(nx*sizeof(double));                       // cumsum vector, same as double cs[nx] but no segfault
  if (!cs) {                                                    // # nocov start
    ans->status = 3;                                            // raise error
    snprintf(ans->message[3], 500, _("%s: Unable to allocate memory for cumsum"), __func__);
    free(cs);
    return;
  }                                                             // # nocov end
  if (!truehasna) {
    for (uint64_t i=0; i<nx; i++) {                             // loop on every observation to calculate cumsum only
      w += x[i];                                                // cumulate in long double
      cs[i] = (double) w;
    }
    if (R_FINITE((double) w)) {                                 // no need to calc this if NAs detected as will re-calc all below in truehasna==1
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=0; i<nx; i++) {                           // loop over observations to calculate final answer
        if (i+1 == k[i]) {
          ans->dbl_v[i] = cs[i]/k[i];                           // current obs window width exactly same as obs position in a vector
        } else if (i+1 > k[i]) {
          ans->dbl_v[i] = (cs[i]-cs[i-k[i]])/k[i];              // window width smaller than position so use cumsum to calculate diff
        } else {
          ans->dbl_v[i] = fill;                                 // position in a vector smaller than obs window width - partial window
        }
      }
    } else {                                                    // update truehasna flag if NAs detected
      if (hasna==-1) {                                          // raise warning
        ans->status = 2;
        snprintf(end(ans->message[2]), 500, _("%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning"), __func__);
      }
      if (verbose)
        snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n"), __func__);
      w = 0.0;
      truehasna = true;
    }
  }
  if (truehasna) {
    uint64_t nc = 0;                                            // running NA counter
    uint64_t *cn = malloc(nx*sizeof(uint64_t));                 // cumulative NA counter, used the same way as cumsum, same as uint64_t cn[nx] but no segfault
    if (!cn) {                                                  // # nocov start
      ans->status = 3;                                          // raise error
      snprintf(ans->message[3], 500, _("%s: Unable to allocate memory for cum NA counter"), __func__);
      free(cs);
      free(cn);
      return;
    }                                                           // # nocov end
    for (uint64_t i=0; i<nx; i++) {                             // loop over observations to calculate cumsum and cum NA counter
      if (R_FINITE(x[i])) {
        w += x[i];                                              // add observation to running sum
      } else {
        nc++;                                                   // increment non-finite counter
      }
      cs[i] = (double) w;                                       // cumsum, na.rm=TRUE always, NAs handled using cum NA counter
      cn[i] = nc;                                               // cum NA counter
    }
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {                             // loop over observations to calculate final answer
      if (i+1 < k[i]) {                                         // partial window
        ans->dbl_v[i] = fill;
      } else if (!narm) {                                       // this branch reduce number of branching in narm=1 below
        if (i+1 == k[i]) {
          ans->dbl_v[i] = cn[i]>0 ? NA_REAL : cs[i]/k[i];
        } else if (i+1 > k[i]) {
          ans->dbl_v[i] = (cn[i] - cn[i-k[i]])>0 ? NA_REAL : (cs[i]-cs[i-k[i]])/k[i];
        }
      } else if (i+1 == k[i]) {                                 // window width equal to observation position in vector
        int thisk = k[i] - ((int) cn[i]);                       // window width taking NAs into account, we assume single window width is int32, cum NA counter can be int64
        ans->dbl_v[i] = thisk==0 ? R_NaN : cs[i]/thisk;         // handle all obs NAs and na.rm=TRUE
      } else if (i+1 > k[i]) {                                  // window width smaller than observation position in vector
        int thisk = k[i] - ((int) (cn[i] - cn[i-k[i]]));        // window width taking NAs into account, we assume single window width is int32, cum NA counter can be int64
        ans->dbl_v[i] = thisk==0 ? R_NaN : (cs[i]-cs[i-k[i]])/thisk; // handle all obs NAs and na.rm=TRUE
      }
    }
    free(cn);
  } // end of truehasna
  free(cs);
}
/* fast adaptive rolling mean exact
 * extra nested loop to calculate mean of each obs and error correction
 * requires much more cpu
 * uses multiple cores
 */
void fadaptiverollmeanExact(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", hasna %d, narm %d\n"), "fadaptiverollmeanExact", (uint64_t)nx, hasna, (int) narm);
  bool truehasna = hasna>0;                                     // flag to re-run if NAs detected
  if (!truehasna || !narm) {                                    // narm=FALSE handled here as NAs properly propagated in exact algo
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {                             // loop on every observation to produce final answer
      if (narm && truehasna) {
        continue;                                               // if NAs detected no point to continue
      }
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill;                                   // position in a vector smaller than obs window width - partial window
      } else {
        long double w = 0.0;
        for (int j=-k[i]+1; j<=0; j++) {                        // sub-loop on window width
          w += x[i+j];                                          // sum of window for particular observation
        }
        if (R_FINITE((double) w)) {                             // no need to calc roundoff correction if NAs detected as will re-call all below in truehasna==1
          long double res = w / k[i];                           // keep results as long double for intermediate processing
          long double err = 0.0;                                // roundoff corrector
          for (int j=-k[i]+1; j<=0; j++) {                      // sub-loop on window width
            err += x[i+j] - res;                                // measure difference of obs in sub-loop to calculated fun for obs
          }
          ans->dbl_v[i] = (double) (res + (err / k[i]));        // adjust calculated fun with roundoff correction
        } else {
          if (!narm) {
            ans->dbl_v[i] = (double) (w / k[i]);                // NAs should be propagated
          }
          truehasna = true;                                     // NAs detected for this window, set flag so rest of windows will not be re-run
        }
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
    for (uint64_t i=0; i<nx; i++) {                             // loop over observations to produce final answer
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill;                                   // partial window
      } else {
        long double w = 0.0;                                    // window to accumulate values in particular window
        long double err = 0.0;                                  // accumulate roundoff error
        long double res;                                        // keep results as long double for intermediate processing
        int nc = 0;                                             // NA counter within current window
        for (int j=-k[i]+1; j<=0; j++) {                        // sub-loop on window width
          if (ISNAN(x[i+j])) {
            nc++;                                               // increment NA count in current window
          } else {
            w += x[i+j];                                        // add observation to current window
          }
        }
        if (w > DBL_MAX) {
          ans->dbl_v[i] = R_PosInf;                             // handle Inf for na.rm=TRUE consistently to base R
        } else if (w < -DBL_MAX) {
          ans->dbl_v[i] = R_NegInf;
        } else {
          if (nc == 0) {                                        // no NAs in current window
            res = w / k[i];
            for (int j=-k[i]+1; j<=0; j++) {                    // sub-loop on window width to accumulate roundoff error
              err += x[i+j] - res;                              // measure roundoff for each obs in window
            }
            ans->dbl_v[i] = (double) (res + (err / k[i]));      // adjust calculated fun with roundoff correction
          } else if (nc < k[i]) {
            res = w / (k[i]-nc);
            for (int j=-k[i]+1; j<=0; j++) {                    // sub-loop on window width to accumulate roundoff error
              if (!ISNAN(x[i+j])) {
                err += x[i+j] - res;                            // measure roundoff for each obs in window
              }
            }
            ans->dbl_v[i] = (double) (res + (err / (k[i] - nc))); // adjust calculated fun with roundoff correction
          } else {                                              // nc == k[i]
            ans->dbl_v[i] = R_NaN;                              // this branch assume narm so R_NaN always here
          }
        }
      }
    }
  } // end of truehasna
}

/* fast adaptive rolling sum */
void fadaptiverollsum(unsigned int algo, double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose) {
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  if (algo==0) {
    fadaptiverollsumFast(x, nx, ans, k, fill, narm, hasna, verbose);
  } else if (algo==1) {
    fadaptiverollsumExact(x, nx, ans, k, fill, narm, hasna, verbose);
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: processing algo %u took %.3fs\n"), __func__, algo, omp_get_wtime()-tic);
}
void fadaptiverollsumFast(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", hasna %d, narm %d\n"), "fadaptiverollsumFast", (uint64_t)nx, hasna, (int) narm);
  bool truehasna = hasna>0;
  long double w = 0.0;
  double *cs = malloc(nx*sizeof(double));
  if (!cs) {                                                    // # nocov start
    ans->status = 3;
    snprintf(ans->message[3], 500, _("%s: Unable to allocate memory for cumsum"), __func__);
    free(cs);
    return;
  }                                                             // # nocov end
  if (!truehasna) {
    for (uint64_t i=0; i<nx; i++) {
      w += x[i];
      cs[i] = (double) w;
    }
    if (R_FINITE((double) w)) {
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=0; i<nx; i++) {
        if (i+1 == k[i]) {
          ans->dbl_v[i] = cs[i];
        } else if (i+1 > k[i]) {
          ans->dbl_v[i] = cs[i]-cs[i-k[i]];
        } else {
          ans->dbl_v[i] = fill;
        }
      }
    } else {
      if (hasna==-1) {
        ans->status = 2;
        snprintf(end(ans->message[2]), 500, _("%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning"), __func__);
      }
      if (verbose)
        snprintf(end(ans->message[0]), 500, _("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n"), __func__);
      w = 0.0;
      truehasna = true;
    }
  }
  if (truehasna) {
    uint64_t nc = 0;
    uint64_t *cn = malloc(nx*sizeof(uint64_t));
    if (!cn) {                                                  // # nocov start
      ans->status = 3;
      snprintf(ans->message[3], 500, _("%s: Unable to allocate memory for cum NA counter"), __func__);
      free(cs);
      free(cn);
      return;
    }                                                           // # nocov end
    for (uint64_t i=0; i<nx; i++) {
      if (R_FINITE(x[i])) {
        w += x[i];
      } else {
        nc++;
      }
      cs[i] = (double) w;
      cn[i] = nc;
    }
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill;
      } else if (!narm) {
        if (i+1 == k[i]) {
          ans->dbl_v[i] = cn[i]>0 ? NA_REAL : cs[i];
        } else if (i+1 > k[i]) {
          ans->dbl_v[i] = (cn[i] - cn[i-k[i]])>0 ? NA_REAL : cs[i]-cs[i-k[i]];
        }
      } else if (i+1 == k[i]) {
        int thisk = k[i] - ((int) cn[i]);
        ans->dbl_v[i] = thisk==0 ? 0.0 : cs[i];
      } else if (i+1 > k[i]) {
        int thisk = k[i] - ((int) (cn[i] - cn[i-k[i]]));
        ans->dbl_v[i] = thisk==0 ? 0.0 : cs[i]-cs[i-k[i]];
      }
    }
    free(cn);
  }
  free(cs);
}
void fadaptiverollsumExact(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", hasna %d, narm %d\n"), "fadaptiverollsumExact", (uint64_t)nx, hasna, (int) narm);
  bool truehasna = hasna>0;
  if (!truehasna || !narm) {
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {
      if (narm && truehasna) {
        continue;
      }
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill;
      } else {
        long double w = 0.0;
        for (int j=-k[i]+1; j<=0; j++) {
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
    for (uint64_t i=0; i<nx; i++) {
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill;
      } else {
        long double w = 0.0;
        int nc = 0;
        for (int j=-k[i]+1; j<=0; j++) {
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
          if (nc < k[i]) {
            ans->dbl_v[i] = (double) w;
          } else {
            ans->dbl_v[i] = 0.0;
          }
        }
      }
    }
  }
}
