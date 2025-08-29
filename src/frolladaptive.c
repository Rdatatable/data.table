#include "data.table.h"

/* rolling adaptive fun - router for fun and algo
 * rfun enum rollfun_t routes to rolling function
 * algo = 0: fast
 *   adding/removing in/out of sliding window of observations
 * algo = 1: exact
 *   recalculate whole fun for each observation, for mean roundoff correction is adjusted
 */
void frolladaptivefun(rollfun_t rfun, unsigned int algo, double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasnf, bool verbose) {
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  switch (rfun) {
  case MEAN :
    if (algo==0) {
      frolladaptivemeanFast(x, nx, ans, k, fill, narm, hasnf, verbose);
    } else if (algo==1) {
      frolladaptivemeanExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  case SUM :
    if (algo==0) {
      frolladaptivesumFast(x, nx, ans, k, fill, narm, hasnf, verbose);
    } else if (algo==1) {
      frolladaptivesumExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  case MAX :
    if (algo==0 && verbose) {
      //frolladaptivemaxFast(x, nx, ans, k, fill, narm, hasnf, verbose); // frolladaptivemaxFast does not exists as of now
      snprintf(end(ans->message[0]), 500, _("%s: algo %u not implemented, fall back to %u\n"), __func__, algo, (unsigned int) 1);
    }
    frolladaptivemaxExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    break;
  default: // #nocov
    error(_("Internal error: Unknown rfun value in froll: %d"), rfun); // #nocov
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: processing fun %d algo %u took %.3fs\n"), __func__, rfun, algo, omp_get_wtime()-tic);
}

/* fast rolling adaptive mean - fast
 * when no info on NF (has.nf argument) then assume no NFs run faster version
 * adaptive rollmean implemented as cumsum first pass, then diff cumsum by indexes `i` to `i-k[i]`
 * if NFs detected re-run rollmean implemented as cumsum with NF support
 */
void frolladaptivemeanFast(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", hasnf %d, narm %d\n"), "frolladaptivemeanFast", (uint64_t)nx, hasnf, (int) narm);
  bool truehasnf = hasnf>0;                                     // flag to re-run if NAs detected
  long double w = 0.0;
  double *cs = malloc(sizeof(*cs) * nx);                       // cumsum vector, same as double cs[nx] but no segfault
  if (!cs) {                                                    // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for cumsum", __func__); // raise error
    return;
  }                                                             // # nocov end
  if (!truehasnf) {
    for (uint64_t i=0; i<nx; i++) {                             // loop on every observation to calculate cumsum only
      w += x[i];                                                // cumulate in long double
      cs[i] = (double) w;
    }
    if (R_FINITE((double) w)) {                                 // no need to calc this if NAs detected as will re-calc all below in truehasnf==1
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
    } else {                                                    // update truehasnf flag if NAs detected
      if (hasnf==-1)
        ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
      if (verbose)
        ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
      w = 0.0; truehasnf = true;
    }
  }
  if (truehasnf) {
    uint64_t nc = 0, pinf = 0, ninf = 0;                        // running NA counter
    uint64_t *cn = malloc(nx*sizeof(*cn));                      // cumulative NA counter, used the same way as cumsum, same as uint64_t cn[nx] but no segfault
    if (!cn) {                                                  // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum NA counter", __func__); // raise error
      free(cs);
      return;
    }                                                           // # nocov end
    uint64_t *cpinf = malloc(nx*sizeof(*cpinf));
    if (!cpinf) {                                               // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum Inf counter", __func__); // raise error
      free(cs); free(cn);
      return;
    }                                                           // # nocov end
    uint64_t *cninf = malloc(nx*sizeof(*cninf));
    if (!cninf) {                                               // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum -Inf counter", __func__); // raise error
      free(cs); free(cn); free(cpinf);
      return;
    }                                           // # nocov end
    for (uint64_t i=0; i<nx; i++) {                             // loop over observations to calculate cumsum and cum NA counter
      if (R_FINITE(x[i])) {
        w += x[i];                                              // add observation to running sum
      } else if (ISNAN(x[i])) {
        nc++;                                                   // increment non-finite counter
      } else if (x[i]==R_PosInf) {
        pinf++;
      } else if (x[i]==R_NegInf) {
        ninf++;
      }
      cs[i] = (double) w;                                       // cumsum, na.rm=TRUE always, NAs handled using cum NA counter
      cn[i] = nc;                                               // cum NA counter
      cpinf[i] = pinf;
      cninf[i] = ninf;
    }

#undef MEAN_WINDOW_STEP_VALUE
#define MEAN_WINDOW_STEP_VALUE                                     \
    if (wn>0) {                                                    \
      if (narm) {                                                  \
        if (wpinf > 0) {                                           \
          if (wninf > 0) {                                         \
            ans->dbl_v[i] = R_NaN;                                 \
          } else {                                                 \
            ans->dbl_v[i] = R_PosInf;                              \
          }                                                        \
        } else if (wninf > 0) {                                    \
          ans->dbl_v[i] = R_NegInf;                                \
        } else {                                                   \
          int thisk = k[i] - ((int) wn);                           \
          ans->dbl_v[i] = thisk==0 ? R_NaN : ws/thisk;             \
        }                                                          \
      } else {                                                     \
        ans->dbl_v[i] = NA_REAL;                                   \
      }                                                            \
    } else {                                                       \
      if (wpinf > 0) {                                             \
        if (wninf > 0) {                                           \
          ans->dbl_v[i] = R_NaN;                                   \
        } else {                                                   \
          ans->dbl_v[i] = R_PosInf;                                \
        }                                                          \
      } else if (wninf > 0) {                                      \
        ans->dbl_v[i] = R_NegInf;                                  \
      } else {                                                     \
        ans->dbl_v[i] = ws/k[i];                                   \
      }                                                            \
    }

    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {                             // loop over observations to calculate final answer
      uint64_t wn, wpinf, wninf;
      double ws;
      if (i+1 < k[i]) {                                         // partial window
        ans->dbl_v[i] = fill;
      } else if (i+1 == k[i]) {                                 // first full window
        wn = cn[i];
        wpinf = cpinf[i];
        wninf = cninf[i];
        ws = cs[i];
        MEAN_WINDOW_STEP_VALUE
      } else {                                                  // all the remaining full windows
        wn = cn[i] - cn[i-k[i]];                                // NAs in current window
        wpinf = cpinf[i] - cpinf[i-k[i]];                       // Inf in current window
        wninf = cninf[i] - cninf[i-k[i]];                       // -Inf in current window
        ws = cs[i] - cs[i-k[i]];                                // cumsum in current window
        MEAN_WINDOW_STEP_VALUE
      }
    }
    free(cninf); free(cpinf); free(cn);
  } // end of truehasnf
  free(cs);
}
/* fast rolling adaptive mean exact
 * extra nested loop to calculate mean of each obs and error correction
 * requires much more cpu
 * uses multiple cores
 */
void frolladaptivemeanExact(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", hasnf %d, narm %d\n"), "frolladaptivemeanExact", (uint64_t)nx, hasnf, (int) narm);
  bool truehasnf = hasnf>0;                                     // flag to re-run if NAs detected
  if (!truehasnf || !narm) {                                    // narm=FALSE handled here as NAs properly propagated in exact algo
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {                             // loop on every observation to produce final answer
      if (narm && truehasnf) {
        continue;                                               // if NAs detected no point to continue
      }
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill;                                   // position in a vector smaller than obs window width - partial window
      } else {
        long double w = 0.0;
        for (int j=-k[i]+1; j<=0; j++) {                        // sub-loop on window width
          w += x[i+j];                                          // sum of window for particular observation
        }
        if (R_FINITE((double) w)) {                             // no need to calc roundoff correction if NAs detected as will re-call all below in truehasnf==1
          long double res = w / k[i];                           // keep results as long double for intermediate processing
          long double err = 0.0;                                // roundoff corrector
          for (int j=-k[i]+1; j<=0; j++) {                      // sub-loop on window width
            err += x[i+j] - res;                                // measure difference of obs in sub-loop to calculated fun for obs
          }
          ans->dbl_v[i] = (double) (res + (err / k[i]));        // adjust calculated fun with roundoff correction
        } else if (ISNAN((double) w)) {
          if (!narm) {
            ans->dbl_v[i] = (double) w;
          }
          truehasnf = true;                                     // NAs detected for this window, set flag so rest of windows will not be re-run
        } else {
          ans->dbl_v[i] = (double) w;                           // Inf and -Inf
        }
      }
    }
    if (truehasnf) {
      if (hasnf==-1)
        ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
      if (verbose) {
        if (narm)
          ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
        else
          ansSetMsg(ans, 0, "%s: non-finite values are present in input, na.rm=FALSE and algo='exact' propagates NFs properply, no need to re-run\n", __func__);
      }
    }
  }
  if (truehasnf && narm) {
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
        if (R_FINITE((double) w)) {
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
        } else {
          ans->dbl_v[i] = (double) w;
        }
      }
    }
  } // end of truehasnf
}

/* fast rolling adaptive sum - fast
 * same as adaptive mean fast
 */
void frolladaptivesumFast(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", hasnf %d, narm %d\n"), "frolladaptivesumFast", (uint64_t)nx, hasnf, (int) narm);
  bool truehasnf = hasnf>0;
  long double w = 0.0;
  double *cs = malloc(sizeof(*cs) * nx);
  if (!cs) {                                                    // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for cumsum", __func__); // raise error
    return;
  }                                                             // # nocov end
  if (!truehasnf) {
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
      if (hasnf==-1)
        ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
      if (verbose)
        ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
      w = 0.0; truehasnf = true;
    }
  }
  if (truehasnf) {
    uint64_t nc = 0, pinf = 0, ninf = 0;                        // running NA counter
    uint64_t *cn = malloc(nx*sizeof(*cn));                 // cumulative NA counter, used the same way as cumsum, same as uint64_t cn[nx] but no segfault
    if (!cn) {                                                  // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum NA counter", __func__); // raise error
      free(cs);
      return;
    }                                                           // # nocov end
    uint64_t *cpinf = malloc(nx*sizeof(*cpinf));
    if (!cpinf) {                                               // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum Inf counter", __func__); // raise error
      free(cs); free(cn);
      return;
    }                                                           // # nocov end
    uint64_t *cninf = malloc(nx*sizeof(*cninf));
    if (!cninf) {                                               // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum -Inf counter", __func__); // raise error
      free(cs); free(cn); free(cpinf);
      return;
    }                                                           // # nocov end
    for (uint64_t i=0; i<nx; i++) {                             // loop over observations to calculate cumsum and cum NA counter
      if (R_FINITE(x[i])) {
        w += x[i];                                              // add observation to running sum
      } else if (ISNAN(x[i])) {
        nc++;                                                   // increment non-finite counter
      } else if (x[i]==R_PosInf) {
        pinf++;
      } else if (x[i]==R_NegInf) {
        ninf++;
      }
      cs[i] = (double) w;                                       // cumsum, na.rm=TRUE always, NAs handled using cum NA counter
      cn[i] = nc;                                               // cum NA counter
      cpinf[i] = pinf;
      cninf[i] = ninf;
    }

#undef SUM_WINDOW_STEP_VALUE
#define SUM_WINDOW_STEP_VALUE                                      \
    if (wn>0) {                                                    \
      if (narm) {                                                  \
        if (wpinf > 0) {                                           \
          if (wninf > 0) {                                         \
            ans->dbl_v[i] = R_NaN;                                 \
          } else {                                                 \
            ans->dbl_v[i] = R_PosInf;                              \
          }                                                        \
        } else if (wninf > 0) {                                    \
          ans->dbl_v[i] = R_NegInf;                                \
        } else {                                                   \
          int thisk = k[i] - ((int) wn);                           \
          ans->dbl_v[i] = thisk==0 ? 0.0 : ws;                     \
        }                                                          \
      } else {                                                     \
        ans->dbl_v[i] = NA_REAL;                                   \
      }                                                            \
    } else {                                                       \
      if (wpinf > 0) {                                             \
        if (wninf > 0) {                                           \
          ans->dbl_v[i] = R_NaN;                                   \
        } else {                                                   \
          ans->dbl_v[i] = R_PosInf;                                \
        }                                                          \
      } else if (wninf > 0) {                                      \
        ans->dbl_v[i] = R_NegInf;                                  \
      } else {                                                     \
        ans->dbl_v[i] = ws;                                        \
      }                                                            \
    }

    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {
      uint64_t wn, wpinf, wninf;
      double ws;
      if (i+1 < k[i]) {                                         // partial window
        ans->dbl_v[i] = fill;
      } else if (i+1 == k[i]) {                                 // first full window
        wn = cn[i];
        wpinf = cpinf[i];
        wninf = cninf[i];
        ws = cs[i];
        SUM_WINDOW_STEP_VALUE
      } else {                                                  // all the remaining full windows
        wn = cn[i] - cn[i-k[i]];                                // NAs in current window
        wpinf = cpinf[i] - cpinf[i-k[i]];                       // Inf in current window
        wninf = cninf[i] - cninf[i-k[i]];                       // -Inf in current window
        ws = cs[i] - cs[i-k[i]];                                // cumsum in current window
        SUM_WINDOW_STEP_VALUE
      }
    }
    free(cninf); free(cpinf); free(cn);
  }
  free(cs);
}
/* fast rolling adaptive sum - exact
 * same as adaptive mean exact
 */
void frolladaptivesumExact(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", hasnf %d, narm %d\n"), "frolladaptivesumExact", (uint64_t)nx, hasnf, (int) narm);
  bool truehasnf = hasnf>0;
  if (!truehasnf || !narm) {
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {
      if (narm && truehasnf) {
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
        } else if (ISNAN((double) w)) {
          if (!narm) {
            ans->dbl_v[i] = (double) w;
          }
          truehasnf = true;                                     // NAs detected for this window, set flag so rest of windows will not be re-run
        } else {
          ans->dbl_v[i] = (double) w;                           // Inf and -Inf
        }
      }
    }
    if (truehasnf) {
      if (hasnf==-1)
        ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
      if (verbose) {
        if (narm)
          ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
        else
          ansSetMsg(ans, 0, "%s: non-finite values are present in input, na.rm=FALSE and algo='exact' propagates NFs properply, no need to re-run\n", __func__);
      }
    }
  }
  if (truehasnf && narm) {
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

/* fast rolling adaptive max - exact
 * for has.nf=FALSE it will not detect if any NAs were in the input, therefore could produce incorrect result, well documented
 */
void frolladaptivemaxExact(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", hasnf %d, narm %d\n"), "frolladaptivemaxExact", (uint64_t)nx, hasnf, (int) narm);
  if (narm || hasnf==-1) { // fastest we can get for adaptive max as there is no algo='fast', therefore we drop any NA checks when has.nf=FALSE
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill;
      } else {
        double w = R_NegInf;
        for (int j=-k[i]+1; j<=0; j++) {
          if (x[i+j] > w)
            w = x[i+j];
        }
        ans->dbl_v[i] = w;
      }
    }
  } else {
    bool *isnan = malloc(nx*sizeof(*isnan)); // isnan lookup - we use it to reduce ISNAN calls in nested loop
    if (!isnan) {                                                    // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for isnan", __func__); // raise error
      return;
    }                                                               // # nocov end
    bool truehasnf = hasnf>0;
    for (uint64_t i=0; i<nx; i++) { // no openmp as this is very fast
      isnan[i] = ISNAN(x[i]);
      truehasnf |= isnan[i];
    }
    if (!truehasnf) { // not found any NAs
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=0; i<nx; i++) {
        if (i+1 < k[i]) {
          ans->dbl_v[i] = fill;
        } else {
          double w = R_NegInf;
          for (int j=-k[i]+1; j<=0; j++) {
            if (x[i+j] > w)
              w = x[i+j];
          }
          ans->dbl_v[i] = w;
        }
      }
    } else { // there are some NAs
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=0; i<nx; i++) {
        if (i+1 < k[i]) {
          ans->dbl_v[i] = fill;
        } else {
          double w = R_NegInf;
          if (isnan[i] && ISNA(x[i])) {
            w = NA_REAL;
          } else {
            for (int j=-k[i]+1; j<=0; j++) {
              if (isnan[i+j]) {
                if (ISNA(x[i+j])) {
                  w = NA_REAL;
                  break;
                } else {
                  w = R_NaN;
                }
              } else if (x[i+j] > w)
                w = x[i+j];
            }
          }
          ans->dbl_v[i] = w;
        }
      }
    }
  }
}

/* fast rolling adaptive any R function
 * not plain C, not thread safe
 * R eval() allocates
 * takes SEXP because it has SETLENGTH for each window
 */
void frolladaptiveapply(double *x, int64_t nx, SEXP pw, int *k, ans_t *ans, double fill, SEXP call, SEXP rho, bool verbose) {
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();

  double *w = REAL(pw);
  // this is i=k[0]-1 iteration - first full window - taken out from the loop
  // we use it to add extra check that results of a FUN are length 1 numeric
  SEXPTYPE teval0;
  uint64_t i; // #loop_counter_not_local_scope_ok
  for (i=0; i<nx; i++) { // this won't go to nx as there is break
    if (i+1 < k[i]) {
      ans->dbl_v[i] = fill;
    } else {
      SETLENGTH(pw, k[i]);
      memcpy(w, x+(i-k[i]+1), k[i]*sizeof(double));
      SEXP eval0 = PROTECT(eval(call, rho));
      if (xlength(eval0) != 1)
        error(_("%s: results from provided FUN are not length 1"), __func__);
      teval0 = TYPEOF(eval0);
      if (teval0 == REALSXP) {
        ans->dbl_v[i] = REAL(eval0)[0];
      } else {
        if (teval0==INTSXP || teval0==LGLSXP) {
          if (verbose)
            Rprintf(_("%s: results from provided FUN are not of type double, coercion from integer or logical will be applied on each iteration\n"), __func__);
          ans->dbl_v[i] = REAL(coerceVector(eval0, REALSXP))[0];
        } else {
          error(_("%s: results from provided FUN are not of type double"), __func__);
        }
      }
      UNPROTECT(1); // eval0
      break;
    }
  }
  if (i==nx) { // none of the windows in k was small enough to cover length of x
    return;
  }
  // for each row it sets length of current window because it is adaptive version
  // then copies expected window data into w
  // evaluate call which has been prepared to point into w
  if (teval0 == REALSXP) {
    for (; i<nx; i++) {
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill; // #nocov // this is never reached because smaller i are handled above, leaving it here because this function will be removed in next PR, and adapting it here will only make git conflicts resolution more difficult
      } else {
        SETLENGTH(pw, k[i]);
        memcpy(w, x+(i-k[i]+1), k[i]*sizeof(double));
        ans->dbl_v[i] = REAL(eval(call, rho))[0]; // this may fail with for a not type-stable fun
      }
    }
  } else {
    for (; i<nx; i++) {
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill;
      } else {
        SETLENGTH(pw, k[i]);
        memcpy(w, x+(i-k[i]+1), k[i]*sizeof(double));
        SEXP evali = PROTECT(eval(call, rho));
        ans->dbl_v[i] = REAL(coerceVector(evali, REALSXP))[0];
        UNPROTECT(1); // evali
      }
    }
  }
  if (verbose)
    Rprintf(_("%s: took %.3fs\n"), __func__, omp_get_wtime()-tic);
}
