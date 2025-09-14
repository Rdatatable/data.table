#include "data.table.h"

/* rolling adaptive fun - router for fun and algo
 * rfun enum rollfun_t routes to rolling function
 * algo = 0: fast
 *   adding/removing in/out of sliding window of observations
 * algo = 1: exact
 *   recalculate whole fun for each observation, for mean roundoff correction is adjusted
 */
void frolladaptivefun(rollfun_t rfun, unsigned int algo, const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
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
  case MIN :
    if (algo==0 && verbose) {
      //frolladaptiveminFast(x, nx, ans, k, fill, narm, hasnf, verbose); // frolladaptiveminFast does not exists as of now
      snprintf(end(ans->message[0]), 500, _("%s: algo %u not implemented, fall back to %u\n"), __func__, algo, (unsigned int) 1);
    }
    frolladaptiveminExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    break;
  case PROD :
    if (algo==0) {
      frolladaptiveprodFast(x, nx, ans, k, fill, narm, hasnf, verbose);
    } else if (algo==1) {
      frolladaptiveprodExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  case MEDIAN :
    if (algo==0 && verbose) {
      //frolladaptivemedianFast(x, nx, ans, k, fill, narm, hasnf, verbose); // frolladaptivemedianFast does not exists as of now
      snprintf(end(ans->message[0]), 500, _("%s: algo %u not implemented, fall back to %u\n"), __func__, algo, (unsigned int) 1);
    }
    frolladaptivemedianExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    break;
  default: // # nocov
    internal_error(__func__, "Unknown rfun value in frolladaptive: %d", rfun); // # nocov
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: processing fun %d algo %u took %.3fs\n"), __func__, rfun, algo, omp_get_wtime()-tic);
}

#undef MEAN_WINDOW_STEP_VALUE
#define MEAN_WINDOW_STEP_VALUE                                   \
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
      ans->dbl_v[i] = ws/k[i]; /* for k[i]==0 it turns into 0/0, as expected */ \
    }                                                            \
  }

/* fast rolling adaptive mean - fast
 * when no info on NF (has.nf argument) then assume no NFs run faster version
 * adaptive rollmean implemented as cumsum first pass, then diff cumsum by indexes `i` to `i-k[i]`
 * if NFs detected re-run rollmean implemented as cumsum with NF support
 */
void frolladaptivemeanFast(const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
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
          ans->dbl_v[i] = (cs[i]-cs[i-k[i]])/k[i];              // window width smaller than position so use cumsum to calculate diff, for k[i]==0 it turns into 0/0, as expected
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
    uint64_t *cn = malloc(sizeof(*cn) * nx);                    // cumulative NA counter, used the same way as cumsum, same as uint64_t cn[nx] but no segfault
    if (!cn) {                                                  // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum NA counter", __func__); // raise error
      free(cs);
      return;
    }                                                           // # nocov end
    uint64_t *cpinf = malloc(sizeof(*cpinf) * nx);
    if (!cpinf) {                                               // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum Inf counter", __func__); // raise error
      free(cs); free(cn);
      return;
    }                                                           // # nocov end
    uint64_t *cninf = malloc(sizeof(*cninf) * nx);
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
/* fast rolling adaptive mean - exact
 * extra nested loop to calculate mean of each obs and error correction
 * requires much more cpu
 * uses multiple cores
 */
void frolladaptivemeanExact(const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
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
          ans->dbl_v[i] = (double) (res + (err / k[i]));        // adjust calculated fun with roundoff correction, for k[i]==0 it turns into NaN + 0/0, as expected
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
            ans->dbl_v[i] = (double) (res + (err / k[i]));      // adjust calculated fun with roundoff correctionfor k[i]==0 it turns into NaN + 0/0, as expected
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

#undef SUM_WINDOW_STEP_VALUE
#define SUM_WINDOW_STEP_VALUE                                    \
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
      ans->dbl_v[i] = ws; /* for k[i]==0 it turns into 0, as expected */ \
    }                                                            \
  }

/* fast rolling adaptive sum - fast
 * same as adaptive mean fast
 */
void frolladaptivesumFast(const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
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
          ans->dbl_v[i] = cs[i]-cs[i-k[i]]; // for k[i]==0 it turns into 0, as expected
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
    uint64_t *cn = malloc(sizeof(*cn) * nx);                    // cumulative NA counter, used the same way as cumsum, same as uint64_t cn[nx] but no segfault
    if (!cn) {                                                  // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum NA counter", __func__); // raise error
      free(cs);
      return;
    }                                                           // # nocov end
    uint64_t *cpinf = malloc(sizeof(*cpinf) * nx);
    if (!cpinf) {                                               // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum Inf counter", __func__); // raise error
      free(cs); free(cn);
      return;
    }                                                           // # nocov end
    uint64_t *cninf = malloc(sizeof(*cninf) * nx);
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
void frolladaptivesumExact(const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
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
          ans->dbl_v[i] = (double) w; // also k[i]==0 then w=0
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
            ans->dbl_v[i] = 0.0; // also k[i]==0
          }
        }
      }
    }
  }
}

/* fast rolling adaptive max - exact
 * for has.nf=FALSE it will not detect if any NAs were in the input, therefore could produce incorrect result, well documented
 */
void frolladaptivemaxExact(const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
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
        ans->dbl_v[i] = w; // also k[i]==0 then -Inf
      }
    }
  } else {
    bool *isnan = malloc(sizeof(*isnan) * nx); // isnan lookup - we use it to reduce ISNAN calls in nested loop
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
          ans->dbl_v[i] = w; // also k[i]==0 then -Inf
        }
      }
    } else { // there are some NAs
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=0; i<nx; i++) {
        if (i+1 < k[i]) {
          ans->dbl_v[i] = fill;
        } else {
          double w = R_NegInf;
          if (k[i] && isnan[i] && ISNA(x[i])) { // consider branch only when NOT k[i]==0
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
    free(isnan);
  }
}

/* fast rolling adaptive min - exact
 * for has.nf=FALSE it will not detect if any NAs were in the input, therefore could produce incorrect result, well documented
 */
void frolladaptiveminExact(const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", hasnf %d, narm %d\n"), "frolladaptiveminExact", (uint64_t)nx, hasnf, (int) narm);
  if (narm || hasnf==-1) { // fastest we can get for adaptive max as there is no algo='fast', therefore we drop any NA checks when has.nf=FALSE
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=0; i<nx; i++) {
      if (i+1 < k[i]) {
        ans->dbl_v[i] = fill;
      } else {
        double w = R_PosInf;
        for (int j=-k[i]+1; j<=0; j++) {
          if (x[i+j] < w)
            w = x[i+j];
        }
        ans->dbl_v[i] = w; // also k[i]==0 then +Inf
      }
    }
  } else {
    bool *isnan = malloc(sizeof(*isnan) * nx); // isnan lookup - we use it to reduce ISNAN calls in nested loop
    if (!isnan) {                                                    // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for isnan", __func__); // raise error
      return;
    }                                                               // # nocov end
    bool truehasnf = hasnf>0;
    for (uint64_t i=0; i<nx; i++) { // no openmp as this should be very fast
      if (ISNAN(x[i])) {
        truehasnf = true;
        isnan[i] = true;
      } else {
        isnan[i] = false;
      }
    }
    if (!truehasnf) { // not found any NAs
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=0; i<nx; i++) {
        if (i+1 < k[i]) {
          ans->dbl_v[i] = fill;
        } else {
          double w = R_PosInf;
          for (int j=-k[i]+1; j<=0; j++) {
            if (x[i+j] < w)
              w = x[i+j];
          }
          ans->dbl_v[i] = w; // also k[i]==0 then +Inf
        }
      }
    } else { // there are some NAs
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=0; i<nx; i++) {
        if (i+1 < k[i]) {
          ans->dbl_v[i] = fill;
        } else {
          double w = R_PosInf;
          if (k[i] && isnan[i] && ISNA(x[i])) { // consider branch only when NOT k[i]==0
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
              } else if (x[i+j] < w)
                w = x[i+j];
            }
          }
          ans->dbl_v[i] = w; // also k[i]==0 then +Inf
        }
      }
    }
    free(isnan);
  }
}

#undef PROD_WINDOW_STEP_VALUE
#define PROD_WINDOW_STEP_VALUE                                   \
  if (wn>0) {                                                    \
    if (narm) {                                                  \
      if (wpinf == 0 && wninf == 0) {                            \
        int thisk = k[i] - ((int) wn);                           \
        ans->dbl_v[i] = thisk==0 ? 1.0 : (double) ws;            \
      } else {                                                   \
        ans->dbl_v[i] = (wninf+(ws<0))%2 ? R_NegInf : R_PosInf;  \
      }                                                          \
    } else {                                                     \
      ans->dbl_v[i] = NA_REAL;                                   \
    }                                                            \
  } else {                                                       \
    if (wpinf == 0 && wninf == 0) {                              \
      ans->dbl_v[i] = (double) ws; /* k[i]==0 produces 1.0 */    \
    } else {                                                     \
      ans->dbl_v[i] = (wninf+(ws<0))%2 ? R_NegInf : R_PosInf;    \
    }                                                            \
  }

/* fast rolling adaptive prod - fast
 * same as adaptive mean fast
 */
void frolladaptiveprodFast(const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", hasnf %d, narm %d\n"), "frolladaptiveprodFast", (uint64_t)nx, hasnf, (int) narm);
  bool truehasnf = hasnf>0;
  long double w = 1.0;
  double *cs = malloc(sizeof(*cs) * nx);
  if (!cs) {                                                    // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for cumprod", __func__); // raise error
    return;
  }                                                             // # nocov end
  if (!truehasnf) {
    for (uint64_t i=0; i<nx; i++) {
      w *= x[i];
      cs[i] = (double) w;
    }
    if (R_FINITE((double) w)) {
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=0; i<nx; i++) {
        if (i+1 == k[i]) {
          ans->dbl_v[i] = cs[i];
        } else if (i+1 > k[i]) {
          ans->dbl_v[i] = cs[i]/cs[i-k[i]]; // for k[i]==0 it turns into cs[i]/cs[i] = 1, as expected
        } else {
          ans->dbl_v[i] = fill;
        }
      }
    } else {
      if (hasnf==-1)
        ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
      if (verbose)
        ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
      w = 1.0; truehasnf = true;
    }
  }
  if (truehasnf) {
    uint64_t nc = 0, pinf = 0, ninf = 0;                        // running NA counter
    uint64_t *cn = malloc(sizeof(*cn) * nx);                 // cumulative NA counter, used the same way as cumprod, same as uint64_t cn[nx] but no segfault
    if (!cn) {                                                  // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum NA counter", __func__); // raise error
      free(cs);
      return;
    }                                                           // # nocov end
    uint64_t *cpinf = malloc(sizeof(*cpinf) * nx);
    if (!cpinf) {                                               // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum Inf counter", __func__); // raise error
      free(cs); free(cn);
      return;
    }                                                           // # nocov end
    uint64_t *cninf = malloc(sizeof(*cninf) * nx);
    if (!cninf) {                                               // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for cum -Inf counter", __func__); // raise error
      free(cs); free(cn); free(cpinf);
      return;
    }                                                           // # nocov end
    for (uint64_t i=0; i<nx; i++) {                             // loop over observations to calculate cumprod and cum NA counter
      if (R_FINITE(x[i])) {
        w *= x[i];                                              // add observation to running prod
      } else if (ISNAN(x[i])) {
        nc++;                                                   // increment non-finite counter
      } else if (x[i]==R_PosInf) {
        pinf++;
      } else if (x[i]==R_NegInf) {
        ninf++;
      }
      cs[i] = (double) w;                                       // cumprod, na.rm=TRUE always, NAs handled using cum NA counter
      cn[i] = nc;                                               // cum NA counter
      cpinf[i] = pinf;
      cninf[i] = ninf;
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
        PROD_WINDOW_STEP_VALUE
      } else {                                                  // all the remaining full windows
        wn = cn[i] - cn[i-k[i]];                                // NAs in current window
        wpinf = cpinf[i] - cpinf[i-k[i]];                       // Inf in current window
        wninf = cninf[i] - cninf[i-k[i]];                       // -Inf in current window
        ws = cs[i] / cs[i-k[i]];                                // cumprod in current window
        PROD_WINDOW_STEP_VALUE
      }
    }
    free(cninf); free(cpinf); free(cn);
  }
  free(cs);
}
/* fast rolling adaptive prod - exact
 * same as adaptive mean exact
 */
void frolladaptiveprodExact(const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", hasnf %d, narm %d\n"), "frolladaptiveprodExact", (uint64_t)nx, hasnf, (int) narm);
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
        long double w = 1.0;
        for (int j=-k[i]+1; j<=0; j++) {
          w *= x[i+j];
        }
        if (R_FINITE((double) w)) {
          ans->dbl_v[i] = (double) w; // k[i]==0 fills with 1.0
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
        long double w = 1.0;
        int nc = 0;
        for (int j=-k[i]+1; j<=0; j++) {
          if (ISNAN(x[i+j])) {
            nc++;
          } else {
            w *= x[i+j];
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
            ans->dbl_v[i] = 1.0; // nc==0, k[i]==0 fills with 1.0
          }
        }
      }
    }
  }
}

/* fast rolling adaptive NA stats - fast
 * online implementation
 * returns total number of NA/NaN in x
 * no nested loops, first loop cum NA count, second loop uses cum NA count to get rolling count
 * updates:
 *   nc for rolling NA count
 *   isna for NA mask
 */
static int frolladaptiveNAFast(const double *x, uint64_t nx, const int *k, int *nc, bool *isna) {
  int cnc = 0; // cumulative na count
  for (uint64_t i=0; i<nx; i++) {
    if (ISNAN(x[i])) {
      isna[i] = true;
      cnc++;
    }
    nc[i] = cnc;
  }
  if (cnc) {
    for (int64_t i=nx-1; i>=0; i--) {
      int ik = k[i];
      if (i+1 == ik) {
        // nc[i] = nc[i];
      } else if (i+1 < ik) {
        nc[i] = NA_INTEGER;
      } else {
        nc[i] = nc[i] - nc[i-ik]; // k[i]==0 results in nc[i]=0
      }
    }
  }
  return cnc;
}
/* fast rolling adaptive NA stats - exact
 * it runs nested loop on contrary to fast which keep running NA count
 * not used as of now, we should use this function when we will implement frolladaptivemedianFast, now because there is no frolladaptivemedianFast we need more optimized frolladaptivemedianExact, so we run at least NA stats in online fashion
 * note that for using frolladaptiveNAExact we need calloc, for frolladaptiveNAFast malloc is ok
 * returns total number of NA/NaN in x
 * updates:
 *   nc for rolling NA count
 *   isna for NA mask
 */
/*static int frolladaptiveNAExact(const double *x, uint64_t nx, const int *k, int *nc, bool *isna) {
  int NC = 0;
  for (uint64_t i=0; i<nx; i++) {
    if (ISNAN(x[i])) {
      isna[i] = true;
      NC++;
    }
  }
  if (NC) {
    for (uint64_t i=0; i<nx; i++) {
      int ik = k[i];
      if (i+1 < ik) { // k[0]==0: (uint64_t)ik turns into 18446744071562067968 so does not satisfy this IF
        nc[i] = NA_INTEGER;
      } else if (ik) { // escape k[i]==0, nc has to be preallocated with 0
        bool *iisna = &isna[i-ik+1];
        for (int j=0; j<ik; j++) {
          if (iisna[j])
            nc[i]++;
        }
      }
    }
  }
  return NC;
}*/
/* fast rolling adaptive median - exact
 * loop in parallel for each element of x and call quickselect
 */
void frolladaptivemedianExact(const double *x, uint64_t nx, ans_t *ans, const int *k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", hasnf %d, narm %d\n"), "frolladaptivemedianExact", (uint64_t)nx, hasnf, (int) narm);
  int maxk = k[0]; // find largest window size
  for (uint64_t i=1; i<nx; i++) {
    if (k[i] > maxk)
      maxk = k[i];
  }
  if (maxk == 0) { // edge case of n=rep(0,lenght(x)) - needs to be here to avoid malloc(0)
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: adaptive window width of size 0, returning all NA vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = NA_REAL;
    }
    return;
  }
  if (hasnf == 0) {
    for (uint64_t i=0; i<nx; i++) {
      if (ISNAN(x[i])) {
        hasnf = 1;
        break;
      }
    }
    if (hasnf == 0)
      hasnf = -1;
  } // detect NAs
  int nth = getDTthreads(nx, true);
  double *xx = malloc(sizeof(*xx) * maxk * nth); // quickselect sorts in-place so we need to copy
  if (!xx) { // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for xx", __func__); // raise error
    return;
  } // # nocov end
  if (hasnf == -1) {
    #pragma omp parallel for num_threads(nth)
    for (uint64_t i=0; i<nx; i++) {
      int ik = k[i];
      if (i+1 < ik) {
        ans->dbl_v[i] = fill;
      } else if (ik) { // escape k[0]==0
        int th = omp_get_thread_num();
        double *thx = &xx[th * maxk]; // thread-specific x
        memcpy(thx, &x[i-ik+1], ik * sizeof(*thx));
        ans->dbl_v[i] = dquickselect(thx, ik);
      } else { // k[0]==0
        ans->dbl_v[i] = NA_REAL;
      }
    }
  } else {
    //int *rollnc = cal_NOLINT_loc(nx, sizeof(*rollnc)); // use when frolladaptiveFast will be implemented - remove _NOLINT_ then
    int *rollnc = malloc(sizeof(*rollnc) * nx); // for frolladaptiveNAExact we need calloc, for frolladaptiveNAFast malloc is ok
    if (!rollnc) { // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for rollnc", __func__); // raise error
      free(xx);
      return;
    } // # nocov end
    bool *isna = calloc(nx, sizeof(*isna));
    if (!isna) { // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for isna", __func__); // raise error
      free(rollnc); free(xx);
      return;
    } // # nocov end
    //int nc = frolladaptiveNAExact(x, nx, k, rollnc, isna); // use when frolladaptiveFast will be implemented
    int nc = frolladaptiveNAFast(x, nx, k, rollnc, isna);
    if (!nc) { // total NA for x
      if (verbose)
        snprintf(end(ans->message[0]), 500, _("%s: no NAs detected, redirecting to itself using has.nf=FALSE\n"), "frolladaptivemedianExact");
      free(isna); free(rollnc); free(xx);
      frolladaptivemedianExact(x, nx, ans, k, fill, narm, /*hasnf=*/false, verbose);
      return;
    }
    #pragma omp parallel for num_threads(nth)
    for (uint64_t i=0; i<nx; i++) {
      int ik = k[i];
      if (i+1 < ik) {
        ans->dbl_v[i] = fill;
      } else {
        const double *ix = &x[i-ik+1];
        int th = omp_get_thread_num();
        double *thx = &xx[th*maxk];
        int inc = rollnc[i];
        if (!inc) { // k[i]==0 lands here because there must have been 0 NAs in 0 length window
          if (ik) {
            memcpy(thx, ix, ik * sizeof(*thx));
            ans->dbl_v[i] = dquickselect(thx, ik);
          } else {
            ans->dbl_v[i] = NA_REAL; // k[i]==0
          }
        } else {
          if (!narm || inc == ik) {
            ans->dbl_v[i] = NA_REAL;
          } else {
            bool *iisna = &isna[i-ik+1];
            int thxn = 0;
            for (int j=0; j<ik; j++) {
              if (!iisna[j])
                thx[thxn++] = ix[j];
            }
            ans->dbl_v[i] = dquickselect(thx, ik-inc);
          }
        }
      }
    }
    free(isna); free(rollnc);
  }
  free(xx);
}
