#include "data.table.h"

/*
  OpenMP is used here to parallelize the loops in most of the
    implementations of rolling functions.

  These functions benefit more in terms of speedup when the data has a large
    number of columns, primarily due to the efficient memory access patterns
    (cache-friendly) used when processing the data for each column
    sequentially in memory to compute the rolling statistic.
*/

/* rolling fun - router for fun and algo
 * early stopping for window bigger than input
 * handles 'align' in single place for center or left
 * rfun enum rollfun_t routes to rolling function
 * algo = 0: fast
 *   adding/removing in/out of sliding window of observations
 * algo = 1: exact
 *   recalculate whole fun for each observation, for mean roundoff correction is adjusted
 */
void frollfun(rollfun_t rfun, unsigned int algo, const double *x, uint64_t nx, ans_t *ans, int k, int align, double fill, bool narm, int hasnf, bool verbose) {
  double tic = 0;
  if (verbose)
    tic = omp_get_wtime();
  if (nx < k) {                      // if window width bigger than input just return vector of fill values
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width longer than input vector, returning all NA vector\n"), __func__);
    // implicit n_message limit discussed here: https://github.com/Rdatatable/data.table/issues/3423#issuecomment-487722586
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = fill;
    }
    return;
  }
  switch (rfun) {
  case MEAN :
    if (algo==0) {
      frollmeanFast(x, nx, ans, k, fill, narm, hasnf, verbose);
    } else if (algo==1) {
      frollmeanExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  case SUM :
    if (algo==0) {
      frollsumFast(x, nx, ans, k, fill, narm, hasnf, verbose);
    } else if (algo==1) {
      frollsumExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  case MAX :
    if (algo==0) {
      frollmaxFast(x, nx, ans, k, fill, narm, hasnf, verbose);
    } else if (algo==1) {
      frollmaxExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  case MIN :
    if (algo==0) {
      frollminFast(x, nx, ans, k, fill, narm, hasnf, verbose);
    } else if (algo==1) {
      frollminExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  case PROD :
    if (algo==0) {
      frollprodFast(x, nx, ans, k, fill, narm, hasnf, verbose);
    } else if (algo==1) {
      frollprodExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  default: // #nocov
    error(_("Internal error: Unknown rfun value in froll: %d"), rfun); // #nocov
  }
  if (align < 1 && ans->status < 3) {
    int k_ = align==-1 ? k-1 : floor(k/2);       // offset to shift
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: align %d, shift answer by %d\n"), __func__, align, -k_);
    memmove((char *)ans->dbl_v, (char *)ans->dbl_v + (k_*sizeof(double)), (nx-k_)*sizeof(double)); // apply shift to achieve expected align
    for (uint64_t i=nx-k_; i<nx; i++) {          // fill from right side
      ans->dbl_v[i] = fill;
    }
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: processing fun %d algo %u took %.3fs\n"), __func__, rfun, algo, omp_get_wtime()-tic);
}

#undef SUM_WINDOW_STEP_FRONT
#define SUM_WINDOW_STEP_FRONT                                  \
  if (R_FINITE(x[i])) {                                        \
    w += x[i];                                                 \
  } else if (ISNAN(x[i])) {                                    \
    nc++;                                                      \
  } else if (x[i]==R_PosInf) {                                 \
    pinf++;                                                    \
  } else if (x[i]==R_NegInf) {                                 \
    ninf++;                                                    \
  }
#undef SUM_WINDOW_STEP_BACK
#define SUM_WINDOW_STEP_BACK                                   \
  if (R_FINITE(x[i-k])) {                                      \
    w -= x[i-k];                                               \
  } else if (ISNAN(x[i-k])) {                                  \
    nc--;                                                      \
  } else if (x[i-k]==R_PosInf) {                               \
    pinf--;                                                    \
  } else if (x[i-k]==R_NegInf) {                               \
    ninf--;                                                    \
  }
#undef MEAN_WINDOW_STEP_VALUE
#define MEAN_WINDOW_STEP_VALUE                                 \
  if (nc == 0) {                                               \
    if (pinf == 0) {                                           \
      if (ninf == 0) {                                         \
        ans->dbl_v[i] = (double) (w / k);                      \
      } else {                                                 \
        ans->dbl_v[i] = R_NegInf;                              \
      }                                                        \
    } else if (ninf == 0) {                                    \
      ans->dbl_v[i] = R_PosInf;                                \
    } else {                                                   \
      ans->dbl_v[i] = R_NaN;                                   \
    }                                                          \
  } else if (nc == k) {                                        \
    ans->dbl_v[i] = narm ? R_NaN : NA_REAL;                    \
  } else {                                                     \
    if (narm) {                                                \
      if (pinf == 0) {                                         \
        if (ninf == 0) {                                       \
          ans->dbl_v[i] = (double) (w / (k - nc));             \
        } else {                                               \
          ans->dbl_v[i] = R_NegInf;                            \
        }                                                      \
      } else if (ninf == 0) {                                  \
        ans->dbl_v[i] = R_PosInf;                              \
      } else {                                                 \
        ans->dbl_v[i] = R_NaN;                                 \
      }                                                        \
    } else {                                                   \
      ans->dbl_v[i] = NA_REAL;                                 \
    }                                                          \
  }

/* fast rolling mean - fast
 * when no info on NF (has.nf argument) then assume no NFs run faster version
 * rollmean implemented as single pass sliding window for align="right"
 * if non-finite detected re-run rollmean implemented as single pass sliding window with NA support
 */
void frollmeanFast(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollmeanFast", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all NaN vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = R_NaN;
    }
    return;
  }
  long double w = 0.0;                                          // sliding window aggregate
  bool truehasnf = hasnf>0;                                     // flag to re-run with NA support if NAs detected
  if (!truehasnf) {
    int i;                                                      // iterator declared here because it is being used after for loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window only; #loop_counter_not_local_scope_ok
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
        if (hasnf==-1)
          ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
        if (verbose)
          ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
        w = 0.0; truehasnf = true;
      }
    } else {                                                    // early stopping branch when NAs detected in first k obs
      if (hasnf==-1)
        ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
      if (verbose)
        ansSetMsg(ans, 0, "%s: non-finite values are present in input, skip non-finite unaware attempt and run with extra care for NFs straighaway\n", __func__);
      w = 0.0; truehasnf = true;
    }
  }
  if (truehasnf) {
    int nc = 0, pinf = 0, ninf = 0;                             // NA counter within sliding window
    int i;                                                      // iterator declared here because it is being used after for loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window only; #loop_counter_not_local_scope_ok
      SUM_WINDOW_STEP_FRONT
      ans->dbl_v[i] = fill;                                     // partial window fill all
    }
    SUM_WINDOW_STEP_FRONT                                       // i==k-1
    MEAN_WINDOW_STEP_VALUE
    for (uint64_t i=k; i<nx; i++) {                             // loop over obs, complete window, all remaining after partial window
      SUM_WINDOW_STEP_BACK
      SUM_WINDOW_STEP_FRONT
      MEAN_WINDOW_STEP_VALUE
    }
  }
}
/* fast rolling mean - exact
 * when no info on NF (has.nf argument) then assume no NFs run faster version, also when na.rm=FALSE faster version can proceed
 * rollmean implemented as mean of k obs for each observation for align="right"
 * if non-finite detected and na.rm=TRUE then re-run NF aware rollmean
 */
void frollmeanExact(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollmeanExact", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all NaN vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = R_NaN;
    }
    return;
  }
  for (int i=0; i<k-1; i++) {                                   // fill partial window only
    ans->dbl_v[i] = fill;
  }
  bool truehasnf = hasnf>0;                                     // flag to re-run with NA support if NAs detected
  if (!truehasnf || !narm) {
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {                           // loop on every observation with complete window, partial already filled in single threaded section
      if (narm && truehasnf) {
        continue;                                               // if NAs detected no point to continue
      }
      long double w = 0.0;
      for (int j=-k+1; j<=0; j++) {                             // sub-loop on window width
        w += x[i+j];                                            // sum of window for particular observation
      }
      if (R_FINITE((double) w)) {                               // no need to calc roundoff correction if NAs detected as will re-call all below in truehasnf==1
        long double res = w / k;                                // keep results as long double for intermediate processing
        long double err = 0.0;                                  // roundoff corrector
        for (int j=-k+1; j<=0; j++) {                           // nested loop on window width
          err += x[i+j] - res;                                  // measure difference of obs in sub-loop to calculated fun for obs
        }
        ans->dbl_v[i] = (double) (res + (err / k));             // adjust calculated rollfun with roundoff correction
      } else if (ISNAN((double) w)) {
        if (!narm) {
          ans->dbl_v[i] = (double) w;                           // NAs should be propagated
        }
        truehasnf = true;                                       // NAs detected for this window, set flag so rest of windows will not be re-run
      } else {
        ans->dbl_v[i] = (double) w;                             // Inf and -Inf
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
      if (R_FINITE((double) w)) {
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
      } else {
        ans->dbl_v[i] = (double) w;                             // Inf and -Inf
      }
    }
  }
}

#undef SUM_WINDOW_STEP_VALUE
#define SUM_WINDOW_STEP_VALUE                                    \
  if (nc == 0) {                                                 \
    if (pinf == 0) {                                             \
      if (ninf == 0) {                                           \
        ans->dbl_v[i] = (double) w;                              \
      } else {                                                   \
        ans->dbl_v[i] = R_NegInf;                                \
      }                                                          \
    } else if (ninf == 0) {                                      \
      ans->dbl_v[i] = R_PosInf;                                  \
    } else {                                                     \
      ans->dbl_v[i] = R_NaN;                                     \
    }                                                            \
  } else if (nc == k) {                                          \
    ans->dbl_v[i] = narm ? 0.0 : NA_REAL;                        \
  } else {                                                       \
    if (narm) {                                                  \
      if (pinf == 0) {                                           \
        if (ninf == 0) {                                         \
          ans->dbl_v[i] = (double) w;                            \
        } else {                                                 \
          ans->dbl_v[i] = R_NegInf;                              \
        }                                                        \
      } else if (ninf == 0) {                                    \
        ans->dbl_v[i] = R_PosInf;                                \
      } else {                                                   \
        ans->dbl_v[i] = R_NaN;                                   \
      }                                                          \
    } else {                                                     \
      ans->dbl_v[i] = NA_REAL;                                   \
    }                                                            \
  }

/* fast rolling sum - fast
 * same as mean fast
 */
void frollsumFast(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollsumFast", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all 0 vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = 0.0;
    }
    return;
  }
  long double w = 0.0;
  bool truehasnf = hasnf>0;
  if (!truehasnf) {
    int i;
    for (i=0; i<k-1; i++) { // #loop_counter_not_local_scope_ok
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
          if (hasnf==-1)
            ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
          if (verbose)
            ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
        w = 0.0; truehasnf = true;
      }
    } else {
        if (hasnf==-1)
          ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
        if (verbose)
          ansSetMsg(ans, 0, "%s: non-finite values are present in input, skip non-finite unaware attempt and run with extra care for NFs straighaway\n", __func__);
      w = 0.0; truehasnf = true;
    }
  }
  if (truehasnf) {
    int nc = 0, pinf = 0, ninf = 0;                             // NA counter within sliding window
    int i;                                                      // iterator declared here because it is being used after for loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window only; #loop_counter_not_local_scope_ok
      SUM_WINDOW_STEP_FRONT
      ans->dbl_v[i] = fill;                                     // partial window fill all
    }
    SUM_WINDOW_STEP_FRONT                                       // i==k-1
    SUM_WINDOW_STEP_VALUE
    for (uint64_t i=k; i<nx; i++) {                             // loop over obs, complete window, all remaining after partial window
      SUM_WINDOW_STEP_BACK
      SUM_WINDOW_STEP_FRONT
      SUM_WINDOW_STEP_VALUE
    }
  }
}
/* fast rolling sum - exact
 * same as mean exact
 */
void frollsumExact(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollsumExact", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all 0 vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = 0.0;
    }
    return;
  }
  for (int i=0; i<k-1; i++) {
    ans->dbl_v[i] = fill;
  }
  bool truehasnf = hasnf>0;
  if (!truehasnf || !narm) {
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {
      if (narm && truehasnf) {
        continue;
      }
      long double w = 0.0;
      for (int j=-k+1; j<=0; j++) {
        w += x[i+j];
      }
      if (R_FINITE((double) w)) {
        ans->dbl_v[i] = (double) w;
      } else if (ISNAN((double) w)) {
        if (!narm) {
          ans->dbl_v[i] = (double) w;
        }
        truehasnf = true;
      } else {
        ans->dbl_v[i] = (double) w;
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
      if (w > DBL_MAX) { // in contrast to mean, here we can overflow long double more than DBL_MAX
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

static inline void wmax(const double * restrict x, uint64_t o, int k, double * restrict w, uint64_t *iw, bool narm) {
  if (narm) {
    for (int i=0; i<k; i++) {
      const uint64_t ii = o+i-k+1;
      if (x[ii] >= w[0]) { // this never true if all x NAs and narm=TRUE
        iw[0] = ii;
        w[0] = x[ii];
      }
    }
  } else {
    double ww = R_NegInf;
    uint64_t iww = 0;
    for (int i=0; i<k; i++) {
      uint64_t ii = o+i-k+1;
      if (ISNAN(x[ii])) {
        if (ISNA(x[ii])) {
          internal_error(__func__, "frollmax reached untested branch of code, for now use frollmax algo='exact', please provide reproducible example of your frollmax usage to data.table github issue tracker"); // # nocov
          //iww = ii; ww = NA_REAL;
        } else if (ISNA(ww)) {
          internal_error(__func__, "frollmax reached untested branch of code, for now use frollmax algo='exact', please provide reproducible example of your frollmax usage to data.table github issue tracker"); // # nocov
          // do nothing because w > x[i]: NA > NaN
        } else { // no NA in window so NaN >= than any non-NA
          iww = ii; ww = R_NaN;
        }
      } else if (ISNAN(ww)) {
        // w still within the window and is NA or NaN, x[i] is not NA - already checked above, therefore to nothing
      } else if (x[ii] >= ww) {
        iww = ii; ww = x[iww];
      }
    }
    iw[0] = iww;
    w[0] = ww;
  }
}
/* fast rolling max - fast
 * fast online algorithm do single pass over elements keeping track of recent max and its index
 * if index of max is within progressing window then it keeps running single pass
 * whenever max is leaving the window (index of max is outside of iterator minus window size) then new maximum is computed via nested loop on current location
 * new max is used to continue outer single pass as long as new max index is not leaving the running window
 * should scale well for bigger window size, may carry overhead for small window, needs benchmarking
 */
void frollmaxFast(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollmaxFast", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all -Inf vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = R_NegInf;
    }
    return;
  }
  double w = R_NegInf; // window max
  uint64_t cmax = 0; // counter of nested loops for verbose
  uint64_t iw = 0; // index of window max
  uint64_t i;
  if (narm || hasnf==-1) {
    for (i=0; i<k-1; i++) { // #loop_counter_not_local_scope_ok
      if (x[i] >= w) { // >= rather than > because we track most recent maximum using iw
        iw = i; w = x[iw];
      }
      ans->dbl_v[i] = fill;
    }
    for (i=k-1; i<nx; i++) {
      if (iw+k <= i) { // max left current window
        iw = i-k; w = R_NegInf;
        wmax(x, i, k, &w, &iw, true); cmax++;
      } else if (x[i] >= w) {
        iw = i; w = x[iw];
      }
      ans->dbl_v[i] = w;
    }
  } else {
    bool truehasnf = hasnf>0;
    for (i=0; i<k-1; i++) { // up to first full window only #loop_counter_not_local_scope_ok
      if (ISNAN(x[i])) {
        truehasnf = true;
        if (ISNA(x[i])) {
          iw = i; w = NA_REAL;
        } else if (ISNA(w)) {
          // do nothing because w > x[i]: NA > NaN
        } else {
          iw = i; w = R_NaN;
        }
      } else if (x[i] >= w) {
        iw = i; w = x[iw];
      }
      ans->dbl_v[i] = fill;
    }
    if (!truehasnf) { // maybe no NAs
      for (; i<nx; i++) {
        if (ISNAN(x[i])) { // Inf properly propagates
          if (verbose)
            ansSetMsg(ans, 0, "%s: non-finite values are present in input, continue with extra care for NFs\n", __func__);
          truehasnf = true;
          break; // does not increment, w stays as from previous iteration
        }
        if (iw+k <= i) { // max left current window
          iw = i-k; w = R_NegInf;
          wmax(x, i, k, &w, &iw, true); cmax++;
        } else if (x[i] >= w) {
          iw = i; w = x[iw];
        }
        ans->dbl_v[i] = w;
      }
    }
    if (truehasnf) {
      for (; i<nx; i++) { // this loop continues from where "maybe no NAs" loop left
        if (ISNAN(x[i])) {
          if (ISNA(x[i])) {
            iw = i; w = NA_REAL;
          } else if (ISNA(w)) {
            // do nothing because w > x[i]: NA > NaN
          } else { // no NA in window so NaN >= than any non-NA
            iw = i; w = R_NaN;
          }
        } else if (iw+k <= i) { // max left current window
          iw = i-k; w = R_NegInf;
          wmax(x, i, k, &w, &iw, false); cmax++;
        } else if (ISNAN(w)) {
          // w still within the window and is NA or NaN, x[i] is not NA - already checked above, therefore do nothing
        } else if (x[i] >= w) {
          iw = i; w = x[iw];
        }
        ans->dbl_v[i] = w;
      }
    }
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: nested window max calculation called %"PRIu64" times\n"), __func__, cmax);
}
/* fast rolling max - exact
 * for each observation in x compute max in window from scratch
 * faster version ignores NAs (narm=T or has.nf=F), as they are not propagated by `>` operator
 * otherwise we scan for NaN/NA and run either of two loops
 * has.nf=FALSE can give incorrect results if NAs provided, documented to be used with care
 */
void frollmaxExact(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollmaxExact", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all -Inf vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = R_NegInf;
    }
    return;
  }
  for (int i=0; i<k-1; i++) {
    ans->dbl_v[i] = fill;
  }
  if (narm || hasnf==-1) {                                          // ignore NAs as > does not propagate
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {
      double w = R_NegInf;
      for (int j=-k+1; j<=0; j++) {
        if (x[i+j] > w)
          w = x[i+j];
      }
      ans->dbl_v[i] = w;
    }
  } else {
    bool *isnan = malloc(sizeof(*isnan) * nx);                      // isnan lookup - we use it to reduce ISNAN calls in nested loop
    if (!isnan) {                                                   // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for isnan", __func__); // raise error
      return;
    }                                                               // # nocov end
    bool truehasnf = hasnf>0;
    for (uint64_t i=0; i<nx; i++) {                                 // no openmp as this should be very fast
      if (ISNAN(x[i])) {
        truehasnf = true;
        isnan[i] = true;
      } else {
        isnan[i] = false;
      }
    }
    if (!truehasnf) {                                               // not found any NAs
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=k-1; i<nx; i++) {
        double w = R_NegInf;
        for (int j=-k+1; j<=0; j++) {
          if (x[i+j] > w)
            w = x[i+j];
        }
        ans->dbl_v[i] = w;
      }
    } else {                                                        // there are some NAs
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=k-1; i<nx; i++) {
        double w = R_NegInf;
        if (isnan[i] && ISNA(x[i])) {
          w = NA_REAL;
        } else {
          for (int j=-k+1; j<=0; j++) {
            if (isnan[i+j]) {
              if (ISNA(x[i+j])) {
                w = NA_REAL;
                break;                                              // break because NA > NaN
              } else {
                w = R_NaN;                                          // continue nested loop in case there is NA there
              }
            } else if (x[i+j] > w)
              w = x[i+j];
          }
        }
        ans->dbl_v[i] = w;
      }
    }
    free(isnan);
  }
}

static inline void wmin(const double * restrict x, uint64_t o, int k, double * restrict w, uint64_t *iw, bool narm) {
  if (narm) {
    for (int i=0; i<k; i++) {
      if (x[o+i-k+1] <= w[0]) { // this never true if all x NAs and narm=TRUE
        iw[0] = o+i-k+1;
        w[0] = x[iw[0]];
      }
    }
  } else {
    double ww = R_PosInf;
    uint64_t iww = 0;
    for (int i=0; i<k; i++) {
      uint64_t ii = o+i-k+1;
      if (ISNAN(x[ii])) {
        if (ISNA(x[ii])) {
          internal_error(__func__, "frollmin reached untested branch of code, for now use frollmin algo='exact', please provide reproducible example of your frollmin usage to data.table github issue tracker"); // # nocov
          //iww = ii; ww = NA_REAL;
        } else if (ISNA(ww)) {
          internal_error(__func__, "frollmin reached untested branch of code, for now use frollmin algo='exact', please provide reproducible example of your frollmin usage to data.table github issue tracker"); // # nocov
          // do nothing because w > x[i]: NA > NaN
        } else { // no NA in window so NaN >= than any non-NA
          iww = ii; ww = R_NaN;
        }
      } else if (ISNAN(ww)) {
        // w still within the window and is NA or NaN, x[i] is not NA - already checked above, therefore to nothing
      } else if (x[ii] <= ww) {
        iww = ii; ww = x[iww];
      }
    }
    iw[0] = iww;
    w[0] = ww;
  }
}
/* fast rolling min - fast
 * see rolling max fast details
 */
void frollminFast(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollminFast", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all +Inf vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = R_PosInf;
    }
    return;
  }
  double w = R_PosInf; // window min
  uint64_t cmin = 0; // counter of nested loops for verbose
  uint64_t iw = 0; // index of window min
  uint64_t i;
  if (narm || hasnf==-1) {
    for (i=0; i<k-1; i++) { // #loop_counter_not_local_scope_ok
      if (x[i] <= w) { // <= rather than < because we track most recent minimum using iw
        iw = i; w = x[iw];
      }
      ans->dbl_v[i] = fill;
    }
    for (i=k-1; i<nx; i++) {
      if (iw+k <= i) { // min left current window // note that it is still <= same as in max
        iw = i-k; w = R_PosInf;
        wmin(x, i, k, &w, &iw, true); cmin++;
      } else if (x[i] <= w) {
        iw = i; w = x[iw];
      }
      ans->dbl_v[i] = w;
    }
  } else {
    bool truehasnf = hasnf>0;
    for (i=0; i<k-1; i++) { // up to first full window only #loop_counter_not_local_scope_ok
      if (ISNAN(x[i])) {
        truehasnf = true;
        if (ISNA(x[i])) {
          iw = i; w = NA_REAL;
        } else if (ISNA(w)) {
          // do nothing because w > x[i]: NA > NaN
        } else {
          iw = i; w = R_NaN;
        }
      } else if (x[i] <= w) {
        iw = i; w = x[iw];
      }
      ans->dbl_v[i] = fill;
    }
    if (!truehasnf) { // maybe no NAs
      for (; i<nx; i++) {
        if (ISNAN(x[i])) { // Inf properly propagates
          if (verbose)
            ansSetMsg(ans, 0, "%s: non-finite values are present in input, continue with extra care for NFs\n", __func__);
          truehasnf = true;
          break; // does not increment, w stays as from previous iteration
        }
        if (iw+k <= i) { // min left current window // note that it is still <= same as in max
          iw = i-k; w = R_PosInf;
          wmin(x, i, k, &w, &iw, true); cmin++;
        } else if (x[i] <= w) {
          iw = i; w = x[iw];
        }
        ans->dbl_v[i] = w;
      }
    }
    if (truehasnf) {
      for (; i<nx; i++) { // this loop continues from where "maybe no NAs" loop left
        if (ISNAN(x[i])) {
          if (ISNA(x[i])) {
            iw = i; w = NA_REAL;
          } else if (ISNA(w)) {
            // do nothing because w > x[i]: NA > NaN
          } else { // no NA in window so NaN >= than any non-NA
            iw = i; w = R_NaN;
          }
        } else if (iw+k <= i) { // min left current window // note that it is still <= same as in max
          iw = i-k; w = R_PosInf;
          wmin(x, i, k, &w, &iw, false); cmin++;
        } else if (ISNAN(w)) {
          // w still within the window and is NA or NaN, x[i] is not NA - already checked above, therefore do nothing
        } else if (x[i] <= w) {
          iw = i; w = x[iw];
        }
        ans->dbl_v[i] = w;
      }
    }
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: nested window min calculation called %"PRIu64" times\n"), __func__, cmin);
}
/* fast rolling min - exact
 * see rolling max exact details
 */
void frollminExact(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollminExact", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all +Inf vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = R_PosInf;
    }
    return;
  }
  for (int i=0; i<k-1; i++) {
    ans->dbl_v[i] = fill;
  }
  if (narm || hasnf==-1) {                                          // ignore NAs as > does not propagate
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {
      double w = R_PosInf;
      for (int j=-k+1; j<=0; j++) {
        if (x[i+j] < w)
          w = x[i+j];
      }
      ans->dbl_v[i] = w;
    }
  } else {
    bool *isnan = malloc(sizeof(*isnan) * nx);                       // isnan lookup - we use it to reduce ISNAN calls in nested loop
    if (!isnan) {                                                   // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for isnan", __func__); // raise error
      return;
    }                                                               // # nocov end
    bool truehasnf = hasnf>0;
    for (uint64_t i=0; i<nx; i++) {                                 // no openmp as this should be very fast
      if (ISNAN(x[i])) {
        truehasnf = true;
        isnan[i] = true;
      } else {
        isnan[i] = false;
      }
    }
    if (!truehasnf) {                                               // not found any NAs
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=k-1; i<nx; i++) {
        double w = R_PosInf;
        for (int j=-k+1; j<=0; j++) {
          if (x[i+j] < w)
            w = x[i+j];
        }
        ans->dbl_v[i] = w;
      }
    } else {                                                        // there are some NAs
      #pragma omp parallel for num_threads(getDTthreads(nx, true))
      for (uint64_t i=k-1; i<nx; i++) {
        double w = R_PosInf;
        if (isnan[i] && ISNA(x[i])) {
          w = NA_REAL;
        } else {
          for (int j=-k+1; j<=0; j++) {
            if (isnan[i+j]) {
              if (ISNA(x[i+j])) {
                w = NA_REAL;
                break;                                              // break because NA > NaN
              } else {
                w = R_NaN;                                          // continue nested loop in case there is NA there
              }
            } else if (x[i+j] < w)
              w = x[i+j];
          }
        }
        ans->dbl_v[i] = w;
      }
    }
    free(isnan);
  }
}

#undef PROD_WINDOW_STEP_FRONT
#define PROD_WINDOW_STEP_FRONT                                   \
  if (R_FINITE(x[i])) {                                          \
    w *= x[i];                                                   \
  } else if (ISNAN(x[i])) {                                      \
    nc++;                                                        \
  } else if (x[i]==R_PosInf) {                                   \
    pinf++;                                                      \
  } else if (x[i]==R_NegInf) {                                   \
    ninf++;                                                      \
  }
#undef PROD_WINDOW_STEP_BACK
#define PROD_WINDOW_STEP_BACK                                    \
  if (R_FINITE(x[i-k])) {                                        \
    w /= x[i-k];                                                 \
  } else if (ISNAN(x[i-k])) {                                    \
    nc--;                                                        \
  } else if (x[i-k]==R_PosInf) {                                 \
    pinf--;                                                      \
  } else if (x[i-k]==R_NegInf) {                                 \
    ninf--;                                                      \
  }
#undef PROD_WINDOW_STEP_VALUE
#define PROD_WINDOW_STEP_VALUE                                   \
  if (nc == 0) {                                                 \
    if (pinf == 0 && ninf == 0) {                                \
      ans->dbl_v[i] = (double) w;                                \
    } else {                                                     \
      ans->dbl_v[i] = (ninf+(w<0))%2 ? R_NegInf : R_PosInf;      \
    }                                                            \
  } else if (nc == k) {                                          \
    ans->dbl_v[i] = narm ? 1.0 : NA_REAL;                        \
  } else {                                                       \
    if (narm) {                                                  \
      if (pinf == 0 && ninf == 0) {                              \
        ans->dbl_v[i] = (double) w;                              \
      } else {                                                   \
        ans->dbl_v[i] = (ninf+(w<0))%2 ? R_NegInf : R_PosInf;    \
      }                                                          \
    } else {                                                     \
      ans->dbl_v[i] = NA_REAL;                                   \
    }                                                            \
  }

/* fast rolling prod - fast
 * same as mean fast
 */
void frollprodFast(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollprodFast", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all 1 vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = 1.0;
    }
    return;
  }
  long double w = 1.0;
  bool truehasnf = hasnf>0;
  if (!truehasnf) {
    int i;
    for (i=0; i<k-1; i++) { // #loop_counter_not_local_scope_ok
      w *= x[i];
      ans->dbl_v[i] = fill;
    }
    w *= x[i];
    ans->dbl_v[i] = (double) w;
    if (R_FINITE((double) w)) {
      for (uint64_t i=k; i<nx; i++) {
        w /= x[i-k];
        w *= x[i];
        ans->dbl_v[i] = (double) w;
      }
      if (!R_FINITE((double) w)) {
        if (hasnf==-1)
          ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
        if (verbose)
          ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
        w = 1.0; truehasnf = true;
      }
    } else {
      if (hasnf==-1)
        ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
      if (verbose)
        ansSetMsg(ans, 0, "%s: non-finite values are present in input, skip non-finite inaware attempt and run with extra care for NFs straighaway\n", __func__);
      w = 1.0; truehasnf = true;
    }
  }
  if (truehasnf) {
    int nc = 0, pinf = 0, ninf = 0;                             // NA counter within sliding window
    int i;                                                      // iterator declared here because it is being used after for loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window only; #loop_counter_not_local_scope_ok
      PROD_WINDOW_STEP_FRONT
      ans->dbl_v[i] = fill;                                     // partial window fill all
    }
    PROD_WINDOW_STEP_FRONT                                       // i==k-1
    PROD_WINDOW_STEP_VALUE
    for (uint64_t i=k; i<nx; i++) {                             // loop over obs, complete window, all remaining after partial window
      PROD_WINDOW_STEP_BACK
      PROD_WINDOW_STEP_FRONT
      PROD_WINDOW_STEP_VALUE
    }
  }
}
/* fast rolling prod - exact
 * same as mean exact
 */
void frollprodExact(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollprodExact", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all 1 vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = 1.0;
    }
    return;
  }
  for (int i=0; i<k-1; i++) {
    ans->dbl_v[i] = fill;
  }
  bool truehasnf = hasnf>0;
  if (!truehasnf || !narm) {
    #pragma omp parallel for num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {
      if (narm && truehasnf) {
        continue;
      }
      long double w = 1.0;
      for (int j=-k+1; j<=0; j++) {
        w *= x[i+j];
      }
      if (R_FINITE((double) w)) {
        ans->dbl_v[i] = (double) w;
      } else if (ISNAN((double) w)) {
        if (!narm) {
          ans->dbl_v[i] = (double) w;
        }
        truehasnf = true;
      } else {
        ans->dbl_v[i] = (double) w;
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
    for (uint64_t i=k-1; i<nx; i++) {
      long double w = 1.0;
      int nc = 0;
      for (int j=-k+1; j<=0; j++) {
        if (ISNAN(x[i+j])) {
          nc++;
        } else {
          w *= x[i+j];
        }
      }
      if (w > DBL_MAX) { // in contrast to mean, here we can overflow long double more than DBL_MAX
        ans->dbl_v[i] = R_PosInf;
      } else if (w < -DBL_MAX) {
        ans->dbl_v[i] = R_NegInf;
      } else {
        if (nc < k) {
          ans->dbl_v[i] = (double) w;
        } else {
          ans->dbl_v[i] = 1.0;
        }
      }
    }
  }
}
