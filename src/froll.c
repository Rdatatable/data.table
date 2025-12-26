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
void frollfun(rollfun_t rfun, unsigned int algo, const double *x, uint64_t nx, ans_t *ans, int k, int align, double fill, bool narm, int hasnf, bool verbose, bool par) {
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
  case MEDIAN :
    if (algo==0) {
      frollmedianFast(x, nx, ans, k, fill, narm, hasnf, verbose, par);
    } else if (algo==1) {
      frollmedianExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  case VAR :
    if (algo==0) {
      frollvarFast(x, nx, ans, k, fill, narm, hasnf, verbose, par); // par is used only when NAs - fallback to exact, to know if outer parallelism has been applied
    } else if (algo==1) {
      if (!par) // par should be true because frollvarExact at this place was invoked directly, and not by fallback, so algo=exact have been used explicitly, then outer parallelism in frollR.c is disabled already
        internal_error(__func__, "par=FALSE but should be TRUE, algo=exact should have disabled outer parallelism for vectorized input so frollvarExact should be allowed to go parallel"); // # nocov
      frollvarExact(x, nx, ans, k, fill, narm, hasnf, verbose, par);
    }
    break;
  case SD :
    if (algo==0) {
      frollsdFast(x, nx, ans, k, fill, narm, hasnf, verbose, par); // par is used only when NAs - fallback to exact, to know if outer parallelism has been applied
    } else if (algo==1) {
      frollsdExact(x, nx, ans, k, fill, narm, hasnf, verbose);
    }
    break;
  default: // # nocov
    internal_error(__func__, "Unknown rfun value in froll: %d", rfun); // # nocov
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
    #pragma omp parallel for num_threads(getDTthreads(nx, true)) shared(truehasnf)
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
        #pragma omp atomic write
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
    #pragma omp parallel for num_threads(getDTthreads(nx, true)) shared(truehasnf)
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
        #pragma omp atomic write
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
    if (x[i] == 0.0) {                                           \
      zerc++;                                                    \
    } else {                                                     \
      w *= x[i];                                                 \
    }                                                            \
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
    if (x[i-k] == 0.0) {                                         \
      zerc--;                                                    \
    } else {                                                     \
      w /= x[i-k];                                               \
    }                                                            \
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
      if (zerc) {                                                \
        ans->dbl_v[i] = 0.0;                                     \
      } else {                                                   \
        ans->dbl_v[i] = (double) w;                              \
      }                                                          \
    } else {                                                     \
      if (zerc) {                                                \
        ans->dbl_v[i] = R_NaN;                                   \
      } else {                                                   \
        ans->dbl_v[i] = (ninf+(w<0))%2 ? R_NegInf : R_PosInf;    \
      }                                                          \
    }                                                            \
  } else if (nc == k) {                                          \
    ans->dbl_v[i] = narm ? 1.0 : NA_REAL;                        \
  } else {                                                       \
    if (narm) {                                                  \
      if (pinf == 0 && ninf == 0) {                              \
        if (zerc) {                                              \
          ans->dbl_v[i] = 0.0;                                   \
        } else {                                                 \
          ans->dbl_v[i] = (double) w;                            \
        }                                                        \
      } else {                                                   \
        if (zerc) {                                              \
          ans->dbl_v[i] = R_NaN;                                 \
        } else {                                                 \
          ans->dbl_v[i] = (ninf+(w<0))%2 ? R_NegInf : R_PosInf;  \
        }                                                        \
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
  int zerc = 0;
  bool truehasnf = hasnf>0;
  if (!truehasnf) {
    int i;
    for (i=0; i<k-1; i++) { // #loop_counter_not_local_scope_ok
      if (x[i] == 0.0) {
        zerc++;
      } else {
        w *= x[i];
      }
      ans->dbl_v[i] = fill;
    }
    if (x[i] == 0.0) {
      zerc++;
    } else {
      w *= x[i];
    }
    if (zerc) {
      ans->dbl_v[i] = 0.0;
    } else {
      ans->dbl_v[i] = (double) w;
    }
    if (R_FINITE((double) w)) {
      for (uint64_t i=k; i<nx; i++) {
        if (x[i-k] == 0.0) {
          zerc--;
        } else {
          w /= x[i-k];
        }
        if (x[i] == 0.0) {
          zerc++;
        } else {
          w *= x[i];
        }
        if (zerc) {
          ans->dbl_v[i] = 0.0;
        } else {
          ans->dbl_v[i] = (double) w;
        }
      }
      if (!R_FINITE((double) w)) {
        if (hasnf==-1)
          ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
        if (verbose)
          ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
        w = 1.0; zerc = 0; truehasnf = true;
      }
    } else {
      if (hasnf==-1)
        ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
      if (verbose)
        ansSetMsg(ans, 0, "%s: non-finite values are present in input, skip non-finite inaware attempt and run with extra care for NFs straighaway\n", __func__);
      w = 1.0; zerc = 0; truehasnf = true;
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
    #pragma omp parallel for num_threads(getDTthreads(nx, true)) shared(truehasnf)
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
        #pragma omp atomic write
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

/* fast rolling var - fast
  Welford's online algorithm
  numerically stable
  no support for NFs, redirecting to exact
  Welford wmean and m2 would have to be recalculated on each NF element
 */
void frollvarFast(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose, bool par) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollvarFast", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0 || k == 1) { // var(scalar) is also NA
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size %d, returning all NA vector\n"), __func__, k);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = NA_REAL;
    }
    return;
  }
  long double wmean = 0.0;
  long double m2 = 0.0;
  int k0 = k-1;
  bool truehasnf = hasnf>0;
  if (!truehasnf) {
    int i;
    for (i=0; i<k-1; i++) { // #loop_counter_not_local_scope_ok
      long double delta = x[i] - wmean;
      wmean += delta / (i + 1);
      m2 += delta * (x[i] - wmean);
      ans->dbl_v[i] = fill;
    }
    long double delta = x[i] - wmean; // i==k-1
    wmean += delta / (i + 1);
    m2 += delta * (x[i] - wmean);
    double ans_i = m2 / k0;
    ans->dbl_v[i] = MAX(0,ans_i);
    if (R_FINITE((double) m2)) {
      for (uint64_t i=k; i<nx; i++) {
          double x_out = x[i-k];
          long double delta_out = x_out - wmean;
          wmean = (wmean * k - x_out) / k0;
          m2 -= delta_out * (x_out - wmean);
          double x_in = x[i];
          long double delta_in = x_in - wmean;
          wmean += delta_in / k;
          m2 += delta_in * (x_in - wmean);
          double ans_i = m2 / k0;
          ans->dbl_v[i] = MAX(0,ans_i);
      }
      if (!R_FINITE((double) m2)) {
        if (hasnf==-1)
          ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
        if (verbose)
          ansSetMsg(ans, 0, "%s: non-finite values are present in input, re-running with extra care for NFs\n", __func__);
        /*wmean = 0.0; m2 = 0.0; */truehasnf = true;
      }
    } else {
      if (hasnf==-1)
        ansSetMsg(ans, 2, "%s: has.nf=FALSE used but non-finite values are present in input, use default has.nf=NA to avoid this warning", __func__);
      if (verbose)
        ansSetMsg(ans, 0, "%s: non-finite values are present in input, skip non-finite inaware attempt and run with extra care for NFs straighaway\n", __func__);
      /*wmean = 0.0; m2 = 0.0; */truehasnf = true;
    }
  }
  if (truehasnf) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: non-finite values are present in input, redirecting to frollvarExact using has.nf=TRUE\n"), __func__);
    frollvarExact(x, nx, ans, k, fill, narm, /*hasnf=*/true, verbose, par);
    return;
  }
}

/* fast rolling var - exact
 */
void frollvarExact(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose, bool par) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running %s for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollvarExact", par ? "in parallel" : "sequentially, because outer parallelism has been used,", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0 || k == 1) { // var(scalar) is also NA
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size %d, returning all NA vector\n"), __func__, k);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = NA_REAL;
    }
    return;
  }
  for (int i=0; i<k-1; i++) {
    ans->dbl_v[i] = fill;
  }
  bool truehasnf = hasnf>0;
  if (!truehasnf || !narm) {
    #pragma omp parallel for if (par) num_threads(getDTthreads(nx, true)) shared(truehasnf)
    for (uint64_t i=k-1; i<nx; i++) {
      if (narm && truehasnf) {
        continue;
      }
      long double wsum = 0.0;
      for (int j=-k+1; j<=0; j++) {
        wsum += x[i+j];
      }
      if (!R_FINITE((double) wsum)) {
        if (ISNAN((double) wsum)) {
          if (!narm) {
            ans->dbl_v[i] = (double) wsum; // propagate NAs
          }
          #pragma omp atomic write
          truehasnf = true;
        } else {
          ans->dbl_v[i] = R_NaN;
        }
      } else {
        long double wmean = wsum / k;
        long double xi = 0.0;
        long double wsumxi = 0;
        for (int j=-k+1; j<=0; j++) {
          xi = x[i+j] - wmean;
          wsumxi += (xi * xi);
        }
        double ans_i = wsumxi / (k - 1);
        ans->dbl_v[i] = MAX(0,ans_i);
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
    #pragma omp parallel for if (par) num_threads(getDTthreads(nx, true))
    for (uint64_t i=k-1; i<nx; i++) {
      long double wsum = 0.0;
      int nc = 0;
      for (int j=-k+1; j<=0; j++) {
        if (ISNAN(x[i+j])) {
          nc++;
        } else {
          wsum += x[i+j];
        }
      }
      if (R_FINITE((double) wsum)) {
        if (nc == 0) {
          long double wmean = wsum / k;
          long double xi = 0.0;
          long double wsumxi = 0;
          for (int j=-k+1; j<=0; j++) {
            xi = x[i+j] - wmean;
            wsumxi += (xi * xi);
          }
          double ans_i = wsumxi / (k - 1);
          ans->dbl_v[i] = MAX(0,ans_i);
        } else if (nc < (k - 1)) { // var(scalar) is also NA thats why k-1 so at least 2 numbers must be there
          long double wmean = wsum / (k - nc);
          long double xi = 0.0;
          long double wsumxi = 0;
          for (int j=-k+1; j<=0; j++) {
            if (!ISNAN(x[i+j])) {
              xi = x[i+j] - wmean;
              wsumxi += (xi * xi);
            }
          }
          double ans_i = wsumxi / (k - nc - 1);
          ans->dbl_v[i] = MAX(0,ans_i);
        } else {
          ans->dbl_v[i] = NA_REAL;
        }
      } else {
        ans->dbl_v[i] = (double) R_NaN;
      }
    }
  }
}

/* fast rolling sd - fast
 */
void frollsdFast(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose, bool par) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: calling sqrt(frollvarFast(...))\n"), "frollsdFast");
  frollvarFast(x, nx, ans, k, fill, narm, hasnf, verbose, par);
  for (uint64_t i=k-1; i<nx; i++) {
    ans->dbl_v[i] = sqrt(ans->dbl_v[i]);
  }
}

/* fast rolling sd - exact
 */
void frollsdExact(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: calling sqrt(frollvarExact(...))\n"), "frollsdExact");
  frollvarExact(x, nx, ans, k, fill, narm, hasnf, verbose, /*par=*/true); // par=true because frollsdExact at this place was invoked directly, and not by fallback, so algo=exact have been used explicitly, then outer parallelism in frollR.c is disabled already. If it would be algo=fast then sdFast -> varFast -> NAs -> varExact, so sdExact is no emplyed in the process, nothing redirects to sdExact
  for (uint64_t i=k-1; i<nx; i++) {
    ans->dbl_v[i] = sqrt(ans->dbl_v[i]);
  }
}

/* fast rolling median - fast - helper functions/macros
 * algo="fast" is a novel sort-median algorithm
 * names suffixed with "2" are corresponding versions that support even k window size, as opposed to the algorithm described in the original paper
 */
 
#ifdef MIN
#undef MIN
#endif
#define MIN(a,b) (((a)<(b))?(a):(b))
// initialize prev and next pointers for list L
static void setlinks(const int *o, int *next, int *prev, int tail) {
  int p = tail;
  int k = tail;
  int q;
  for (int i=0; i<k; i++) {
    q = o[i];
    next[p] = q;
    prev[q] = p;
    p = q;
  }
  next[p] = tail;
  prev[tail] = p;
}
// Return the first large element in block, or Inf if all elements are small
inline static double peek(const double *x, int m, int tail) {
  return m == tail ? R_PosInf : x[m];
}
#define PEEK(j) peek(&x[(j)*k], m[(j)], tail)
// return median, for even k, from x_A[m_a], x_A[n_A], x_B[m_B], x_B[n_B] - having two sets of sorted 2 elements, take two smallest and give their mean
#define MED(A, B) MIN(PEEK(A), PEEK(B))
// delete all elements from B in reverse order
static void unwind(int *prev, int *next, int *m, int *s, int k, int tail) {
   for (int i=k-1; i>=0; i--) {
     next[prev[i]] = next[i];
     prev[next[i]] = prev[i];
   }
   m[0] = tail;
   s[0] = 0;
 }
#define UNWIND(j) unwind(&prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &s[(j)], k, tail);
// checks if object i is "small" - smaller than value(s) used for median answer
inline static bool small(int i, const double *x, int m, int tail) {
  return (m == tail) || (x[i] < x[m]) || ((x[i] == x[m]) && (i < m));
}
#define SMALL(i) small((i), x, m[0], tail)
// delete element from A
static void delete(int i, const double *x, int *prev, int *next, int *m, int *s, int tail) {
  next[prev[i]] = next[i];
  prev[next[i]] = prev[i];
  if (SMALL(i)) {
    s[0]--;
  } else {
    if (m[0] == i) {
      m[0] = next[m[0]];
    }
    if (s[0] > 0) {
      m[0] = prev[m[0]];
      s[0]--;
    }
  }
}
#define DELETE(j) delete(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &s[(j)], tail)
// undelete element from B
static void undelete(int i, const double *x, int *prev, int *next, int *m, int tail) {
  next[prev[i]] = i;
  prev[next[i]] = i;
  if (SMALL(i)) {
    m[0] = prev[m[0]];
  }
}
#define UNDELETE(j) undelete(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], tail)
// advance - balance block after delete/undelete, fixed m (and n) pointers and s counter
static void advance(int *next, int* m, int *s) {
  m[0] = next[m[0]];
  s[0]++;
}
#define ADVANCE(j) advance(&next[(j)*(k+1)], &m[(j)], &s[(j)])
// same helper functions supporting any k
#define PEEK2(j) peek(&x[(j)*k], n[(j)], tail)
static double med2(const double *xa, const double *xb, int ma, int na, int mb, int nb, int tail) {
  double xam = ma == tail ? R_PosInf : xa[ma];
  double xan = na == tail ? R_PosInf : xa[na];
  double xbm = mb == tail ? R_PosInf : xb[mb];
  double xbn = nb == tail ? R_PosInf : xb[nb];
  if (xam == xbm) {
    return xam;
  } else if (xam < xbm) { // so xam < xbn
    if (xan <= xbm) { // so xan <= xbn
      return (xam + xan) / 2;
    } else {
      return (xam + xbm) / 2;
    }
  } else { // xbm < xam
    if (xbn <= xam) {
      return (xbm + xbn) / 2;
    } else {
      return (xbm + xam) / 2;
    }
  }
}
#define MED2(A, B) med2(&x[(A)*k], &x[(B)*k], m[(A)], n[(A)], m[(B)], n[(B)], tail)
static void unwind2(int *prev, int *next, int *m, int *n, int *s, int k, int tail, bool even) {
  for (int i=k-1; i>=0; i--) {
    next[prev[i]] = next[i];
    prev[next[i]] = prev[i];
  }
  m[0] = tail;
  if (even)
    n[0] = tail;
  s[0] = 0;
}
#define UNWIND2(j) unwind2(&prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &n[(j)], &s[(j)], k, tail, even);
#define SMALL2(i) small((i), x, n[0], tail)
static void delete2(int i, const double *x, int *prev, int *next, int *m, int *n, int *s, int tail, bool even) {
  next[prev[i]] = next[i];
  prev[next[i]] = prev[i];
  if (SMALL(i)) {
    s[0]--;
  } else {
    if (m[0] == i) {
      m[0] = next[m[0]];
      if (even)
        n[0] = next[n[0]];
    } else if (even && n[0] == i) {
      n[0] = next[n[0]];
    }
    if (s[0] > 0) {
      m[0] = prev[m[0]];
      if (even)
        n[0] = prev[n[0]];
      s[0]--;
    }
  }
}
#define DELETE2(j) delete2(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &n[(j)], &s[(j)], tail, even)
static void undelete2(int i, const double *x, int *prev, int *next, int *m, int *n, int tail, bool even) {
  next[prev[i]] = i;
  prev[next[i]] = i;
  if (SMALL(i)) {
    m[0] = prev[m[0]];
    if (even)
      n[0] = prev[n[0]];
  } else if (even && SMALL2(i)) {
    n[0] = prev[n[0]];
  }
}
#define UNDELETE2(j) undelete2(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &n[(j)], tail, even)
static void advance2(int *next, int* m, int* n, int *s, bool even) {
  m[0] = next[m[0]];
  if (even)
    n[0] = next[n[0]];
  s[0]++;
}
#define ADVANCE2(j) advance2(&next[(j)*(k+1)], &m[(j)], &n[(j)], &s[(j)], even)

/* fast rolling median - fast
 * sort median: https://arxiv.org/pdf/1406.1717.pdf
 * extended for arbitrary nx and for even k
 * finding order of blocks of input is made in parallel
 * NA handling redirected to algo="exact"
 */
void frollmedianFast(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose, bool par) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollmedianFast", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all NA vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = NA_REAL;
    }
    return;
  }
  if (k == 1 || k == 2) { // special case for k==1 and k==2, wont rise warning for NAs present and hasnf=false
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size %d, skip median and use simple loop\n"), "frollmedianFast", k);
    if (k == 1) {
      for (int i=0; i<nx; i++) // na.rm included
        ans->dbl_v[i] = x[i];
    } else if (k == 2) {
      ans->dbl_v[0] = fill;
      if (narm) {
        for (int i=1; i<nx; i++) {
          if (ISNAN(x[i])) {
            if (ISNAN(x[i-1])) {
              ans->dbl_v[i] = NA_REAL;
            } else {
              ans->dbl_v[i] = x[i-1];
            }
          } else {
            if (ISNAN(x[i-1])) {
              ans->dbl_v[i] = x[i];
            } else {
              ans->dbl_v[i] = (x[i] + x[i-1]) / 2;
            }
          }
        }
      } else {
        for (int i=1; i<nx; i++)
          ans->dbl_v[i] = (x[i] + x[i-1]) / 2;
      }
    }
    return;
  }
  bool hasna = false;
  if (hasnf>=0) {
    for (uint64_t i=0; i<nx; i++) {
      if (ISNAN(x[i])) {
        hasna = true;
        break;
      }
    }
    if (hasna) {
      if (verbose)
        snprintf(end(ans->message[0]), 500, _("%s: NAs detected, fall back to frollmedianExact\n"), "frollmedianFast");
      frollmedianExact(x, nx, ans, k, fill, narm, /*hasnf=*/true, verbose);
      return;
    }
  }
  double tic = 0;
  double *ansv = ans->dbl_v;
  // handling of nx not multiple of k
  int nx_mod_k = nx % k;
  int onx = nx;
  if (nx_mod_k) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: nx=%"PRIu64" is not multiple of k=%d, padding with %d elements, new nx=%"PRIu64"\n"), "frollmedianFast", nx, k, k-nx_mod_k, nx+(k-nx_mod_k));
    nx = nx + (k-nx_mod_k);
  }
  // number of blocks, even k, h number of small elements in list
  int b = nx/k;
  bool even = !(k % 2);
  int h = even ? k/2-1 : (k-1)/2;
  int tail = k;
  // allocation as long arrays rather than block structs
  int *o = malloc(sizeof(*o) * nx); // permutation that sorts input vector x
  if (!o) { // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for o", __func__); // raise error
    return;
  } // # nocov end
  if (nx_mod_k) { // initialize padded elements, otherwise setlinks could crash
    for (int i=0; i<k-nx_mod_k; i++)
      o[onx+i] = nx_mod_k+i;
  }
  int *m = malloc(sizeof(*m) * b); // pointer to median candidate in that L, initialized to first large element in L
  if (!m) { // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for m", __func__); // raise error
    free(o);
    return;
  } // # nocov end
  int *n = malloc(sizeof(*n) * b); // pointer to median candidate of a pair mid values in that L for even k, initialized to second large element
  if (!n) { // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for n", __func__); // raise error
    free(m); free(o);
    return;
  } // # nocov end
  int *s = malloc(sizeof(*s) * b); // counter between 0 and k, number of 'small' elements in list L
  if (!s) { // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for s", __func__); // raise error
    free(n); free(m); free(o);
    return;
  } // # nocov end
  int *prev = malloc(sizeof(*prev) * b * (k+1)); // pointer to previous element in list L
  if (!prev) { // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for prev", __func__); // raise error
    free(s); free(n); free(m); free(o);
    return;
  } // # nocov end
  int *next = malloc(sizeof(*next) * b * (k+1)); // pointer to next element in list L
  if (!next) { // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for next", __func__); // raise error
    free(prev); free(s); free(n); free(m); free(o);
    return;
  } // # nocov end
  // find ordering permutation for each block
  if (verbose)
    tic = omp_get_wtime();
  #pragma omp parallel for if (par) num_threads(getDTthreads(b, false))
  for (int j=0; j<b; j++) { // par ensures no nested parallelism so if input is not already vectorized (and therefore parallel) then we can use omp here
    shellsort(&x[j*k], (nx_mod_k && j==b-1) ? nx_mod_k : k, &o[j*k]);
    m[j] = o[j*k+h];
    if (even)
      n[j] = o[j*k+h+1];
    s[j] = h;
    setlinks(&o[j*k], &next[j*(k+1)], &prev[j*(k+1)], tail);
  }
  if (verbose)
    snprintf(end(ans->message[0]), 500, par ? _("%s: finding order and initializing links for %d blocks in parallel took %.3fs\n")
                                            : _("%s: finding order and initializing links for %d blocks sequentially took %.3fs\n"), "frollmedianFast", b, omp_get_wtime()-tic);
  // fill leading partial window
  for (int i=0; i<k-1; i++) {
    ansv[i] = fill;
  }
  // main rolling loop - called post processing in the paper
  if (verbose)
    tic = omp_get_wtime();
  if (!nx_mod_k && !even) { // see verbose message below for explanation
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: running implementation as described in the paper by Jukka Suomela, for uneven window size, length of input a multiple of window size, no NAs in the input data\n"), "frollmedianFast");
    ansv[k-1] = PEEK(0);
    for (int j=1; j<b; j++) {
      int A = j-1, B = j;
      UNWIND(B);
      /*stopifnot*/ /*if (!(s[A] == h)) {
        snprintf(end(ans->message[3]), 500, _("%s: 's[A] == h' is not true\n"), "frollmedianFast");
        return;
      }*/
      /*stopifnot*/ /*if (!(s[B] == 0)) {
        snprintf(end(ans->message[3]), 500, _("%s: 's[B] == 0' is not true\n"), "frollmedianFast");
        return;
      }*/
      for (int i=0; i<k; i++) {
        DELETE(A);
        UNDELETE(B);
        /*stopifnot*/ /*if (!(s[A] + s[B] <= h)) {
          snprintf(end(ans->message[3]), 500, _("%s: 's[A] + s[B] <= h' is not true\n"), "frollmedianFast");
          return;
        }*/
        if (s[A] + s[B] < h) {
          if (PEEK(A) <= PEEK(B)) {
            ADVANCE(A);
          } else {
            ADVANCE(B);
          }
        }
        /*stopifnot*/ /*if (!(s[A] + s[B] == h)) {
          snprintf(end(ans->message[3]), 500, _("%s: 's[A] + s[B] == h' is not true\n"), "frollmedianFast");
          return;
        }*/
        ansv[j*k+i] = MED(A, B);
      }
    }
  } else {
    ansv[k-1] = even ? (PEEK(0) + PEEK2(0)) / 2 : PEEK(0);
    for (int j=1; j<b; j++) {
      int A = j-1, B = j;
      UNWIND2(B);
      /*stopifnot*/ /*if (!(s[A] == h)) {
        snprintf(end(ans->message[3]), 500, _("%s: 's[A] == h' is not true\n"), "frollmedianFast");
        return;
      }*/
      /*stopifnot*/ /*if (!(s[B] == 0)) {
        snprintf(end(ans->message[3]), 500, _("%s: 's[B] == 0' is not true\n"), "frollmedianFast");
        return;
      }*/
      for (int i=0; i<k; i++) {
        if (nx_mod_k && j == b-1) {
          if (verbose && i == nx_mod_k)
            snprintf(end(ans->message[0]), 500, _("%s: skip rolling for %d padded elements\n"), "frollmedianFast", k-nx_mod_k);
          if (i >= nx_mod_k)
            continue;
        }
        DELETE2(A);
        UNDELETE2(B);
        /*stopifnot*/ /*if (!(s[A] + s[B] <= h)) {
          snprintf(end(ans->message[3]), 500, _("%s: 's[A] + s[B] <= h' is not true\n"), "frollmedianFast");
          return;
        }*/
        if (s[A] + s[B] < h) {
          if (PEEK(A) <= PEEK(B)) {
            ADVANCE2(A);
          } else {
            ADVANCE2(B);
          }
        }
        /*stopifnot*/ /*if (!(s[A] + s[B] == h)) {
          snprintf(end(ans->message[3]), 500, _("%s: 's[A] + s[B] == h' is not true\n"), "frollmedianFast");
          return;
        }*/
        if (n[A]!=tail && m[A] == n[A]) {
          n[A] = tail;
        }
        if (n[B]!=tail && m[B] == n[B]) {
          n[B] = tail;
        }
        ansv[j*k+i] = even ? MED2(A, B) : MED(A, B);
      }
    }
  }
  free(next); free(prev); free(s); free(n); free(m); free(o);
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: rolling took %.3f\n"), "frollmedianFast", omp_get_wtime()-tic);
}

/* fast rolling NA stats - fast
 * used in frollmedianExact
 * returns NC total number of NA/NaN in x
 * updates:
 *   nc for rolling NA count
 *   isna for NA mask
 * isna must be initialized to false
 * nc doesn't have to be initialized
 */
static int frollNAFast(const double *x, uint64_t nx, int k, int *nc, bool *isna) {
  int w = 0; // rolling nc
  for (int i=0; i<k-1; i++) {
    if (ISNAN(x[i])) {
      isna[i] = true;
      w++;
    }
    nc[i] = w;
  }
  if (ISNAN(x[k-1])) {
    isna[k-1] = true;
    w++;
  }
  nc[k-1] = w;
  int NC = w;
  for (uint64_t i=k; i<nx; i++) {
    if (ISNAN(x[i-k]))
      w--;
    if (ISNAN(x[i])) {
      isna[i] = true;
      w++;
      NC++;
    }
    nc[i] = w;
  }
  return NC;
}

/* fast rolling median - exact
 * loop in parallel for each element of x and call quickselect
 */
void frollmedianExact(const double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasnf, bool verbose) {
  if (verbose)
    snprintf(end(ans->message[0]), 500, _("%s: running in parallel for input length %"PRIu64", window %d, hasnf %d, narm %d\n"), "frollmedianExact", (uint64_t)nx, k, hasnf, (int)narm);
  if (k == 0) {
    if (verbose)
      snprintf(end(ans->message[0]), 500, _("%s: window width of size 0, returning all NA vector\n"), __func__);
    for (uint64_t i=0; i<nx; i++) {
      ans->dbl_v[i] = NA_REAL;
    }
    return;
  }
  for (int i=0; i<k-1; i++) {
    ans->dbl_v[i] = fill;
  } // fill leading partial window
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
  int nth = getDTthreads(nx-k+1, true);
  double *xx = malloc(sizeof(*xx) * k * nth); // quickselect sorts in-place so we need to copy
  if (!xx) { // # nocov start
    ansSetMsg(ans, 3, "%s: Unable to allocate memory for xx", __func__); // raise error
    return;
  } // # nocov end
  if (hasnf == -1) {
    #pragma omp parallel for num_threads(nth)
    for (uint64_t i=k-1; i<nx; i++) {
      int th = omp_get_thread_num();
      double *thx = &xx[th*k]; // thread-specific x
      memcpy(thx, &x[i-k+1], k * sizeof(double));
      ans->dbl_v[i] = dquickselect(thx, k);
    }
  } else {
    int *rollnc = malloc(sizeof(*rollnc) * nx);
    if (!rollnc) { // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for rollnc", __func__); // raise error
      free(xx);
      return;
    } // # nocov end
    bool *isna = calloc(nx, sizeof(*isna)); // note that calloc signature (2 args) differs from malloc (1 arg)
    if (!isna) { // # nocov start
      ansSetMsg(ans, 3, "%s: Unable to allocate memory for isna", __func__); // raise error
      free(rollnc); free(xx);
      return;
    } // # nocov end
    int nc = frollNAFast(x, nx, k, rollnc, isna);
    if (!nc) { // total NA for x
      if (verbose)
        snprintf(end(ans->message[0]), 500, _("%s: no NAs detected, redirecting to itself using has.nf=FALSE\n"), "frollmedianExact");
      free(isna); free(rollnc); free(xx);
      frollmedianExact(x, nx, ans, k, fill, narm, /*hasnf=*/false, verbose);
      return;
    }
    #pragma omp parallel for num_threads(nth)
    for (uint64_t i=k-1; i<nx; i++) {
      const double *ix = &x[i-k+1];
      int th = omp_get_thread_num();
      double *thx = &xx[th*k];
      int inc = rollnc[i];
      if (!inc) {
        memcpy(thx, ix, k * sizeof(*thx));
        ans->dbl_v[i] = dquickselect(thx, k);
      } else {
        if (!narm || inc == k) {
          ans->dbl_v[i] = NA_REAL;
        } else {
          bool *iisna = &isna[i-k+1];
          int thxn = 0;
          for (int j=0; j<k; j++) {
            if (!iisna[j])
              thx[thxn++] = ix[j];
          }
          ans->dbl_v[i] = dquickselect(thx, k-inc);
        }
      }
    }
    free(isna); free(rollnc);
  }
  free(xx);
}
