#include "froll.h"

/* fast rolling functions */

/* fast rolling mean - router
 * early stopping for window bigger than input
 * also handles 'align' in single place
 * algo = 0: frollmeanFast
 *   adding/removing in/out of sliding window of observations
 * algo = 1: frollmeanExact
 *   recalculate whole mean for each observation, roundoff correction is adjusted, also support for NaN and Inf
 */

void frollmean(unsigned int algo, double *x, uint_fast64_t nx, double_ans_t *ans, int k, int align, double fill, bool narm, int hasna, bool verbose) {
  ans->status = 0;                                              // default status success
  if (nx < k) {                                                 // if window width bigger than input just return vector of fill values
    if (verbose) Rprintf("%s: window width longer than input vector, returning all NA vector\n", __func__);
    for (int i=0; i<nx; i++) ans->ans[i] = fill;
    return;
  }
  if (algo==0) frollmeanFast(x, nx, ans, k, fill, narm, hasna, verbose);
  else if (algo==1) frollmeanExact(x, nx, ans, k, fill, narm, hasna, verbose);
  // align
  if (ans->status < 3 && align < 1) {                           // align center or left, only when no errors occurred
    int k_ = align==-1 ? k-1 : floor(k/2);                      // offset to shift
    if (verbose) Rprintf("%s: align %d, shift answer by %d\n", __func__, align, -k_);
    memmove((char *)ans->ans, (char *)ans->ans + (k_*sizeof(double)), (nx-k_)*sizeof(double)); // apply shift to achieve expected align
    for (uint_fast64_t i=nx-k_; i<nx; i++) ans->ans[i] = fill;  // fill from right side
  }
}

/* fast rolling mean - fast
 * when no info on NA (hasNA argument) then assume no NAs run faster version
 * rollmean implemented as single pass sliding window for align="right"
 * if NAs detected re-run rollmean implemented as single pass sliding window with NA support
 */

void frollmeanFast(double *x, uint_fast64_t nx, double_ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose) Rprintf("%s: running for input length %llu, window %d, hasna %d, narm %d\n", __func__, nx, k, hasna, (int) narm);
  long double w = 0.0;                                          // sliding window aggregate
  bool truehasna = hasna>0;                                     // flag to re-run with NA support if NAs detected
  if (!truehasna) {
    int i;                                                      // iterator declared here because it is being used after foor loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window only
      w += x[i];                                                // add current row to sliding window
      ans->ans[i] = fill;                                       // answers are fill for partial window
    }
    w += x[i];                                                  // i==k-1
    ans->ans[i] = (double) (w / k);                             // first full sliding window, non-fill rollfun answer
    if (R_FINITE((double) w)) {                                 // proceed only if no NAs detected in first k obs, otherwise early stopping
      for (uint_fast64_t i=k; i<nx; i++) {                      // loop over obs, complete window, all remaining after partial window
        w -= x[i-k];                                            // remove leaving row from sliding window
        w += x[i];                                              // add current row to sliding window
        ans->ans[i] = (double) (w / k);                         // rollfun to answer vector
      }
      if (!R_FINITE((double) w)) {                              // mark to re-run with NA care
        if (hasna==-1) {                                        // raise warning
          ans->status = 2;
          sprintf(ans->message[2], "%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning", __func__);
        }
        if (verbose) Rprintf("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n", __func__);
        w = 0.0;
        truehasna = 1;
      }
    } else {                                                    // early stopping branch when NAs detected in first k obs
      if (hasna==-1) {                                          // raise warning
        ans->status = 2;
        sprintf(ans->message[2], "%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning", __func__);
      }
      if (verbose) Rprintf("%s: NA (or other non-finite) value(s) are present in input, skip non-NA attempt and run with extra care for NAs\n", __func__);
      w = 0.0;
      truehasna = 1;
    }
  }
  if (truehasna) {
    int nc = 0;                                                 // NA counter within sliding window
    int i;                                                      // iterator declared here because it is being used after foor loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window only
      if (R_FINITE(x[i])) w += x[i];                            // add only finite values to window aggregate
      else nc++;                                                // increment NA count in current window
      ans->ans[i] = fill;                                       // partial window fill all
    }
    if (R_FINITE(x[i])) w += x[i];                              // i==k-1
    else nc++;
    if (nc == 0) ans->ans[i] = (double) (w / k);                // no NAs in first full window
    else if (nc == k) ans->ans[i] = narm ? R_NaN : NA_REAL;     // all values in sliding window are NA, expected output for fun(NA, na.rm=T/F)
    else ans->ans[i] = narm ? (double) (w / (k - nc)) : NA_REAL; // some values in window are NA
    for (uint_fast64_t i=k; i<nx; i++) {                        // loop over obs, complete window, all remaining after partial window
      if (R_FINITE(x[i])) w += x[i];                            // add only finite to window aggregate
      else nc++;                                                // increment NA count in current window
      if (R_FINITE(x[i-k])) w -= x[i-k];                        // remove only finite from window aggregate
      else nc--;                                                // decrement NA count in current window
      if (nc == 0) ans->ans[i] = (double) (w / k);              // no NAs in sliding window for present observation
      else if (nc == k) ans->ans[i] = narm ? R_NaN : NA_REAL;   // all values in window are NA, expected output for fun(NA, na.rm=T/F)
      else ans->ans[i] = narm ? (double) (w / (k - nc)) : NA_REAL; // some values in window are NA
    }
  }
}

/* fast rolling mean - exact
 * when no info on NA (hasNA argument) then assume no NAs run faster version, also when na.rm=FALSE faster version can proceed
 * rollmean implemented as mean of k obs for each observation for align="right"
 * if NAs detected and na.rm=TRUE then re-run rollmean implemented as mean of k bos for each observation with NA support
 */

void frollmeanExact(double *x, uint_fast64_t nx, double_ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose) {
  if (verbose) Rprintf("%s: running for input length %llu, window %d, hasna %d, narm %d\n", __func__, nx, k, hasna, (int) narm);
  for (int i=0; i<k-1; i++) {                                   // fill partial window only
    ans->ans[i] = fill;
  }
  volatile bool truehasna = hasna>0;                            // flag to re-run with NA support if NAs detected, volatile to be used from parallel region
  if (!truehasna || !narm) {
    const int threads = MIN(getDTthreads(), nx);
    #pragma omp parallel num_threads(threads) shared(truehasna)
    {
      #pragma omp for schedule(static)
      for (uint_fast64_t i=k-1; i<nx; i++) {                    // loop on every observation with complete window, partial already filled in single threaded section
        if (narm && truehasna) continue;                        // if NAs detected no point to continue
        long double w = 0.0;
        for (int j=-k+1; j<=0; j++) {                           // sub-loop on window width
          w += x[i+j];                                          // sum of window for particular observation
        }
        if (R_FINITE((double) w)) {                             // no need to calc roundoff correction if NAs detected as will re-call all below in truehasna==1
          long double res = w / k;                              // keep results as long double for intermediate processing
          long double err = 0.0;                                // roundoff corrector
          for (int j=-k+1; j<=0; j++) {                         // nested loop on window width
            err += x[i+j] - res;                                // measure difference of obs in sub-loop to calculated fun for obs
          }
          ans->ans[i] = (double) (res + (err / k));             // adjust calculated rollfun with roundoff correction
        } else {
          if (!narm) ans->ans[i] = (double) (w / k);            // NAs should be propagated
          truehasna = 1;                                        // NAs detected for this window, set flag so rest of windows will not be re-run
        }
      }
    } // end of parallel region
    if (truehasna) {
      if (hasna==-1) {                                          // raise warning
        ans->status = 2;
        sprintf(ans->message[2], "%s: hasNA=FALSE used but NA (or other non-finite) value(s) are present in input, use default hasNA=NA to avoid this warning", __func__);
      }
      if (verbose) {
        if (narm) Rprintf("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n", __func__);
        else Rprintf("%s: NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run\n", __func__);
      }
    }
  }
  if (truehasna && narm) {
    const int threads = MIN(getDTthreads(), nx);
    #pragma omp parallel num_threads(threads)
    {
      #pragma omp for schedule(static)
      for (uint_fast64_t i=k-1; i<nx; i++) {                    // loop on every observation with complete window, partial already filled in single threaded section
        long double w = 0.0;
        int nc = 0;                                             // NA counter within sliding window
        for (int j=-k+1; j<=0; j++) {                           // nested loop on window width
          if (ISNAN(x[i+j])) nc++;                              // increment NA count in current window
          else w += x[i+j];                                     // add observation to current window
        }
        long double res = w / k;                                // keep results as long double for intermediate processing
        long double err = 0.0;                                  // roundoff corrector
        if (nc == 0) {                                          // no NAs in current window
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            err += x[i+j] - res;                                // measure roundoff for each obs in window
          }
          ans->ans[i] = (double) (res + (err / k));             // adjust calculated fun with roundoff correction
        } else if (nc < k) {
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            if (!ISNAN(x[i+j])) err += x[i+j] - res;            // measure roundoff for each non-NA obs in window
          }
          ans->ans[i] = (double) (res + (err / (k - nc)));      // adjust calculated fun with roundoff correction
        } else {                                                // nc == k
          ans->ans[i] = R_NaN;                                  // all values NAs and narm so produce expected values
        }
      }
    } // end of parallel region
  }
}
