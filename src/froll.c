#include "froll.h"

/* fast rolling functions
 * adaptive=F: window width is constant value
 * 
 * TODO:
 * consider turning align branch: if (align < 1) { ... } into macro or inline function
 * benchmark alt C impl exact=F: `tmp<-cumsum(x); (tmp-shift(tmp, k))/k` # if faster than current then add to api
 * exact=T measure speed penaulty of volatile truehasna `if (narm && truehasna) continue`, maybe better is to let faster loop finish and re-run after
 * consider support error handling, even non-stop.
 */

/* wrapper over different rolling mean algos
 * algo = 0:
 *   adding/removing in/out of sliding window of observations
 * algo = 1:
 *   recalculate whole mean for each observation, roundoff correction is adjusted, also support for NaN and Inf
 * also handles 'align' in single place
 */

void frollmean(unsigned int algo, double *x, uint_fast64_t nx, double *ans, int k, int align, double fill, bool narm, int hasna, bool verbose) {
  if (algo==0) frollmeanFast(x, nx, ans, k, fill, narm, hasna, verbose);
  else if (algo==1) frollmeanExact(x, nx, ans, k, fill, narm, hasna, verbose);
  // align
  if (align < 1) {                                              // align center or left
    int k_ = align==-1 ? k-1 : floor(k/2);                      // offset to shift
    if (verbose) Rprintf("%s: align %d, shift answer by %d\n", __func__, align, -k_);
    memmove((char *)ans, (char *)ans + (k_*sizeof(double)), (nx-k_)*sizeof(double)); // apply shift to achieve expected align
    for (uint_fast64_t i=nx-k_; i<nx; i++) ans[i] = fill;       // fill from right side
  }
}

/* fast fast rolling mean
 * if window bigger than input length then skip processing and return NA vector
 * when no info on NA (hasNA argument) then assume no NAs run faster version
 * rollmean implemented as sliding window for align="right"
 * if NAs detected re-run rollmean implemented as sliding window with NA support
 */

void frollmeanFast(double *x, uint_fast64_t nx, double *ans, int k, double fill, bool narm, int hasna, bool verbose) {
  if (nx < k) {                                                 // if window width bigger than input just return vector of fill values
    if (verbose) Rprintf("%s: window width longer than input vector, returning all NA vector\n", __func__);
    for (int i=0; i<nx; i++) ans[i] = fill;
    return;
  }
  if (verbose) Rprintf("%s: running for input length %lu, window %d, hasna %d, narm %d\n", __func__, nx, k, hasna, (int) narm);
  long double w = 0.;                                           // sliding window aggregate
  bool truehasna = hasna>0;                                     // flag to re-run with NA support if NAs detected
  if (!truehasna) {
    int i;                                                      // iterator declared here because it is being used after foor loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window
      w += x[i];                                                // add current row to sliding window
      ans[i] = fill;                                            // answers are fill for partial window
    }
    w += x[i];                                                  // i==k-1
    ans[i] = (double) (w / k);                                  // first full sliding window, non-fill rollfun answer
    for (uint_fast64_t i=k; i<nx; i++) {                        // loop over obs, complete window
      w -= x[i-k];                                              // remove leaving row from sliding window
      w += x[i];                                                // add current row to sliding window
      ans[i] = (double) (w / k);                                // rollfun to answer vector
    }
    if (!R_FINITE((double) w)) {
      if (verbose) {
        if (hasna==-1) Rprintf("%s: hasNA FALSE was used but NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n", __func__);
        else Rprintf("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n", __func__);
      }
      w = 0.;
      truehasna = 1;
    }
  }
  if (truehasna) {
    int nc = 0;                                                 // NA counter within sliding window
    int i;                                                      // iterator declared here because it is being used after foor loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window
      if (R_FINITE(x[i])) w += x[i];                            // add only finite values to window aggregate
      else nc++;                                                // increment NA count in current window
      ans[i] = fill;                                            // partial window fill all
    }
    if (R_FINITE(x[i])) w += x[i];                              // i==k-1
    else nc++;
    if (nc == 0) ans[i] = (double) (w / k);                     // no NAs in first full window
    else if (nc == k) ans[i] = narm ? R_NaN : NA_REAL;          // all values in sliding window are NA, expected output for fun(NA, na.rm=T/F)
    else ans[i] = narm ? (double) (w / (k - nc)) : NA_REAL;     // some values in window are NA
    for (uint_fast64_t i=k; i<nx; i++) {                        // loop over obs, complete window
      if (R_FINITE(x[i])) w += x[i];                            // add only finite to window aggregate
      else nc++;                                                // increment NA count in current window
      if (R_FINITE(x[i-k])) w -= x[i-k];                        // remove only finite from window aggregate
      else nc--;                                                // decrement NA count in current window
      if (nc == 0) ans[i] = (double) (w / k);                   // no NAs in sliding window for present observation
      else if (nc == k) ans[i] = narm ? R_NaN : NA_REAL;        // all values in window are NA, expected output for fun(NA, na.rm=T/F)
      else ans[i] = narm ? (double) (w / (k - nc)) : NA_REAL;   // some values in window are NA
    }
  }
}

/* fast exact rolling mean
 * if window bigger than input length then skip processing and return NA vector
 * when no info on NA (hasNA argument) then assume no NAs run faster version, also when na.rm=FALSE faster version can proceed
 * rollmean implemented as mean for each observation for align="right"
 * if NAs detected and na.rm=TRUE then re-run rollmean implemented as mean for each observation with NA support
 */

void frollmeanExact(double *x, uint_fast64_t nx, double *ans, int k, double fill, bool narm, int hasna, bool verbose) {
  if (nx < k) {                                                 // if window width bigger than input just return vector of fill values
    if (verbose) Rprintf("%s: window width longer than input vector, returning all NA vector\n", __func__);
    for (int i=0; i<nx; i++) ans[i] = fill;
    return;
  }
  if (verbose) Rprintf("%s: running for input length %lu, window %d, hasna %d, narm %d\n", __func__, nx, k, hasna, (int) narm);
  volatile bool truehasna = hasna>0;                            // flag to re-run with NA support if NAs detected, volatile to be used from parallel region
  if (!truehasna || !narm) {
    for (int i=0; i<k-1; i++) {                                 // fill partial window
      ans[i] = fill;
    }
    #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx)) shared(truehasna)
    {
      #pragma omp for schedule(static)
      for (uint_fast64_t i=k-1; i<nx; i++) {                    // loop on every observation
        if (narm && truehasna) continue;                        // if NAs detected no point to continue
        long double w = 0.0;
        for (int j=-k+1; j<=0; j++) {                           // sub-loop on window width
          w += x[i+j];                                          // sum of window for particular observation
        }
        if (R_FINITE((double) w)) {                             // no need to calc roundoff correction if NAs detected as will re-call all below in truehasna==1
          long double res = w / k;                              // keep results as long double for intermediate processing
          long double err = 0.0;                                // roundoff corrector
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            err += x[i+j] - res;                                // measure difference of obs in sub-loop to calculated fun for obs
          }
          ans[i] = (double) (res + (err / k));                  // adjust calculated rollfun with roundoff correction
        } else {
          if (!narm) ans[i] = (double) (w / k);                 // NAs should be propagated
          truehasna = 1;                                        // NAs detected for this window, set flag so rest of windows will not be re-run
        }
      }
    } // end of parallel region
    if (truehasna && verbose) {
      if (narm) {
        if (hasna==-1) Rprintf("%s: hasNA FALSE was used but NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n", __func__);
        else Rprintf("%s: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs\n", __func__);
      } else {
        if (hasna==-1) Rprintf("%s: hasNA FALSE was used but NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run\n", __func__);
        else Rprintf("%s: NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run\n", __func__);
      }
    }
  }
  if (truehasna && narm) {
    for (int i=0; i<k-1; i++) {                                 // fill partial window
      ans[i] = fill;
    }
    #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx))
    {
      #pragma omp for schedule(static)
      for (uint_fast64_t i=k-1; i<nx; i++) {                    // loop on every observation
        long double w = 0.0;
        int nc = 0;                                             // NA counter within sliding window
        for (int j=-k+1; j<=0; j++) {                           // sub-loop on window width
          if (ISNAN(x[i+j])) nc++;                              // increment NA count in current window
          else w += x[i+j];                                     // add observation to current window
        }
        long double res = w / k;                                // keep results as long double for intermediate processing
        long double err = 0.0;                                  // roundoff corrector
        if (nc == 0) {                                          // no NAs in current window
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            err += x[i+j] - res;                                // measure roundoff for each obs in window
          }
          ans[i] = (double) (res + (err / k));                  // adjust calculated fun with roundoff correction
        } else if (nc < k) {
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            if (!ISNAN(x[i+j])) err += x[i+j] - res;            // measure roundoff for each non-NA obs in window
          }
          ans[i] = (double) (res + (err / (k - nc)));           // adjust calculated fun with roundoff correction
        } else {                                                // nc == k
          ans[i] = R_NaN;                                       // all values NAs and narm so produce expected values
        }
      }
    } // end of parallel region
  }
}
