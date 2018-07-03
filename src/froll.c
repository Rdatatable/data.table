#include "froll.h"

/* TODO
 * consider turning align branch: if (align < 1) { ... } into macro or inline function
 * benchmark alt C impl exact=F: `tmp<-cumsum(x); (tmp-shift(tmp, k))/k` # if faster than current then add to api
 * exact=T measure speed penaulty of volatile truehasna `if (narm && truehasna) continue`, maybe better is to let faster loop finish and re-run after
 * support error handling, even non-stop, or run exact=T from exact=F when Inf detected to never produce incorrect answer silently
 */

/* fast rolling functions
 * exact=F: sliding window adding/removing in/out of sliding window of observations
 * exact=T: recalculate whole mean for each observation, roundoff correction is adjusted
 * adaptive=F: sliding window is constant value
 * adaptive=T: sliding window is defined for every single observation
 */

/* fast rolling mean
 * if window bigger than input length then skip processing and return NA vector
 * when no info on NA (hasNA argument) then assume no NAs run faster version
 * rollmean implemented as sliding window for align="right"
 * if NAs detected re-run rollmean implemented as sliding window with NA support
 * apply shift if align!="right"
 */

void frollmean(double *x, uint_fast64_t nx, double *ans, int k, int align, double fill, bool narm, int hasna, bool verbose) {
  if (nx < k) {                                                 // if window width bigger than input just return vector of fill values
    if (verbose) Rprintf("frollfun window width longer than input vector, returning all NA vector\n");
    for (int i=0; i<nx; i++) ans[i] = fill;
    return;
  }
  if (verbose) Rprintf("frollfun running for input length %lu, window %d, align %d, hasna %d\n", nx, k, align, hasna);
  long double w = 0.;                                           // sliding window aggregate
  bool truehasna = hasna>0;                                     // flag to re-run with NA support if NAs detected
  if (!truehasna) {
    int i;                                                      // iterator declared here because it is being used after foor loop
    for (i=0; i<k-1; i++) {                                     // loop over leading observation, all partial window
      w += x[i];                                                // add current row to sliding window
      ans[i] = fill;                                            // answers are fill for partial window
    }
    w += x[i];                                                  // i==k-1
    ans[i] = (double) w / k;                                    // first full sliding window, non-fill rollfun answer
    for (uint_fast64_t i=k; i<nx; i++) {                        // loop over obs, complete window
      w -= x[i-k];                                              // remove leaving row from sliding window
      w += x[i];                                                // add current row to sliding window
      ans[i] = (double) w / k;                                  // rollfun to answer vector
    }
    if (!R_FINITE((double) w)) {
      if (verbose) {
        if (hasna==-1) Rprintf("hasNA FALSE was used but NA (or other non-finite) value(s) are present in input, re-running frollfun with extra care for NAs\n");
        else Rprintf("NA (or other non-finite) value(s) are present in input, re-running frollfun with extra care for NAs\n");
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
    if (nc == 0) ans[i] = (double) w / k;                       // no NAs in first full window
    else if (nc == k) ans[i] = narm ? R_NaN : NA_REAL;          // all values in sliding window are NA, expected output for fun(NA, na.rm=T/F)
    else ans[i] = narm ? (double) w / (k - nc) : NA_REAL;       // some values in window are NA
    for (uint_fast64_t i=k; i<nx; i++) {                        // loop over obs, complete window
      if (R_FINITE(x[i])) w += x[i];                            // add only finite to window aggregate
      else nc++;                                                // increment NA count in current window
      if (R_FINITE(x[i-k])) w -= x[i-k];                        // remove only finite from window aggregate
      else nc--;                                                // decrement NA count in current window
      if (nc == 0) ans[i] = (double) w / k;                     // no NAs in sliding window for present observation
      else if (nc == k) ans[i] = narm ? R_NaN : NA_REAL;        // all values in window are NA, expected output for fun(NA, na.rm=T/F)
      else ans[i] = narm ? (double) w / (k - nc) : NA_REAL;     // some values in window are NA
    }
    if (!R_FINITE((double) w) && verbose) {
      Rprintf("infinite value(s) are present in input, default frollfun function does not handle infinite values, use exact=TRUE\n");
    }
  }
  if (align < 1) {                                              // align center or left
    int k_ = align==-1 ? k-1 : floor(k/2);                      // offset to shift
    memcpy((char *)ans, (char *)ans + (k_*sizeof(double)), (nx-k_)*sizeof(double)); // apply shift to achieve expected align
    for (uint_fast64_t i=nx-k_; i<nx; i++) ans[i] = fill;       // fill from right side
  }
}

/* fast exact rolling mean
 * if window bigger than input length then skip processing and return NA vector
 * when no info on NA (hasNA argument) then assume no NAs run faster version, also when na.rm=FALSE faster version can proceed
 * rollmean implemented as mean for each observation for align="right"
 * if NAs detected and na.rm=TRUE then re-run rollmean implemented as mean for each observation with NA support
 * apply shift if align!="right"
 */

void frollmeanExact(double *x, uint_fast64_t nx, double *ans, int k, int align, double fill, bool narm, int hasna, bool verbose) {
  if (nx < k) {                                                 // if window width bigger than input just return vector of fill values
    if (verbose) Rprintf("frollfunExact window width longer than input vector, returning all NA vector\n");
    for (int i=0; i<nx; i++) ans[i] = fill;
    return;
  }
  if (verbose) Rprintf("frollfunExact running for input length %lu, window %d, align %d, hasna %d\n", nx, k, align, hasna);
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
        long double w = 0.;
        for (int j=-k+1; j<=0; j++) {                           // sub-loop on window width
          w += x[i+j];                                          // sum of window for particular observation
        }
        ans[i] = ((double) w) / k;                              // fun of window for particular observation
        if (R_FINITE((double) w)) {                             // no need to calc roundoff correction if NAs detected as will re-call all below in truehasna==1
          long double t = 0.0;                                  // accumulate roundoff error
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            // todo replace ans[i] with long double
            t += x[i+j] - ans[i];                               // measure difference of obs in sub-loop to calculated fun for obs
          }
          ans[i] += ((double) t) / k;                           // adjust calculated rollfun with roundoff correction
        } else {
          truehasna = 1;                                        // NAs detected for this window, set flag so rest of windows will not be re-run
        }
      }
    } // end of parallel region
    if (truehasna && verbose) {
      if (narm) {
        if (hasna==-1) Rprintf("hasNA FALSE was used but NA (or other non-finite) value(s) are present in input, re-running frollfunExact with extra care for NAs\n");
        else Rprintf("NA (or other non-finite) value(s) are present in input, re-running frollfunExact with extra care for NAs\n");
      } else {
        if (hasna==-1) Rprintf("hasNA FALSE was used but NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run\n");
        else Rprintf("NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run\n");
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
        long double w = 0.;
        int nc = 0;                                             // NA counter within sliding window
        for (int j=-k+1; j<=0; j++) {                           // sub-loop on window width
          if (ISNAN(x[i+j])) nc++;                              // increment NA count in current window
          else w += x[i+j];                                     // add observation to current window
        }
        ans[i] = nc==k ? R_NaN : ((double) w) / (k - nc);       // rollfun answer before error correction, handle all NA input
        long double t = 0.0;                                    // accumulate roundoff error
        if (nc == 0) {                                          // no NAs in current window
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            t += x[i+j] - ans[i];                               // measure roundoff for each obs in window
          }
          ans[i] += ((double) t) / k;                           // adjust calculated fun with roundoff correction
        } else if (nc < k) {
          for (int j=-k+1; j<=0; j++) {                         // sub-loop on window width
            if (!ISNAN(x[i+j])) t += x[i+j] - ans[i];           // measure roundoff for each obs in window
          }
          ans[i] += ((double) t) / (k - nc);                    // adjust calculated fun with roundoff correction
        }
      }
    } // end of parallel region
  }
  if (align < 1) {                                              // align center or left
    int k_ = align==-1 ? k-1 : floor(k/2);                      // offset to shift
    memcpy((char *)ans, (char *)ans + (k_*sizeof(double)), (nx-k_)*sizeof(double)); // apply shift to achieve expected align
    for (uint_fast64_t i=nx-k_; i<nx; i++) ans[i] = fill;       // fill from right side
  }
}

/* fast adaptive rolling mean
 * when no info on NA (hasNA argument) then assume no NAs run faster version
 * adaptive rollmean implemented as cumsum first pass, then diff cumsum by indexes `i` to `i-k[i]`, for align="right"
 * if NAs detected re-run rollmean implemented as sliding window with NA support
 * apply shift if align!="right"
 */

void frollmeanAdaptive(double *x, uint_fast64_t nx, double *ans, int *k, double fill, bool narm, int hasna, bool verbose) {
  bool truehasna = hasna>0;                                     // flag to re-run if NAs detected
  long double w = 0.0;
  // TODO measure speed of cs as long double
  double cs[nx];                                                // cumsum vector
  if (!truehasna) {
    for (uint_fast64_t i=0; i<nx; i++) {                        // loop on every observation
      w += x[i];                                                // cumulate in long double
      cs[i] = (double) w;
    }
    if (R_FINITE((double) w)) {                                 // no need to calc this if NAs detected as will re-calc all below in truehasna==1
      #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx))
      {
        #pragma omp for schedule(static)
        for (uint_fast64_t i=0; i<nx; i++) {
          if (i+1 == k[i]) ans[i] = cs[i]/k[i];                 // current obs window width exactly same as obs position in a vector
          else if (i+1 > k[i]) ans[i] = (cs[i]-cs[i-k[i]])/k[i];// window width smaller than position so use cumsum to calculate diff
          else ans[i] = fill;                                   // position in a vector smaller than obs window width - partial window
        }
      } // end of parallel region
    } else {                                                    // update truehasna flag if NAs detected
      if (verbose) {
        if (hasna==-1) Rprintf("hasNA FALSE was used but NA (or other non-finite) value(s) are present in input, re-running frollfunAdaptive with extra care for NAs\n");
        else Rprintf("NA (or other non-finite) value(s) are present in input, re-running frollfunAdaptive with extra care for NAs\n");
      }
      w = 0.0;
      truehasna = 1;
    }
  }
  if (truehasna) {
    uint_fast64_t nc = 0;                                       // running NA counter
    uint_fast64_t cn[nx];                                       // cumulative NA counter, used the same way as cumsum
    for (uint_fast64_t i=0; i<nx; i++) {                        // loop over observations
      if (R_FINITE(x[i])) w += x[i];                            // add observation to running sum
      else nc++;                                                // increment non-finite counter
      cs[i] = (double) w;                                       // cumsum, na.rm=TRUE always, NAs handled using cum NA counter
      cn[i] = nc;                                               // cum NA counter
    }
    #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx))
    {
      #pragma omp for schedule(static)
      for (uint_fast64_t i=0; i<nx; i++) {                      // loop over observations
        if (i+1 < k[i]) {                                       // partial window
          ans[i] = fill;
        } else if (!narm) {                                     // this branch reduce number of branching in narm=1 below
          if (i+1 == k[i]) {
            ans[i] = cn[i]>0 ? NA_REAL : cs[i]/k[i];
          } else if (i+1 > k[i]) {
            ans[i] = (cn[i] - cn[i-k[i]])>0 ? NA_REAL : (cs[i]-cs[i-k[i]])/k[i];
          }
        } else if (i+1 == k[i]) {                               // window width equal to observation position in vector
          int thisk = k[i] - ((int) cn[i]);                     // window width after taking NAs into account
          ans[i] = thisk==0 ? R_NaN : cs[i]/thisk;              // handle all obs NAs and na.rm=TRUE
        } else if (i+1 > k[i]) {                                // window width smaller than observation position in vector
          int thisk = k[i] - ((int) (cn[i] - cn[i-k[i]]));      // window width after taking NAs into account
          ans[i] = thisk==0 ? R_NaN : (cs[i]-cs[i-k[i]])/thisk; // handle all obs NAs and na.rm=TRUE
        }
      }
    } // end of parallel region
  } // end of truehasna
}

void frollmeanExactAdaptive(double *x, uint_fast64_t nx, double *ans, int *k, double fill, bool narm, int hasna, bool verbose) {
  volatile bool truehasna = hasna>0;                            // flag to re-run if NAs detected
  
  if (!truehasna || !narm) {
    #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx)) shared(truehasna)
    {
      #pragma omp for schedule(static)
      for (uint_fast64_t i=0; i<nx; i++) {                      // loop on every observation
        if (narm && truehasna) continue;                        // if NAs detected no point to continue
        if (i+1 < k[i]) ans[i] = fill;                          // position in a vector smaller than obs window width - partial window
        else {
          long double w = 0.0;
          for (int j=-k[i]+1; j<=0; j++) {                      // sub-loop on window width
            w += x[i+j];                                        // sum of window for particular observation
          }
          if (R_FINITE((double) w)) {                           // no need to calc roundoff correction if NAs detected as will re-call all below in truehasna==1
            long double res = w / k[i];                         // keep results as long double for intermediate processing
            long double err = 0.0;                              // roundoff corrector
            for (int j=-k[i]+1; j<=0; j++) {                    // sub-loop on window width
              err += x[i+j] - res;                              // measure difference of obs in sub-loop to calculated fun for obs
            }
            ans[i] = (double) (res + (err / k[i]));             // adjust calculated fun with roundoff correction
          } else {
            if (!narm) ans[i] = (double) (w / k[i]);            // NAs should be propagated
            truehasna = 1;                                      // NAs detected for this window, set flag so rest of windows will not be re-run
          }
        }
      }
    } // end of parallel region
    if (truehasna && verbose) {
      if (narm) {
        if (hasna==-1) Rprintf("hasNA FALSE was used but NA (or other non-finite) value(s) are present in input, re-running frollfunExactAdaptive with extra care for NAs\n");
        else Rprintf("NA (or other non-finite) value(s) are present in input, re-running frollfunExactAdaptive with extra care for NAs\n");
      } else {
        if (hasna==-1) Rprintf("hasNA FALSE was used but NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run\n");
        else Rprintf("NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run\n");
      }
    }
  }
  if (truehasna && narm) {
    #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx))
    {
      #pragma omp for schedule(static)
      for (uint_fast64_t i=0; i<nx; i++) {                      // loop over observations
        if (i+1 < k[i]) ans[i] = fill;                          // partial window
        else {
          long double w = 0.0;                                  // window to accumulate values in particular window
          long double err = 0.0;                                // accumulate roundoff error
          long double res;                                      // keep results as long double for intermediate processing
          int nc = 0;                                           // NA counter within current window
          for (int j=-k[i]+1; j<=0; j++) {                      // sub-loop on window width
            if (ISNAN(x[i+j])) nc++;                            // increment NA count in current window
            else w += x[i+j];                                   // add observation to current window
          }
          if (nc == 0) {                                        // no NAs in current window
            res = w / k[i];
            for (int j=-k[i]+1; j<=0; j++) {                    // sub-loop on window width to accumulate roundoff error
              err += x[i+j] - res;                              // measure roundoff for each obs in window
            }
            ans[i] = (double) (res + (err / k[i]));             // adjust calculated fun with roundoff correction
          } else if (nc < k[i]) {
            res = w / (k[i]-nc);
            for (int j=-k[i]+1; j<=0; j++) {                    // sub-loop on window width to accumulate roundoff error
              if (!ISNAN(x[i+j])) err += x[i+j] - res;          // measure roundoff for each obs in window
            }
            ans[i] = (double) (res + (err / (k[i] - nc)));      // adjust calculated fun with roundoff correction
          } else if (nc == k[i]) {
            ans[i] = R_NaN;                                     // this branch assume narm so R_NaN always here
          }
        }
      }
    } // end of parallel region
  } // end of truehasna
}

/* fast rolling sum 
 * copy from frollmean when reviewed
 */
