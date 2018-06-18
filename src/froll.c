#include "froll.h"

void frollmeanVector(double *x, uint_fast64_t nx, double *ans, int k, int align, double fill, bool partial, bool exact, bool narm, int hasna, bool verbose) {

  uint_fast64_t si =                                            // align shift for ans index
    align > 0 ? 0 :                                             // align right
    align < 0 ? -k+1 :                                          // align left
    -floor(k/2);                                                // align center
  double w = 0.;                                                // running window aggregate
  bool truehasna = hasna>0;                                     // flag to re-run if NAs detected
  if (!truehasna) {
    if (!exact) {
      int wk = nx < k ? nx : k;                                 // window width might be longer than column length, could not override k unless we merge `fill` loops here
      for (uint_fast64_t i=0; i<wk; i++) {                      // loop over obs, potentially incomplete window from left
        w += x[i];                                              // add current row to window aggregate
        if (i >= -si) {
          ans[i+si] = w / wk;                                   // rollfun to answer vector
          //if (verbose) Rprintf("loop1: ans[%lu] = %8.3f\n", i+si, w/wk);
        }
        if (verbose) Rprintf("loop1: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, NA_REAL, x[i], i+si, w);
      }
      for (uint_fast64_t i=k; i<nx; i++) {                      // loop over obs, complete window
        w -= x[i-k];                                            // remove leaving row from window aggregate
        w += x[i];                                              // add current row to window aggregate
        ans[i+si] = w / k;                                      // rollfun to answer vector
        //if (verbose) Rprintf("loop2: ans[%lu] = %8.3f\n", i+si, w/k);
        if (verbose) Rprintf("loop2: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], x[i], i+si, w);
      }
      for (uint_fast64_t i=nx; i<(nx-si); i++) {                // loop over obs, potentially incomplete window from right, skipped for align=right
        w -= x[i-k];                                            // remove leaving row from window aggregate
        ans[i+si] = w / k;                                      // rollfun to answer vector
        //if (verbose) Rprintf("loop3: ans[%lu] = %8.3f\n", i+si, w/k);
        if (verbose) Rprintf("loop3: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], NA_REAL, i+si, w);
      }
    } else { // exact==TRUE

    }
    if (ISNAN(w)) {
      if (verbose && hasna==-1) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
      w = 0.;
      truehasna = 1;
    } else if (!partial) {                                      // fill partial window
      for (uint_fast64_t i=0; i<(k-1); i++) {                   // fill for align right/center
        if (i >= -si) ans[i+si] = fill;                         // fill answer vector
      }
      for (uint_fast64_t i=nx; i<(nx-si); i++) {                // fill for align left/center
        ans[i+si] = fill;                                       // fill answer vector
      }
    }
  }
  if (truehasna) {
    if (!exact) {
      int nc = 0;                                               // NA counter within running window
      int wk = nx < k ? nx : k;                                 // window width might be longer than column length, could not override k unless we merge `fill` loops here
      for (uint_fast64_t i=0; i<wk; i++) {                      // loop over obs, potentially incomplete window from left
        if (ISNAN(x[i])) nc++;                                  // increment NA count in current window
        else w += x[i];                                         // add only non-NA to window aggregate
        if (i >= -si) {
          if (nc == 0) ans[i+si] = w / wk;                      // rollfun to answer vector
          else if (nc == wk) {                                  // all values in window are NA
            if (narm) ans[i+si] = R_NaN;                        // expected output for fun(NA, na.rm=T)
            else ans[i+si] = NA_REAL;                           // expected output for fun(NA, na.rm=F)
          }
          else {                                                // some values in window are NA
            if (narm) ans[i+si] = w / (wk - nc);                // rollfun to answer vector when NA present
            else ans[i+si] = NA_REAL;                           // NA present and na.rm=FALSE result NA
          }
          //if (verbose) Rprintf("loop1: ans[%lu] = %8.3f\n", i+si, w/wk);
        }
        if (verbose) Rprintf("loop1: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, NA_REAL, x[i], i+si, w);
      }
      for (uint_fast64_t i=k; i<nx; i++) {                      // loop over obs, complete window
        if (ISNAN(x[i])) nc++;                                  // increment NA count in current window
        else w += x[i];                                         // add only non-NA to window aggregate
        if (ISNAN(x[i-k])) nc--;                                // decrement NA count in current window
        else w -= x[i-k];                                       // remove only non-NA from window aggregate
        if (nc == 0) ans[i+si] = w / k;                         // rollfun to answer vector when no NAs
        else if (nc == k) {                                     // all values in window are NA
          if (narm) ans[i+si] = R_NaN;                          // expected output for fun(NA, na.rm=T)
          else ans[i+si] = NA_REAL;                             // expected output for fun(NA, na.rm=F)
        }
        else {                                                  // some values in window are NA
          if (narm) ans[i+si] = w / (k - nc);                   // rollfun to answer vector when NA present
          else ans[i+si] = NA_REAL;                             // NA present and na.rm=FALSE result NA
        }
        //if (verbose) Rprintf("loop2: ans[%lu] = %8.3f\n", i+si, w/k);
        if (verbose) Rprintf("loop2: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], x[i], i+si, w);
      }
      for (uint_fast64_t i=nx; i<(nx-si); i++) {                // loop over obs, potentially incomplete window from right, skipped for align=right
        if (ISNAN(x[i-k])) nc--;                                // decrement NA count in current window
        else w -= x[i-k];                                       // remove only non-NA from window aggregate
        if (nc == 0) ans[i+si] = w / k;                         // rollfun to answer vector when no NAs
        else if (nc == k) {                                     // all values in window are NA
          if (narm) ans[i+si] = R_NaN;                          // expected output for fun(NA, na.rm=T)
          else ans[i+si] = NA_REAL;                             // expected output for fun(NA, na.rm=F)
        }
        else {                                                  // some values in window are NA
          if (narm) ans[i+si] = w / (k - nc);                   // rollfun to answer vector when NA present
          else ans[i+si] = NA_REAL;                             // NA present and na.rm=FALSE result NA
        }
        //if (verbose) Rprintf("loop3: ans[%lu] = %8.3f\n", i+si, w/k);
        if (verbose) Rprintf("loop3: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], NA_REAL, i+si, w);
      }
      if (ISNAN(w)) {
        error("internal error: NAs should be handled at this point");
      } else if (!partial) {                                    // fill partial window
        for (uint_fast64_t i=0; i<(k-1); i++) {                 // fill for align right/center
           if (i >= -si) ans[i+si] = fill;                      // fill answer vector
        }
        for (uint_fast64_t i=nx; i<(nx-si); i++) {              // fill for align left/center
          ans[i+si] = fill;                                     // fill answer vector
        }
      }
    } else { // exact==TRUE

    }
  }
}

void frollmeanVectorAdaptive(double *x, uint_fast64_t nx, double *ans, int *k, double fill, bool exact, bool narm, int hasna, bool verbose) {
  bool truehasna = hasna>0;                                     // flag to re-run if NAs detected
  long double w = 0.0;
  
  if (!truehasna) {
    if (!exact) {                                               // exact==0 adaptive roll fun implementation using cumsum
      double cs[nx];                                            // cumsum vector
      for (uint_fast64_t i=0; i<nx; i++) {                      // loop on every observation
        w += x[i];                                              // cumulate in long double
        cs[i] = (double) w;
      }
      if (R_FINITE((double) w)) {                               // no need to calc this if NAs detected as will re-calc all below in truehasna==1
        #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx))
        {
          #pragma omp for schedule(static)
          for (uint_fast64_t i=0; i<nx; i++) {
            if (i+1 == k[i]) ans[i] = cs[i]/k[i];               // current obs window width exactly same as obs position in a vector
            else if (i+1 > k[i]) ans[i] = (cs[i]-cs[i-k[i]])/k[i]; // window width smaller than position so use cumsum to calculate diff
            else ans[i] = fill;                                 // position in a vector smaller than obs window width - partial window
          }
        } // end of parallel region
      }
    } else {                                                    // exact==1 adaptive roll fun implementation using loops for each observation
      #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx))
      {
        #pragma omp for schedule(static)
        for (uint_fast64_t i=0; i<nx; i++) {                    // loop on every observation
          if (i+1 < k[i]) ans[i] = fill;                        // position in a vector smaller than obs window width - partial window
          else {
            for (int j=1-k[i]; j<=0; j++) {                     // sub-loop on window width
              w += x[i+j];                                      // sum of window for particular observation
            }
            ans[i] = ((double) w) / k[i];                       // mean of window for particular observation
            if (R_FINITE((double) w)) {                         // no need to calc roundoff correction if NAs detected as will re-call all below in truehasna==1
              long double t = 0.0;                              // roundoff corrector
              for (int j=-k[i]+1; j<=0; j++) {                  // sub-loop on window width
                t += x[i+j] - ans[i];                           // measure difference of obs in sub-loop to calculated mean for obs
              }
              ans[i] += ((double) t) / k[i];                    // adjust calculated mean with roundoff correction
            }
          }
        }
      } // end of parallel region
    }
    if (!R_FINITE((double) w)) {                                // update truehasna flag if NAs detected
      if (verbose) {
        if (hasna==-1) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
        else Rprintf("NAs detected, re-running rolling function with extra care for NAs\n");
      }
      w = 0.0;
      truehasna = 1;
    }
  }
  if (truehasna) {
    if (!exact) {                                               // exact==0 branch
      uint_fast64_t v = 0;                                      // running NA counter
      double cs[nx];                                            // cumsum
      uint_fast64_t cn[nx];                                     // cumulative NA counter, used the same way as cumsum
      for (uint_fast64_t i=0; i<nx; i++) {                      // loop over observations
        if (ISNAN(x[i])) v++;                                   // increment NA counter
        else w += x[i];                                         // add observation to running sum
        cs[i] = (double) w;                                     // cumsum, na.rm=TRUE always, NAs handled using cum NA counter
        cn[i] = v;                                              // cum NA counter
      }
      #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx))
      {
        #pragma omp for schedule(static)
        for (uint_fast64_t i=0; i<nx; i++) {                    // loop over observations
          if (i+1 < k[i]) {                                     // partial window
            ans[i] = fill;
          } else if (!narm) {                                   // this branch reduce number of branching in narm=1 below
            if (i+1 == k[i]) {
              ans[i] = cn[i]>0 ? NA_REAL : cs[i]/k[i];
            } else if (i+1 > k[i]) {
              ans[i] = (cn[i] - cn[i-k[i]])>0 ? NA_REAL : (cs[i]-cs[i-k[i]])/k[i];
            }
          } else if (i+1 == k[i]) {                             // window width equal to observation position in vector
            int thisk = k[i] - ((int) cn[i]);                   // window width after taking NAs into account
            ans[i] = thisk==0 ? R_NaN : cs[i]/thisk;            // handle all obs NAs and na.rm=TRUE
          } else if (i+1 > k[i]) {                              // window width smaller than observation position in vector
            int thisk = k[i] - ((int) (cn[i] - cn[i-k[i]]));    // window width after taking NAs into account
            ans[i] = thisk==0 ? R_NaN : (cs[i]-cs[i-k[i]])/thisk; // handle all obs NAs and na.rm=TRUE
          }
        }
      } // end of parallel region
    } else {                                                    // exact==1 branch
      #pragma omp parallel num_threads(verbose ? 1 : MIN(getDTthreads(), nx))
      {
        #pragma omp for schedule(static)
        for (uint_fast64_t i=0; i<nx; i++) {                    // loop over observations
          if (i+1 < k[i]) ans[i] = fill;                        // partial window
          else {
            int thisk=0;
            w = 0.0;
            for (int j=1-k[i]; j<=0; j++) {                     // sub-loop on window width
              if (narm) {
                if (!ISNAN(x[i+j])) {
                  w += x[i+j];                                  // window sum na.rm=TRUE
                  thisk++;                                      // window width after removing NAs
                }
              } else {                                          // narm==0 this will propagate NAs nicely
                w += x[i+j];
              }
            }
            if (narm) {
              ans[i] = thisk==0 ? R_NaN : ((double) w / thisk); // handle all NAs and na.rm=TRUE
            } else {
              ans[i] = ((double) w) / k[i];                     // narm==0, propagate NAs
            }
            if (R_FINITE((double) w)) {                         // roundoff correction only when window sum was non NA
              if (R_FINITE(ans[i])) {                           // roundoff correction only when answer was non NA
                long double t = 0.0;                            // roundoff collector
                thisk = 0;
                for (int j=-k[i]+1; j<=0; j++) {                // sub-loop over window for each observation
                  if (!ISNAN(x[i+j])) {                         // hard skip NAs here, there should be no thisk==0 case here as R_FINITE(ans[i]) check would filter that out
                    t += x[i+j] - ans[i];                       // measure difference of obs in sub-loop to calculated mean for obs
                    thisk++;                                    // window width after removing NAs
                  }
                  else if (verbose && !narm) error("That should not happen as R_FINITE(ans[i]) should filter out this loop iteration, please report\n");
                }
                ans[i] += ((double) t) / thisk;                 // adjust answer for roundoff error
              }
            }
          }
        }
      } // end of parallel region
    } // end of exact==1
  } // end of truehasna
}

void frollsumVector(double *x, uint_fast64_t nx, double *ans, int k, int align, double fill, bool partial, bool exact, bool narm, int hasna, bool verbose) {
  uint_fast64_t si =
    align > 0 ? 0 :
    align < 0 ? -k+1 :
    -floor(k/2);
  double w = 0.;
  bool truehasna = hasna>0;
  if (!truehasna) {
    if (!exact) {
      int wk = nx < k ? nx : k;
      for (uint_fast64_t i=0; i<wk; i++) {
        w += x[i];
        if (i >= -si) {
          ans[i+si] = w;
          //if (verbose) Rprintf("ans[%lu] = %8.3f\n", i+si, w);
        }
        if (verbose) Rprintf("loop1: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, NA_REAL, x[i], i+si, w);
      }
      for (uint_fast64_t i=k; i<nx; i++) {
        w -= x[i-k];
        w += x[i];
        ans[i+si] = w;
        //if (verbose) Rprintf("ans[%lu] = %8.3f\n", i+si, w);
        if (verbose) Rprintf("loop2: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], x[i], i+si, w);
      }
      
      for (uint_fast64_t i=nx; i<(nx-si); i++) {
        w -= x[i-k];
        ans[i+si] = w;
        //if (verbose) Rprintf("ans[%lu] = %8.3f\n", i+si, w);
        if (verbose) Rprintf("loop3: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], NA_REAL, i+si, w);
      }
    } else { // exact==TRUE
    
    }
    if (ISNAN(w)) { // ISNAN translate to C
      if (verbose && hasna==-1) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
      w = 0.;
      truehasna = 1;
    } else if (!partial) {
      for (uint_fast64_t i=0; i<(k-1); i++) {
        if (i >= -si) ans[i+si] = fill;
      }
      for (uint_fast64_t i=nx; i<(nx-si); i++) {
        ans[i+si] = fill;
      }
    }
  }
  if (truehasna) {
    if (!exact) {
      int nc = 0;
      int wk = nx < k ? nx : k;
      for (uint_fast64_t i=0; i<wk; i++) {
        if (ISNAN(x[i])) nc++;
        else w += x[i];
        if (i >= -si) {
          if (nc == 0) ans[i+si] = w;
          else if (nc == wk) {
            if (narm) ans[i+si] = 0;
            else ans[i+si] = NA_REAL;
          }
          else {
            if (narm) ans[i+si] = w;
            else ans[i+si] = NA_REAL;
          }
          //if (verbose) Rprintf("loop1: ans[%lu] = %8.3f\n", i+si, w/wk);
        }
        if (verbose) Rprintf("loop1: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, NA_REAL, x[i], i+si, w);
      }
      for (uint_fast64_t i=k; i<nx; i++) {
        if (ISNAN(x[i])) nc++;
        else w += x[i];
        if (ISNAN(x[i-k])) nc--;
        else w -= x[i-k];
        if (nc == 0) ans[i+si] = w;
        else if (nc == k) {
          if (narm) ans[i+si] = 0;
          else ans[i+si] = NA_REAL;
        }
        else {
          if (narm) ans[i+si] = w;
          else ans[i+si] = NA_REAL;
        }
        //if (verbose) Rprintf("loop2: ans[%lu] = %8.3f\n", i+si, w);
        if (verbose) Rprintf("loop2: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], x[i], i+si, w);
      }
      for (uint_fast64_t i=nx; i<(nx-si); i++) {
        if (ISNAN(x[i-k])) nc--;
        else w -= x[i-k];
        if (nc == 0) ans[i+si] = w;
        else if (nc == k) {
          if (narm) ans[i+si] = 0;
          else ans[i+si] = NA_REAL;
        }
        else {
          if (narm) ans[i+si] = w;
          else ans[i+si] = NA_REAL;
        }
        //if (verbose) Rprintf("loop3: ans[%lu] = %8.3f\n", i+si, w);
        if (verbose) Rprintf("loop3: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], NA_REAL, i+si, w);
      }
      if (ISNAN(w)) {
        error("internal error: NAs should be handled at this point");
      } else if (!partial) {
        for (uint_fast64_t i=0; i<(k-1); i++) {
           if (i >= -si) ans[i+si] = fill;
        }
        for (uint_fast64_t i=nx; i<(nx-si); i++) {
          ans[i+si] = fill;
        }
      }
    } else { // exact==TRUE

    }
  }
}

void frollsumVectorAdaptive(double *x, uint_fast64_t nx, double *ans, int *k, double fill, bool exact, bool narm, int hasna, bool verbose) {
  double w = 0.;
  bool truehasna = hasna>0;
  int thisk, lastk, diffk;

  if (!truehasna) {
    if (!exact) {
      for (uint_fast64_t i=0; i<nx; i++) {
        thisk = k[i];
        w += x[i];
        if (i + 1 < thisk) ans[i] = fill;
        else {
          lastk = k[i-1];
          diffk = thisk - lastk;
          //Rprintf("row %"PRIuFAST64", window %d, prev window %d, diff %d\n", i+1, thisk, lastk, diffk);
          if (diffk > 1) {
            for (int j=0; j<diffk-1; j++) {
              //Rprintf("row %"PRIuFAST64", diff %d (so adding %d), adding row %d, value %8.3f\n", i+1, diffk, diffk-1, i-thisk+j+1+1, x[i-thisk+j+1]);
              w += x[i-thisk+j+1];
            }
          } else if (diffk < 1) {
            for (int j=0; j<-diffk+1; j++) {
              if (i-thisk-j < 0) continue;
              //Rprintf("row %"PRIuFAST64", diff %d (so removing %d), removing row %d, value %8.3f\n", i+1, diffk, -diffk+1, i-thisk-j+1, x[i-thisk-j]);
              w -= x[i-thisk-j];
            }
          } else {
            //Rprintf("row %"PRIuFAST64", diff %d, no add no remove\n", i+1, diffk);
          }
          ans[i] = w;
        }
      }
    } else { // exact==TRUE
    
    }
    if (ISNAN(w)) {
      if (verbose && hasna==-1) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
      w = 0.;
      truehasna = 1;
    }
  }
  if (truehasna) {
    if (!exact) {
      
    } else { // exact==TRUE
      
    }
  }
}
