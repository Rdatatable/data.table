#include "roll.h"

void rollmeanVector(double *x, uint_fast64_t nx, double *ans, int k, int align, double fill, bool partial, bool exact, bool narm, int hasna, int verbose) {

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
          if (verbose>2) Rprintf("loop1: ans[%lu] = %8.3f\n", i+si, w/wk);
        }
        if (verbose>1) Rprintf("loop1: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, NA_REAL, x[i], i+si, w);
      }
      for (uint_fast64_t i=k; i<nx; i++) {                      // loop over obs, complete window
        w -= x[i-k];                                            // remove leaving row from window aggregate
        w += x[i];                                              // add current row to window aggregate
        ans[i+si] = w / k;                                      // rollfun to answer vector
        if (verbose>2) Rprintf("loop2: ans[%lu] = %8.3f\n", i+si, w/k);
        if (verbose>1) Rprintf("loop2: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], x[i], i+si, w);
      }
      for (uint_fast64_t i=nx; i<(nx-si); i++) {                // loop over obs, potentially incomplete window from right, skipped for align=right
        w -= x[i-k];                                            // remove leaving row from window aggregate
        ans[i+si] = w / k;                                      // rollfun to answer vector
        if (verbose>2) Rprintf("loop3: ans[%lu] = %8.3f\n", i+si, w/k);
        if (verbose>1) Rprintf("loop3: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], NA_REAL, i+si, w);
      }
    } else { // exact==TRUE
    
    }
    if (ISNAN(w)) {
      if (verbose>0 && hasna==-1) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
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
          if (verbose>2) Rprintf("loop1: ans[%lu] = %8.3f\n", i+si, w/wk);
        }
        if (verbose>1) Rprintf("loop1: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, NA_REAL, x[i], i+si, w);
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
        if (verbose>2) Rprintf("loop2: ans[%lu] = %8.3f\n", i+si, w/k);
        if (verbose>1) Rprintf("loop2: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], x[i], i+si, w);
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
        if (verbose>2) Rprintf("loop3: ans[%lu] = %8.3f\n", i+si, w/k);
        if (verbose>1) Rprintf("loop3: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], NA_REAL, i+si, w);
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

void rollmeanVectorAdaptive(double *x, uint_fast64_t nx, double *ans, int *k, double fill, bool exact, bool narm, int hasna, int verbose) {
  double w = 0.; // running window sum
  bool truehasna = hasna>0; // flag to re-run if NAs detected
  int thisk, lastk, diffk;

  if (!truehasna) {
    if (!exact) {
      // move to rollmean when ans allocation is done:      if (length(thiskl) != xrows) error("length of integer vector in 'n' is not equal to length of column in 'x', for adaptive TRUE those has to be equal");
      for (uint_fast64_t i=0; i<nx; i++) {                        // loop over observations in column
        thisk = k[i];
        w += x[i];                            // add current row to window sum
        if (i + 1 < thisk) ans[i] = fill;    // fill partial window
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
            // diffk == 1: do not add or remove from window
          }
          ans[i] = w / thisk;                 // calculate mean
        }
      }
    } else { // exact==TRUE
    
    }
    if (ISNAN(w)) { // ISNAN translate to C
      if (verbose>0 && hasna==-1) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
      // warning/print not thread safe
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

void rollsumVector(double *x, uint_fast64_t nx, double *ans, int k, int align, double fill, bool partial, bool exact, bool narm, int hasna, int verbose) {
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
          if (verbose>2) Rprintf("ans[%lu] = %8.3f\n", i+si, w);
        }
        if (verbose>1) Rprintf("loop1: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, NA_REAL, x[i], i+si, w);
      }
      for (uint_fast64_t i=k; i<nx; i++) {
        w -= x[i-k];
        w += x[i];
        ans[i+si] = w;
        if (verbose>2) Rprintf("ans[%lu] = %8.3f\n", i+si, w);
        if (verbose>1) Rprintf("loop2: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], x[i], i+si, w);
      }
      
      for (uint_fast64_t i=nx; i<(nx-si); i++) {
        w -= x[i-k];
        ans[i+si] = w;
        if (verbose>2) Rprintf("ans[%lu] = %8.3f\n", i+si, w);
        if (verbose>1) Rprintf("loop3: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], NA_REAL, i+si, w);
      }
    } else { // exact==TRUE
    
    }
    if (ISNAN(w)) { // ISNAN translate to C
      if (verbose>0 && hasna==-1) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
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
          if (verbose>2) Rprintf("loop1: ans[%lu] = %8.3f\n", i+si, w/wk);
        }
        if (verbose>1) Rprintf("loop1: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, NA_REAL, x[i], i+si, w);
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
        if (verbose>2) Rprintf("loop2: ans[%lu] = %8.3f\n", i+si, w);
        if (verbose>1) Rprintf("loop2: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], x[i], i+si, w);
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
        if (verbose>2) Rprintf("loop3: ans[%lu] = %8.3f\n", i+si, w);
        if (verbose>1) Rprintf("loop3: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-k], NA_REAL, i+si, w);
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

void rollsumVectorAdaptive(double *x, uint_fast64_t nx, double *ans, int *k, double fill, bool exact, bool narm, int hasna, int verbose) {
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
      if (verbose>0 && hasna==-1) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
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
