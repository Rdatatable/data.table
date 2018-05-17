#include "roll.h"

void rollmeanVector(double x[], uint_fast64_t nx, double ans[], int k, double fill, bool exact, bool narm, bool hasna, bool nahasna) {
  double w = 0.; // running window sum
  bool truehasna = hasna; // flag to re-run if NAs detected

  if (!truehasna) {
    if (!exact) {
      for (uint_fast64_t i=0; i<nx; i++) {  // loop over observations in column
        w += x[i];                          // add current row to window sum
        if (i < k-1) ans[i] = fill;         // fill partial window
        else {
          if (i >= k) w -= x[i-k];          // remove leaving row from window sum
          ans[i] = w / k;                   // calculate mean
        }
      }
    } else { // exact==TRUE
    
    }
    if (ISNAN(w)) { // ISNAN translate to C
      //if (nahasna==0) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
      // warning/print not thread safe
      w = 0.;
      truehasna = 1;
    }
  }
  if (truehasna) {
    if (!exact) {
      int nc = 0;                               // NA counter within running window
      for (uint_fast64_t i=0; i<nx; i++) {      // loop over observations in column
        if (ISNAN(x[i])) nc++;                  // increment NA count in current window
        else w += x[i];                         // add only non-NA to window sum
        if (i >= k) {
          if (ISNAN(x[i-k])) nc--;              // decrement NA count in current window
          else w -= x[i-k];                     // remove only non-NA from window sum
        }
        if (i + 1 < k) ans[i] = fill;           // fill partial window
        else {
          if (nc == 0) ans[i] = w / k;          // calculate mean
          else if (nc == k) {                   // all values in window are NA
            if (narm) ans[i] = R_NaN;           // for mean(NA, na.rm=T) result NaN
            else ans[i] = NA_REAL;              // for mean(NA, na.rm=F) result NA
          }
          else {                                // some values in window are NA
            if (narm) ans[i] = w / (k - nc);    // calculate mean if NA present
            else ans[i] = NA_REAL;              // NA present and na.rm=FALSE result NA
          }
        }
      }
    } else { // exact==TRUE

    }
  }
}

void rollmeanVectorAdaptive(double x[], uint_fast64_t nx, double ans[], int k[], double fill, bool exact, bool narm, bool hasna, bool nahasna) {
  double w = 0.; // running window sum
  bool truehasna = hasna; // flag to re-run if NAs detected
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
      //if (nahasna==0) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
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

// - [ ] hasna + nahasna -> int (-1, 0, 1)
// - [ ] int align (-1, 0, 1)
// - [ ] 3 loops instead of 1, less nested `if`s, easier to plug `align`

void rollsumVector(double x[], uint_fast64_t nx, double ans[], int k, double fill, bool exact, bool narm, bool hasna, bool nahasna) {
  double w = 0.; // running window sum
  bool truehasna = hasna; // flag to re-run if NAs detected

  int align=1;
  uint_fast64_t w1, w2, si;                // window indexes, shift iterator
  if (align > 0) {                         // right
    w1 = k;
    w2 = nx;
    si = 0;
  } else if (align < 0) {                  // left
    w1 = 0;
    w2 = nx-k;
    si = 0;
  } else {                                 // center
    w1 = k/2;
    w2 = nx-(k/2);
    si = 0;
  }
  
  if (!truehasna) {
    if (!exact) {
      for (uint_fast64_t i=0; i<w1; i++) {  // loop over obs, potentially incomplete window from left
        w += x[i];                             // add current row to window sum
        ans[i+si] = w;                         // fill answer vector
        Rprintf("loop1: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, NA_REAL, x[i], i+si, w);
      }
      
      // extra single i=w1, and loop from w1+1 ?? to avoid: `if (i >= w1) w -= x[i-w1]` inside biggest loop
      //w += x[w1];
      //ans[w1] = w;                            // full window, so no fill, but nothing to remove yet
      // this could be merged into upper loop? as we do not fill NA there now, we do it later in separate loop, measure speed and adjust
      //Rprintf("expr1: iter %lu, x.in %8.3f, ans idx %lu, w %8.3f\n", w1, x[w1], w1, w);
      
      for (uint_fast64_t i=w1; i<w2; i++) { // loop over obs, complete window
        w -= x[i-w1];                         // remove leaving row from window sum
        w += x[i];                            // add current row to window sum
        ans[i+si] = w;                        // fill answer vector
        Rprintf("loop2: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-w1], x[i], i+si, w);
      }
      
      for (uint_fast64_t i=w2; i<nx; i++) {   // loop over obs, potentially incomplete window from right
        w -= x[i-w1];                         // remove leaving row from window sum
        ans[i+si] = w;                        // fill answer vector
        Rprintf("loop3: i %lu, x- %8.3f, x+ %8.3f, i.ans %lu, w %8.3f\n", i, x[i-w1], NA_REAL, i+si, w);
      }
    } else { // exact==TRUE
    
    }
    if (ISNAN(w)) { // ISNAN translate to C
      //if (nahasna==0) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
      // warning/print not thread safe
      w = 0.;
      truehasna = 1;
    } else {                                  // fill partial window, could be an option
      for (uint_fast64_t i=0; i<(w1-1); i++) {             // fill for align right/center
        ans[i+si] = fill;
      }
      for (uint_fast64_t i=w2; i<nx; i++) {   // fill for align left/center
        ans[i+si] = fill;
      }
    }
  }
  if (truehasna) {
    if (!exact) {
      int nc = 0;                               // NA counter within running window
      for (uint_fast64_t i=0; i<nx; i++) {      // loop over observations in column
        if (ISNAN(x[i])) nc++;                  // increment NA count in current window
        else w += x[i];                         // add only non-NA to window sum
        if (i >= k) {
          if (ISNAN(x[i-k])) nc--;              // decrement NA count in current window
          else w -= x[i-k];                     // remove only non-NA from window sum
        }
        if (i + 1 < k) ans[i] = fill;           // fill partial window
        else {
          if (nc == 0) ans[i] = w;              // calculate sum
          else if (nc == k) {                   // all values in window are NA
            if (narm) ans[i] = 0;               // for sum(NA, na.rm=T) result 0
            else ans[i] = NA_REAL;              // for sum(NA, na.rm=F) result NA
          }
          else {                                // some values in window are NA
            if (narm) ans[i] = w;               // calculate sum if NA present
            else ans[i] = NA_REAL;              // NA present and na.rm=FALSE result NA
          }
        }
      }
    } else { // exact==TRUE

    }
  }
}

void rollsumVectorAdaptive(double x[], uint_fast64_t nx, double ans[], int k[], double fill, bool exact, bool narm, bool hasna, bool nahasna) {
  double w = 0.; // running window sum
  bool truehasna = hasna; // flag to re-run if NAs detected
  int thisk, lastk, diffk;

  if (!truehasna) {
    if (!exact) {
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
          ans[i] = w;                 // calculate sum
        }
      }
    } else { // exact==TRUE
    
    }
    if (ISNAN(w)) { // ISNAN translate to C
      //if (nahasna==0) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
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
