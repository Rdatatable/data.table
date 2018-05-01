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
      if (nahasna==0) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
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
