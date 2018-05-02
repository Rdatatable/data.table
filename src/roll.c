//#include "roll.h"
#include "data.table.h"
#include <Rdefines.h>

/*typedef union
{
    bool value;
    bool na;
    } boolna;*/


//#define warning printf

//#define Rprintf printf
/*
// NA but not NaN
int R_IsNA(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] == 1954);
    }
    return 0;
}

// NaN but not NA
int R_IsNaN(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] != 1954);
    }
    return 0;
}
*/
//#include <R.h>

//#define NAND R_NaN
//#define NA_FLOAT64 NA_REAL

//static const double NAND = (double)NAN;
//static const double INFD = (double)INFINITY;
//static double NA_FLOAT64 = NA_FLOAT64_I64;

void rollmeanVector(double x[], int nx, double ans[], int k, double fill, bool exact, bool narm, bool hasna, bool nahasna) {
  Rprintf("rollmeanVector starting");
  double w = 0.; // running window sum
  bool truehasna = hasna; // flag to re-run if NAs detected
  _Bool verbose = 1;

  if (!truehasna) {
    if (!exact) {
      Rprintf("for (uint_fast64_t, ...)\n");
      for (int i=0; i<nx; i++) {  // loop over observations in column
        w += x[i];                          // add current row to window sum
        if (i < k-1) {
          if (verbose) Rprintf("rollmeanVector partial, filling with %8.3f", fill);
          ans[i] = fill;         // fill partial window
        }
        else {
          if (i >= k) w -= x[i-k];          // remove leaving row from window sum
          if (verbose) Rprintf("rollmeanVector window sum %8.3f, size %d", w, k);
          ans[i] = w / k;                   // calculate mean
        }
      }
    } else { // exact==TRUE
    
    }
    if (ISNAN(w)) { // ISNAN translate to C
      if (nahasna==0) warning("hasNA FALSE was used but NA values are present in input (or overflow?), re-running rolling function with extra care for NAs");
      w = 0.;
      truehasna = 1;
    }
  }
  if (truehasna) {
    if (!exact) {
      int nc = 0;                               // NA counter within running window
      for (int i=0; i<nx; i++) {      // loop over observations in column
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

void rollmeanVectorAdaptive(double x[], int nx, double ans[], int k[], double fill, bool exact, bool narm, bool hasna, bool nahasna) {
  double w = 0.; // running window sum
  bool truehasna = hasna; // flag to re-run if NAs detected
  int thisk, lastk, diffk;

  if (!truehasna) {
    if (!exact) {
      // move to rollmean when ans allocation is done:      if (length(thiskl) != xrows) error("length of integer vector in 'n' is not equal to length of column in 'x', for adaptive TRUE those has to be equal");
      for (int i=0; i<nx; i++) {                        // loop over observations in column
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
      if (nahasna==0) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
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
