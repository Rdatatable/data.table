#include "data.table.h"
#include <Rdefines.h>

static void rollmeanVector(double x[], uint_fast64_t nx, double ans[], int k, double fill, _Bool exact, _Bool narm, _Bool hasna, _Bool nahasna) {
  double w = 0.; // running window sum
  _Bool truehasna = hasna; // flag to re-run if NAs detected

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

SEXP rollmean(SEXP obj, SEXP k, SEXP fill, SEXP exact, SEXP align, SEXP narm, SEXP hasna, SEXP adaptive) {

  /*
    Ideally this function should be generic `roll()`
    accepting function name as argument and base on
    that redirecting to appropriate `roll*Vector()`.
    This will greatly reduce repetition.
   */
  
  R_len_t i=0, j, nx, nk, xrows, protecti=0;
  SEXP x, kl, tmp=R_NilValue, this, ans, thisfill;
  enum {RIGHT, CENTER, LEFT} salign = RIGHT;
  
  if (!length(obj)) return(obj);                                          // empty input: NULL, list()
  if (isVectorAtomic(obj)) {                                              // wrap atomic vector into list()
    x = allocVector(VECSXP, 1);
    SET_VECTOR_ELT(x, 0, obj);
  } else {
    x = obj;
  }

  if (!isNewList(x))
    error("x must be a list, data.frame or data.table");

  nx = length(x);
  i = 0;                                                                  // check that every column is double/integer type
  while (i < nx && (isReal(VECTOR_ELT(x, i)) || isInteger(VECTOR_ELT(x, i)))) i++;
  if (i != nx)
    error("x must be list, data.frame or data.table of numeric types");

  nk = length(k);
  if (nk == 0)                                                            // check that window is not empty
    error("n must be non 0 length");

  if (!isLogical(adaptive) || length(adaptive) != 1 || LOGICAL(adaptive)[0] == NA_LOGICAL)
    error("adaptive must be logical TRUE or FALSE");

  if (!LOGICAL(adaptive)[0]) {                                            // validating n input for adaptive=FALSE
    if (isNewList(k))
      error("n must be integer, list is accepted for adaptive TRUE");
    //TODO move R's n=as.integer(n) to C
    if (!isInteger(k))                                                    // check that k is integer vector
      error("n must be integer");
    i = 0;                                                                // check that all window values non-negative
    while (i < nk && INTEGER(k)[i] > 0) i++;
    if (i != nk)
      error("n must be positive integer values (> 0)");
  } else if (LOGICAL(adaptive)[0]) {                                      // validating n input for adaptive=TRUE
    if (isVectorAtomic(k)) {                                              // if not-list then wrap into list
      kl = allocVector(VECSXP, 1);
      SET_VECTOR_ELT(kl, 0, k);
      nk = 1;
    } else {
      kl = k;
    }
    //TODO move R's n=as.integer(n) to C
    i = 0;                                                                // check that every column is integer type
    while (i < nk && isInteger(VECTOR_ELT(kl, i))) i++;
    if (i != nk)
      error("n must be list of integer vectors when adaptive TRUE");
  }
  
  if (length(fill) != 1)
    error("fill must be a vector of length 1");

  if (!strcmp(CHAR(STRING_ELT(align, 0)), "right"))
    salign = RIGHT;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "center"))
    salign = CENTER;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "left"))
    salign = LEFT;
  else
    error("Internal error: invalid align argument in rolling function, should have been caught before. please report to data.table issue tracker.");

  if (!isLogical(exact) || length(exact)!=1 || LOGICAL(exact)[0]==NA_LOGICAL)
    error("exact must be TRUE or FALSE");
  
  if (!isLogical(narm) || length(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL)
    error("na.rm must be TRUE or FALSE");

  if (!isLogical(hasna) || length(hasna)!=1)
    error("hasNA must be TRUE, FALSE or NA");

  _Bool hasna_, nahasna; // decode once, pass extra flag if hasNA was neither TRUE/FALSE but NA so no SEXP passed to roll*Vector()
  nahasna = (LOGICAL(hasna)[0]==NA_LOGICAL) ? 1 : 0; // this is actually used only to raise warning if hasNA was FALSE but NA were present, if hasNA was NA then silent re-run happens
  hasna_ = (LOGICAL(hasna)[0]==0 || nahasna==1) ? 0 : 1; // we do not want NA here, instead of one variable T/F/NA we pass two T/F variables
  
  if (LOGICAL(hasna)[0]==FALSE && LOGICAL(narm)[0])
    error("using hasNA FALSE and na.rm TRUE does not make sense, if you know there are NA values use hasNA TRUE, otherwise leave it as default NA");
    
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;              // allocate list to keep results
  
  thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;

  if (salign == LEFT) error("align 'left' not yet implemented");
  else if (salign == CENTER) error("align 'center' not yet implemented");
  
  for (i=0; i<nx; i++) {                                            // loop over columns
    this = AS_NUMERIC(VECTOR_ELT(x, i));                            // extract column/vector from data.table/list
    xrows = length(this);                                           // for list input each vector can have different length
    #pragma omp parallel num_threads(MIN(getDTthreads(), nk))
    {
      #pragma omp for schedule(dynamic)
      for (j=0; j<nk; j++) {                                          // loop over multiple windows
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows)); // allocate answer vector for this column-window
        rollmeanVector(REAL(this), xrows, REAL(tmp), INTEGER(k)[j], REAL(thisfill)[0], LOGICAL(exact)[0], LOGICAL(narm)[0], hasna_, nahasna);
      }
    }
  }
  
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
