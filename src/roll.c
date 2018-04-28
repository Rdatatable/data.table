#include "data.table.h"
#include <Rdefines.h>

static SEXP rollmeanVectorRaw(SEXP tmp, SEXP this, R_len_t xrows, R_len_t thisk, SEXP thiskl, double thisfill, Rboolean exact, Rboolean narm, Rboolean hasna, Rboolean adaptive) {
  double w = 0.;
  R_len_t m;
  Rboolean hasna_ = 0;

  if (hasna==1) hasna_ = 1;                          // cannot use if (hasna) because NA is MIN_INT so TRUE
  else {                                             // if (hasna==NA_LOGICAL || hasna==0): default assume no NA
    if (!adaptive) {                                 // non-adaptive: window is scalar
      if (!exact) {
        for (m=0; m<xrows; m++) {                    // loop over observations in column
          w += REAL(this)[m];                        // add current row to window sum
          if (m < thisk-1) REAL(tmp)[m] = thisfill;  // fill partial window
          else {
            if (m >= thisk) w -= REAL(this)[m-thisk];// remove leaving row from window sum
            REAL(tmp)[m] = w / thisk;                // calculate mean
          }
        }
      } else { // exact==TRUE                        // roundoff error correction, correction as in fastmean and base R but not identical
        R_len_t i;
        double s, t;
        for (m=0; m<xrows; m++) {                    // loop over observations in column
          s = 0.;
          t = 0.;
          if (m < thisk-1) REAL(tmp)[m] = thisfill;  // fill partial window
          else {
            for (i=m-thisk+1; i<=m; i++) s += REAL(this)[i];
            s /= thisk;
            if(R_FINITE(s)) {
              for (i=m-thisk+1; i<=m; i++) t += (REAL(this)[i] - s);
            }
            s += t/thisk;
          }
          REAL(tmp)[m] = s;
        }
      }
    } else {                                           // adaptive TRUE: variable window size
      if (!exact) {
        Rprintf("adaptive rolling functions suffers badly from floating point rounding error, consider using exact TRUE");
        R_len_t thiskl_, lastkl_, diffkl_, s;
        if (length(thiskl) != xrows) error("length of integer vector in 'n' is not equal to length of column in 'x', for adaptive TRUE those has to be equal");
        for (m=0; m<xrows; m++) {                        // loop over observations in column
          thiskl_ = INTEGER(thiskl)[m];
          w += REAL(this)[m];                            // add current row to window sum
          if (m + 1 < thiskl_) REAL(tmp)[m] = thisfill;    // fill partial window
          else {
            lastkl_ = INTEGER(thiskl)[m-1];
            diffkl_ = thiskl_ - lastkl_;
            //Rprintf("row %d, window %d, prev window %d, diff %d\n", m+1, thiskl_, lastkl_, diffkl_);
            if (diffkl_ > 1) {
              for (s=0; s<diffkl_-1; s++) {
                //Rprintf("row %d, diff %d (so adding %d), adding row %d, value %8.3f\n", m+1, diffkl_, diffkl_-1, m-thiskl_+s+1+1, REAL(this)[m-thiskl_+s+1]);
                w += REAL(this)[m-thiskl_+s+1];
              }
            } else if (diffkl_ < 1) {
              for (s=0; s<-diffkl_+1; s++) {
                if (m-thiskl_-s < 0) continue;
                //Rprintf("row %d, diff %d (so removing %d), removing row %d, value %8.3f\n", m+1, diffkl_, -diffkl_+1, m-thiskl_-s+1, REAL(this)[m-thiskl_-s]);
                w -= REAL(this)[m-thiskl_-s];
              }
            } else {
              //Rprintf("row %d, diff %d, no add no remove\n", m+1, diffkl_);
              // diffkl_ == 1: do not add or remove from window
            }
            REAL(tmp)[m] = w / thiskl_;                 // calculate mean
          }
        }
      } else { // exact==TRUE
        error("adaptive and exact not yet implemented");
      }
    }
    if (ISNAN(w)) {                                  // if NA in input then set re-run flag
      if (hasna==0) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
      w = 0.;
      hasna_ = 1;
    }
  }
  if (hasna_) {                                                   // hasna_ is non-NA so no `==` check required
    if (adaptive) error("hasNA and adaptive not yet implemented");
    if (exact) error("hasNA and exact not yet implemented");
    R_len_t nc = 0;
    if (!exact) {
      for (m=0; m<xrows; m++) {                                     // loop over observations in column
        if (ISNAN(REAL(this)[m])) nc++;                             // increment NA count in current window
        else w += REAL(this)[m];                                    // add only non-NA to window sum
        if (m >= thisk) {
          if (ISNAN(REAL(this)[m-thisk])) nc--;                     // decrement NA count in current window
          else w -= REAL(this)[m-thisk];                            // remove only non-NA from window sum
        }
        if (m + 1 < thisk) REAL(tmp)[m] = thisfill;                 // fill partial window
        else {
          if (nc == 0) REAL(tmp)[m] = w / thisk;                    // calculate mean
          else if (nc == thisk) {                                   // all values in window are NA
            if (narm) REAL(tmp)[m] = R_NaN;                         // for mean(NA, na.rm=T) result NaN
            else REAL(tmp)[m] = NA_REAL;                            // for mean(NA, na.rm=F) result NA
          }
          else {                                                    // some values in window are NA
            if (narm) REAL(tmp)[m] = w / (thisk - nc);              // calculate mean if NA present
            else REAL(tmp)[m] = NA_REAL;                            // NA present and na.rm=FALSE result NA
          }
        }
      }
    } else { // exact==TRUE // fastmean/base R floating point error correction
      error("hasNA and exact not yet implemented");
      /*R_len_t i;
      double s, t;
      for (m=0; m<xrows; m++) {                                     // loop over observations in column
        if (ISNAN(REAL(this)[m])) nc++;                             // increment NA count in current window
        else w += REAL(this)[m];                                    // add only non-NA to window sum
        if (m >= thisk) {
          if (ISNAN(REAL(this)[m-thisk])) nc--;                     // decrement NA count in current window
          else w -= REAL(this)[m-thisk];                            // remove only non-NA from window sum
        }
        if (m + 1 < thisk) REAL(tmp)[m] = thisfill;                 // fill partial window
        else {
          if (nc == 0) REAL(tmp)[m] = w / thisk;                    // calculate mean
          else if (nc == thisk) {                                   // all values in window are NA
            if (narm) REAL(tmp)[m] = R_NaN;                         // for mean(NA, na.rm=T) result NaN
            else REAL(tmp)[m] = NA_REAL;                            // for mean(NA, na.rm=F) result NA
          }
          else {                                                    // some values in window are NA
            if (narm) REAL(tmp)[m] = w / (thisk - nc);              // calculate mean if NA present
            else REAL(tmp)[m] = NA_REAL;                            // NA present and na.rm=FALSE result NA
          }
        }
      for (m=0; m<xrows; m++) {                    // loop over observations in column
        s = 0.;
        t = 0.;
        if (m < thisk-1) REAL(tmp)[m] = thisfill;  // fill partial window
        else {
          for (i=m-thisk+1; i<=m; i++) {
            s += REAL(this)[i];
          }
          s /= thisk;
          if(R_FINITE((double) s)) {
            for (i=m-thisk+1; i<=m; i++) t += (REAL(this)[i] - s);
          }
          s += t/thisk;
        }
        REAL(tmp)[m] = (double) s;
        }*/
    }
  }
  copyMostAttrib(this, tmp);
  
  return tmp;
}

SEXP rollmean(SEXP obj, SEXP k, SEXP fill, SEXP exact, SEXP align, SEXP narm, SEXP hasna, SEXP adaptive) {

  /*
    Ideally this function should be generic `roll()`
    accepting function name as argument and base on
    that redirecting to appropriate `roll*VectorRaw()`.
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
  
  if (LOGICAL(hasna)[0]==FALSE && LOGICAL(narm)[0])
    error("using hasNA FALSE and na.rm TRUE does not make sense, if you know there are NA values use hasNA TRUE, otherwise leave it as default NA");
    
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;              // allocate list to keep results
  
  thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;

  if (salign == LEFT) error("align 'left' not yet implemented");
  else if (salign == CENTER) error("align 'center' not yet implemented");

  for (i=0; i<nx; i++) {                                            // loop over columns
    //adaptive TODO check that every k list element is integer vector same length as x
    this = AS_NUMERIC(VECTOR_ELT(x, i));                            // extract column/vector from data.table/list
    xrows = length(this);                                           // for list input each vector can have different length
    #pragma omp parallel num_threads(MIN(getDTthreads(), nk))
    {
      #pragma omp for schedule(dynamic)
      for (j=0; j<nk; j++) {                                          // loop over multiple windows
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows)); // allocate answer vector for this column-window
        rollmeanVectorRaw(tmp, this, xrows, INTEGER(k)[j], VECTOR_ELT(kl, j), REAL(thisfill)[0], LOGICAL(exact)[0], LOGICAL(narm)[0], LOGICAL(hasna)[0], LOGICAL(adaptive)[0]);
      }
    }
  }
  
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
