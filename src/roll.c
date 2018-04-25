#include "data.table.h"
#include <Rdefines.h>

static SEXP rollmeanVectorRaw(SEXP tmp, SEXP this, R_len_t xrows, R_len_t thisk, double thisfill, Rboolean narm, Rboolean hasna) {
  // only for adaptive=FALSE at the moment due to thisk scalar R_len_t
  double w = 0.;
  R_len_t m;
  Rboolean hasna_ = 0;

  if (hasna==1) hasna_ = 1;                          // cannot simply if (hasna) because NA is -2147483648
  else {                                             // if (hasna==NA_LOGICAL || hasna==0): default assume no NA
    for (m=0; m<xrows; m++) {                        // loop over observations in column
      w += REAL(this)[m];                            // add current row to window sum
      if (m >= thisk) w -= REAL(this)[m-thisk];      // remove leaving row from window sum
      if (m + 1 < thisk) REAL(tmp)[m] = thisfill;    // fill partial window
      else REAL(tmp)[m] = w / thisk;                 // calculate mean
    }
    if (ISNAN(w)) {                                  // if NA in input then setup re-run flag
      if (hasna==0) warning("hasNA FALSE was used but NA values are present in input, re-running rolling function with extra care for NAs");
      w = 0.;
      hasna_ = 1;
    }
  }
  if (hasna_) {                                                   // hasna_ is non-NA so no `==` check required
    R_len_t nc = 0;
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
  }
  copyMostAttrib(this, tmp);
  
  return tmp;
}

SEXP rollmean(SEXP obj, SEXP k, SEXP fill, SEXP align, SEXP narm, SEXP hasna, SEXP adaptive) {
  
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
    while (i < nk && INTEGER(k)[i] >= 0) i++;
    if (i != nk)
      error("n must be non-negative integer values (>= 0)");
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

  if (!isLogical(narm) || length(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL)
    error("na.rm must be TRUE or FALSE");

  if (!isLogical(hasna) || length(hasna)!=1)
    error("hasNA must be TRUE, FALSE or NA");
  
  if (LOGICAL(hasna)[0]==FALSE && LOGICAL(narm)[0])
    error("using hasNA FALSE and na.rm TRUE does not make sense, if you know there are NA value use hasNA TRUE, otherwise leave it as default NA");
  
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;              // allocate list to keep results
  
  thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
  
  if (!LOGICAL(adaptive)[0]) {                                          // adaptive=FALSE
    if (salign == RIGHT) {                                              // align right scenario
      for (i=0; i<nx; i++) {                                            // loop over columns
        this = AS_NUMERIC(VECTOR_ELT(x, i));                            // extract column/vector from data.table/list
        xrows = length(this);                                           // for list input each vector can have different length
        #pragma omp parallel num_threads(MIN(getDTthreads(), nk))
        {
          #pragma omp for schedule(dynamic)
          for (j=0; j<nk; j++) {                                          // loop over multiple windows
            SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows)); // allocate answer vector for this column-window
            rollmeanVectorRaw(tmp, this, xrows, INTEGER(k)[j], REAL(thisfill)[0], LOGICAL(narm)[0], LOGICAL(hasna)[0]);
          }
        }
      }
    } else if (salign == LEFT) {                                        // align left scenario
      error("align 'left' not yet implemented");
    } else if (salign == CENTER) {                                      // align center scenario
      error("align 'center' not yet implemented");
    }
  } else if (LOGICAL(adaptive)[0]) {                                      // adaptive=TRUE
    error("adaptive TRUE not yet implemented");
    if (salign == RIGHT) {                                              // align right scenario
      error("align 'right' not yet implemented");
      //TODO // check that every k list element is integer vector same length as x - push down to for loop
    } else if (salign == LEFT) {                                        // align left scenario
      error("align 'left' not yet implemented");
    } else if (salign == CENTER) {                                      // align center scenario
      error("align 'center' not yet implemented");
    }
 }
  
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
