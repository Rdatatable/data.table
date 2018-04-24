#include "data.table.h"
#include <Rdefines.h>

SEXP rollmean(SEXP obj, SEXP k, SEXP fill, SEXP align, SEXP narm, SEXP adaptive) {
  
  R_len_t i=0, j, m, nx, nk, nc, xrows, thisk, protecti=0;
  SEXP x, kl, tmp=R_NilValue, this, ans, thisfill;
  double w;
  enum {RIGHT, CENTER, LEFT} salign = RIGHT;
  //Rboolean debug = 0;                                                     // temporary debug messages switch
  
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
  
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;                // allocate list to keep results
  
  thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
  
  if (!LOGICAL(adaptive)[0]) {                                          // adaptive=FALSE
    if (salign == RIGHT) {                                              // align right scenario
      for (i=0; i<nx; i++) {                                            // loop over columns
        this = AS_NUMERIC(VECTOR_ELT(x, i));                            // extract column/vector from data.table/list
        xrows = length(this);                                           // for list input each vector can have different length
        for (j=0; j<nk; j++) {                                          // loop over multiple windows
          thisk = INTEGER(k)[j];                                        // current window size
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows)); // allocate answer vector for this column-window
          w = 0.;                                                       // reset window's sum
          nc = 0;
          for (m=0; m<xrows; m++) {                                     // loop over observations in column
            if (ISNAN(REAL(this)[m])) nc += 1;
            else w += REAL(this)[m];                                    // add only non-NA to window sum
            if (m - thisk >= 0) {
              if (ISNAN(REAL(this)[m-thisk])) nc -= 1;
              else w -= REAL(this)[m-thisk];                            // remove only non-NA from window sum
            }
            if (m < thisk - 1) REAL(tmp)[m] = REAL(thisfill)[0];        // fill NA
            else {
              if (nc == 0) REAL(tmp)[m] = w / thisk;                    // calculate mean
              else if (nc > 0) {
                if (LOGICAL(narm)[0]) REAL(tmp)[m] = w / (thisk - nc);  // calculate mean if NA present
                else REAL(tmp)[m] = NA_REAL;                            // NA present and na.rm=FALSE result NA
              }
              else if (nc == thisk) {                                   // all values in window are NA
                if (LOGICAL(narm)[0]) REAL(tmp)[m] = R_NaN;             // for mean(NA, na.rm=T) result NaN
                else REAL(tmp)[m] = NA_REAL;                            // for mean(NA, na.rm=F) result NA
              }
            }
          }
          copyMostAttrib(this, tmp);
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
