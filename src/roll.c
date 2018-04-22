#include "data.table.h"
#include <Rdefines.h>

SEXP rollmean(SEXP obj, SEXP k, SEXP fill, SEXP align, SEXP adaptive) {
  
  R_len_t i=0, j, m, nx, nk, xrows, thisk, protecti=0;
  SEXP x, kl, tmp=R_NilValue, this, ans, thisfill;
  double w;
  enum {RIGHT, CENTER, LEFT} salign = RIGHT;
  Rboolean debug = 0;                                                   // temporary debug messages switch
  
  if (!length(obj)) return(obj);                                        // NULL, list()
  if (isVectorAtomic(obj)) {
    x = allocVector(VECSXP, 1);
    SET_VECTOR_ELT(x, 0, obj);
  } else {
    x = obj;
  }

  if (!isNewList(x))
    error("x must be a list, data.frame or data.table");

  if (length(fill) != 1)
    error("fill must be a vector of length 1");

  if (!strcmp(CHAR(STRING_ELT(align, 0)), "right"))
    salign = RIGHT;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "center"))
    salign = CENTER;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "left"))
    salign = LEFT;
  else
    error("Internal error: invalid align argument in rolling function, should have been caught before. Please report.");

  if (!isLogical(adaptive) || length(adaptive) != 1 || LOGICAL(adaptive)[0] == NA_LOGICAL)
    error("adaptive must be logical TRUE/FALSE.");

  nx = length(x);  
  i = 0;                                                                // check that every column is double/integer type
  while (i < nx && (isReal(VECTOR_ELT(x, i)) || isInteger(VECTOR_ELT(x, i)))) i++;
  if (i != nx)
    error("x must be list, data.frame or data.table of numeric types");

  if (!LOGICAL(adaptive)[0]) {                                            // validating n input for adaptive=FALSE
    nk = length(k);
    if (nk == 0)                                                          // check that window is not empty
      error("n must be non 0 length integer vector or list");
    //TODO move R's n=as.integer(n) to C
    if (!isInteger(k))                                                    // check that k is integer vector, not a list
      error("Internal error: n must be integer");
    i = 0;                                                                // check that all window values non-negative
    while (i < nk && INTEGER(k)[i] >= 0) i++;
    if (i != nk)
      error("n must be non-negative integer values (>= 0)");
  } else if (LOGICAL(adaptive)[0]) {                                      // validating n input for adaptive=TRUE
    if (isVectorAtomic(k)) {                                              // if not-list then wrap into list
      kl = allocVector(VECSXP, 1);
      SET_VECTOR_ELT(kl, 0, k);
    } else {
      kl = k;
    }
    nk = length(kl);
    //TODO // check that every k list element is integer vector same length as x
  } else {
    error("Internal error: invalid adaptive argument in rolling function, should have been caught before. Please report.");
  }
  
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;              // allocate list to keep results
  
  thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;

  if (debug) Rprintf("DEBUG: placeholder debug message\n");

  if (!LOGICAL(adaptive)[0]) {                                            // adaptive=FALSE
    if (salign == RIGHT) {                                                // align right scenario
      for (i=0; i<nx; i++) {                                              // loop over columns
        this = AS_NUMERIC(VECTOR_ELT(x, i));                              // extract column/vector from data.table/list
        xrows = length(this);                                             // for list input each vector can have different length
        for (j=0; j<nk; j++) {                                            // loop over multiple windows
          thisk = INTEGER(k)[j];                                          // current window size
          SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows));   // allocate answer vector for this column-window
          w = 0.;                                                         // reset window's sum
          for (m=0; m<xrows; m++) {                                       // loop over observations in column
            w += REAL(this)[m];                                           // add current row to window sum
            if (m - thisk >= 0) w -= REAL(this)[m-thisk];                 // remove row from window sum that is outside of the window already
            if (m < thisk - 1) REAL(tmp)[m] = REAL(thisfill)[0];          // fill NA
            else REAL(tmp)[m] = w / thisk;                                // calculate rollmean for a row
          }
          copyMostAttrib(this, tmp);
        }
      }
    } else if (salign == CENTER) {                                        // align center scenario
      error("align 'center' not yet implemented");
    } else if (salign == LEFT) {                                          // align left scenario
      error("align 'left' not yet implemented");
    }
  } else if (LOGICAL(adaptive)[0]) {                                      // adaptive=TRUE
    error("adaptive TRUE not yet implemented");
    if (salign == RIGHT) {                                                // align right scenario
    } else if (salign == CENTER) {                                        // align center scenario
      error("align 'center' not yet implemented");
    } else if (salign == LEFT) {                                          // align left scenario
      error("align 'left' not yet implemented");
    }
  }
  
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
