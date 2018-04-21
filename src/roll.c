#include "data.table.h"
#include <Rdefines.h>

SEXP rollmean(SEXP obj, SEXP k, SEXP fill, SEXP align) {
  
  R_len_t i=0, j, m, nx, nk, xrows, protecti=0;
  SEXP x, tmp=R_NilValue, this, ans, thisfill;
  double w;
  enum {RIGHT, CENTER, LEFT} salign = RIGHT;
  Rboolean debug = 0;                                                   // temporary debug messages switch
  
  if (!length(obj)) return(obj);                                        // NULL, list()
  if (isVectorAtomic(obj)) {
    x = allocVector(VECSXP, 1);
    SET_VECTOR_ELT(x, 0, obj);
  } else x = obj;
  if (!isNewList(x))
    error("x must be a list, data.frame or data.table");
  if (!isInteger(k))
    error("Internal error: n must be integer");
  if (length(fill) != 1)
    error("fill must be a vector of length 1");
  
  if (!strcmp(CHAR(STRING_ELT(align, 0)), "right"))  salign = RIGHT;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "center")) salign = CENTER;
  else if (!strcmp(CHAR(STRING_ELT(align, 0)), "left")) salign = LEFT;
  else error("Internal error: invalid align argument in rolling function, should have been caught before. Please report.");
  
  nx = length(x); nk = length(k);
  i = 0;
  while (i < nk && INTEGER(k)[i] >= 0) i++;
  if (i != nk) error("n must be non-negative integer values (>= 0)");   // check that all window values non-negative
  i = 0;
  while (i < nx && (isReal(VECTOR_ELT(x, i)) || isInteger(VECTOR_ELT(x, i)))) i++;
  if (i != nx) error("x must be list of 'double' types");               // check that every column is double/integer type
  
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;              // allocate list to keep results
  
  thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
  
  if (salign == RIGHT) {                                                // align right scenario
    for (i=0; i<nx; i++) {                                              // loop over columns
      if (debug) Rprintf("DEBUG: rollmean.c: col %d\n", i);
      this = AS_NUMERIC(VECTOR_ELT(x, i));                              // extract column/vector from data.table/list
      xrows = length(this);                                             // for list input each vector can have different length
      for (j=0; j<nk; j++) {                                            // loop over multiple windows
        if (debug) Rprintf("DEBUG: rollmean.c: col %d: win %d\n", i, j);
        SET_VECTOR_ELT(ans, i*nk+j, tmp=allocVector(REALSXP, xrows));   // allocate answer vector for this column-window
        w = 0.;                                                         // reset window's sum
        for (m=0; m<xrows; m++) {                                       // loop over observations in column
          if (debug) Rprintf("DEBUG: rollmean.c: col %d: win %d: row %d\n", i, j, m);
          w += REAL(this)[m];                                           // add current row to window sum        
          if (m - INTEGER(k)[j] >= 0) w -= REAL(this)[m-INTEGER(k)[j]]; // remove row from window sum that is outside of the window already
          if (m < INTEGER(k)[j] - 1) REAL(tmp)[m] = REAL(thisfill)[0];  // fill NA
          else REAL(tmp)[m] = w / INTEGER(k)[j];                        // calculate rollmean for a row
          if (debug) Rprintf("DEBUG: rollmean.c: col %d: win %d: row %d: ans: %8.3f\n", i, j, m, REAL(tmp)[m]);
        }
        copyMostAttrib(this, tmp);
      }
    }
  } else if (salign == CENTER) {                                        // align center scenario
    error("align 'center' not yet implemented");
  } else if (salign == LEFT) {                                          // align left scenario
    error("align 'left' not yet implemented");
  }
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
