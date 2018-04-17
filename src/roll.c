#include "data.table.h"
#include <Rdefines.h>

SEXP rollmean(SEXP obj, SEXP k, SEXP fill) {

  //size_t size;
  R_len_t i=0, j, m, nx, nk, xrows, thisk, protecti=0, sum=0;
  SEXP x, tmp=R_NilValue, this, ans, thisfill; //, class;
  //unsigned long long *dthisfill;
  if (!length(obj)) return(obj); // NULL, list()
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

  nx = length(x); nk = length(k);
  i=0;
  while(i < nk && INTEGER(k)[i] >= 0) i++;
  if (i != nk)
    error("n must be non-negative integer values (>= 0)");
  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;

  thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;
  
  // loop over columns in 'obj' input
  for (i=0; i<nx; i++) {
    Rprintf("rollmean.c: column: %d\n", i);
    this  = VECTOR_ELT(x, i);
    if (!isReal(this)) error("Currently only 'double' type is supported in rollmean");
    //size  = SIZEOF(this);
    xrows = length(this);
      
    // loop over multiple windows provided as 'k' input
    for (j=0; j<nk; j++) {
      Rprintf("rollmean.c: window: %d\n", j);
      thisk = (xrows >= INTEGER(k)[j]) ? INTEGER(k)[j] : xrows;
      tmp = allocVector(REALSXP, xrows);
      SET_VECTOR_ELT(ans, i*nk+j, tmp);
      //// shift - to remove
      //if (xrows - INTEGER(k)[j] >= 0) {
      //  memcpy((char *)DATAPTR(tmp)+(INTEGER(k)[j]*size),
      //       (char *)DATAPTR(this),
      //       (xrows-INTEGER(k)[j])*size);
      //}
      //for (m=0; m<thisk; m++) {
      //  Rprintf("rollmean.c: fill: %d\n", m);
      //  REAL(tmp)[m] = REAL(thisfill)[0];
      //}
      sum = 0;
      for (m=0; m<xrows; m++) {
        Rprintf("rollmean.c: row: %d\n", m);
        sum += REAL(VECTOR_ELT(tmp, m));
        if (m - INTEGER(k)[j] > 0) {
          sum -= REAL(VECTOR_ELT(tmp, m - INTEGER(k)[j] + 1));
        }
        Rprintf("rollmean.c: row: current sum: %8.3f\n", sum);
        if (m<thisk) {
          REAL(tmp)[m] = REAL(thisfill)[0];
        } else {
          //s[thisgrp] += REAL(x)[ix];
          REAL(tmp)[m] = REAL(thisfill)[0]; //TODO
          //REAL(tmp)[m] = sum / INTEGER(k)[j]
        }
      }
      copyMostAttrib(this, tmp);
    }
  }
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
