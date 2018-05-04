#include "rollR.h"

SEXP rollmean(SEXP obj, SEXP k, SEXP fill, SEXP exact, SEXP align, SEXP narm, SEXP hasna, SEXP adaptive) {

  /*
    Ideally this function should be generic `roll()`
    accepting function name as argument and base on
    that redirecting to appropriate `roll*Vector()`.
    This will greatly reduce repetition.
   */
  
  R_len_t i=0, j, nx, nk, protecti=0;
  SEXP x, kl, tmp=R_NilValue, this, ans, thisfill;
  enum {RIGHT, CENTER, LEFT} salign = RIGHT;
  
  if (!length(obj)) return(obj);                                          // empty input: NULL, list()
  if (isVectorAtomic(obj)) {                                              // wrap atomic vector into list()
    x = PROTECT(allocVector(VECSXP, 1)); protecti++;
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
      kl = PROTECT(allocVector(VECSXP, 1)); protecti++;
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

  bool hasnatf, nahasna; // decode once, pass extra flag if hasNA was neither TRUE/FALSE but NA so no SEXP passed to roll*Vector()
  nahasna = (LOGICAL(hasna)[0]==NA_LOGICAL) ? 1 : 0; // this is actually used only to raise warning if hasNA was FALSE but NA were present, if hasNA was NA then silent re-run happens
  hasnatf = (LOGICAL(hasna)[0]==0 || nahasna==1) ? 0 : 1; // we do not want NA here, instead of one variable T/F/NA we pass two T/F variables
  
  if (LOGICAL(hasna)[0]==FALSE && LOGICAL(narm)[0])
    error("using hasNA FALSE and na.rm TRUE does not make sense, if you know there are NA values use hasNA TRUE, otherwise leave it as default NA");
    
  thisfill = PROTECT(coerceVector(fill, REALSXP)); protecti++;

  if (salign == LEFT) error("align 'left' not yet implemented");
  else if (salign == CENTER) error("align 'center' not yet implemented");

  ans = PROTECT(allocVector(VECSXP, nk * nx)); protecti++;         // allocate list to keep results
  uint_fast64_t xrows[nx-1];                                       // to not recalculate `length(x[[i]])` we store it in extra array
  for (i=0; i<nx; i++) {
    xrows[i] = length(VECTOR_ELT(x, i));                           // for list input each vector can have different length
    for (j=0; j<nk; j++) {
      if (LOGICAL(adaptive)[0]) {                                  // extra input validation
        if (i > 0 && (xrows[i]!=xrows[i-1]))                       // variable length list input not allowed for adaptive roll
          error("Adaptive rolling function can only process 'x' having equal length of elements, like data.table or data.frame. If you want to call rolling function on list having variable length of elements call it for each field separately.");
        if (length(VECTOR_ELT(kl, j))!=xrows[0])                   // check that length of integer vectors in n list match to xrows[0] ([0] and not [i] because there is above check for equal xrows)
          error("Length of integer vector(s) provided as list to 'n' argument must have equal length to number of observations provided in 'x'.");
      }
      SET_VECTOR_ELT(ans, i*nk+j, allocVector(REALSXP, xrows[i])); // allocate answer vector for this column-window
    }
  }

  if (nx==1 && nk==1) {                                               // no need to init openmp for single thread call
    this = AS_NUMERIC(VECTOR_ELT(x, 0));
    tmp = VECTOR_ELT(ans, 0);
    if (!LOGICAL(adaptive)[0]) rollmeanVector(REAL(this), xrows[0], REAL(tmp), INTEGER(k)[0], REAL(thisfill)[0], LOGICAL(exact)[0], LOGICAL(narm)[0], hasnatf, nahasna);
    else rollmeanVectorAdaptive(REAL(this), xrows[0], REAL(tmp), INTEGER(VECTOR_ELT(kl, 0)), REAL(thisfill)[0], LOGICAL(exact)[0], LOGICAL(narm)[0], hasnatf, nahasna);
  } else {
    for (i=0; i<nx; i++) {                                            // loop over columns
      this = AS_NUMERIC(VECTOR_ELT(x, i));                            // extract column/vector from data.table/list
      #pragma omp parallel num_threads(MIN(getDTthreads(), nk))
      {
        #pragma omp for schedule(dynamic)
        for (j=0; j<nk; j++) {                                          // loop over multiple windows
          tmp = VECTOR_ELT(ans, i*nk+j);
          if (!LOGICAL(adaptive)[0]) rollmeanVector(REAL(this), xrows[i], REAL(tmp), INTEGER(k)[j], REAL(thisfill)[0], LOGICAL(exact)[0], LOGICAL(narm)[0], hasnatf, nahasna);
          else rollmeanVectorAdaptive(REAL(this), xrows[i], REAL(tmp), INTEGER(VECTOR_ELT(kl, j)), REAL(thisfill)[0], LOGICAL(exact)[0], LOGICAL(narm)[0], hasnatf, nahasna);
        }
      }
    }
  }
  
  UNPROTECT(protecti);
  return isVectorAtomic(obj) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
