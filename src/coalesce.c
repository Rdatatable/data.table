#include "data.table.h"

SEXP coalesce(SEXP x, SEXP values, SEXP inplace) {
  int JJ = length(values), protecti = 0, nx = length(x);

  if (!isVectorAtomic(x)) error("%s: argument 'x' must be an atomic vector", __func__);

  if (!isTrueFalse(inplace)) error("%s: argument '.inplace' must be TRUE or FALSE", __func__);
  bool binplace = LOGICAL(inplace)[0];

  int firstLengthMiss = lengthMiss(values, nx, true);
  if (firstLengthMiss >= 0) error("%s: Replacement %d has length %d but length(x) is %d. Only singletons will be recycled.", __func__, firstLengthMiss+1, length(VECTOR_ELT(values, firstLengthMiss)), nx);

  int firstTypeMiss = typeMiss(values, TYPEOF(x));
  if (firstTypeMiss >= 0) error("%s: Replacement %d has internal type %s but typeof(x) is %s. Please coerce before coalescing", __func__, firstTypeMiss+1, type2char(TYPEOF(VECTOR_ELT(values, firstTypeMiss))), type2char(TYPEOF(x)));

  if (INHERITS(x, char_integer64)) {
    int firstClassMiss = classMiss(values, char_integer64);
    if (firstClassMiss >= 0) error("%s: Replacement %d does not inherit interger64 class while 'x' do. Please align classes before coalescing", __func__, firstClassMiss+1);
  } else if (INHERITS(x, char_Date)) {
    int firstClassMiss = classMiss(values, char_Date);
    if (firstClassMiss >= 0) error("%s: Replacement %d does not inherit Date class while 'x' do. Please align classes before coalescing", __func__, firstClassMiss+1);
  } else if (INHERITS(x, char_factor)) {
    int firstClassMiss = classMiss(values, char_factor);
    if (firstClassMiss >= 0) error("%s: Replacement %d does not inherit factor class while 'x' do. Please align classes before coalescing", __func__, firstClassMiss+1);
    int firstLevelsMiss = levelsMiss(values, getAttrib(x, R_LevelsSymbol));
    if (firstLevelsMiss >= 0) error("%s: Replacement %d has different factor levels than 'x'. Coerce to character or align factor levels before coalescing", __func__, firstLevelsMiss+1);
  }

  double tic = 0;
  const bool verbose = GetVerbose();

  int *iwhich = malloc(nx*sizeof(iwhich)); // only nwhich will be filled, remaining space will store garbage
  if (!iwhich) error("%s: Unable to allocate memory", __func__); // # nocov
  int nwhich=0;

  if (verbose) tic = omp_get_wtime();
  switch(TYPEOF(x)) {
  case LGLSXP: {
    which_eq_int(LOGICAL(x), nx, iwhich, &nwhich, NA_LOGICAL, false);
  } break;
  case INTSXP: {
    which_eq_int(INTEGER(x), nx, iwhich, &nwhich, NA_INTEGER, false);
  } break;
  case REALSXP: {
    if (inherits(x,"integer64")) {
      which_eq_int64((int64_t *)REAL(x), nx, iwhich, &nwhich, NA_INTEGER64, false);
    } else {
      which_eq_double(REAL(x), nx, iwhich, &nwhich, NA_REAL, false);
    }
  } break;
  case STRSXP: {
    which_eq_char(x, nx, iwhich, &nwhich, NA_STRING, false);
  } break;
  default: {
    error("%s: Incompatible type: %s", __func__, type2char(TYPEOF(x)));
  }
  }
  if (verbose) Rprintf("%s: which NA took %.3fs\n", __func__, omp_get_wtime()-tic);

  if (nwhich==0) {
    if (verbose) Rprintf("%s: no NAs in input, skip further processing\n", __func__);
    return(binplace ? x : duplicate(x));
  }

  SEXP out = R_NilValue;
  if (verbose) tic = omp_get_wtime();
  if (binplace) out = x; else {
    out = PROTECT(duplicate(x)); protecti++;
  }
  if (verbose) Rprintf("%s: duplicate (if !inplace) took %.3fs\n", __func__, omp_get_wtime()-tic);

  int *valueslens = malloc(JJ*sizeof(valueslens));
  if (!valueslens) error("%s: Unable to allocate memory", __func__); // # nocov
  for (int j=0; j<JJ; j++) valueslens[j] = length(VECTOR_ELT(values, j));

  if (verbose) tic = omp_get_wtime();
  switch(TYPEOF(x)) {
  case LGLSXP: {
    int *out_ptr = LOGICAL(out);
    for (int i=0; i<nwhich; i++) {
      int j = 0;
      while (j < JJ) {
        int this = LOGICAL(VECTOR_ELT(values, j))[valueslens[j]==1 ? 0 : iwhich[i]];
        if (this != NA_LOGICAL) {
          out_ptr[iwhich[i]] = this;
          break;
        }
        j++;
      }
    }
  } break;
  case INTSXP: {
    int *out_ptr = INTEGER(out);
    for (int i=0; i<nwhich; i++) {
      int j = 0;
      while (j < JJ) {
        int this = INTEGER(VECTOR_ELT(values, j))[valueslens[j]==1 ? 0 : iwhich[i]];
        if (this != NA_INTEGER) {
          out_ptr[iwhich[i]] = this;
          break;
        }
        j++;
      }
    }
  } break;
  case REALSXP: {
    if (inherits(x,"integer64")) { // integer64
      int64_t *out_ptr = (int64_t *)REAL(out);
      for (int i=0; i<nwhich; i++) {
        int j = 0;
        while (j < JJ) {
          int64_t this = ((int64_t *)REAL(VECTOR_ELT(values, j)))[valueslens[j]==1 ? 0 : iwhich[i]];
          if (this != NA_INTEGER64) {
            out_ptr[iwhich[i]] = this;
            break;
          }
          j++;
        }
      }
    } else { // numeric
      double *out_ptr = REAL(out);
      for (int i=0; i<nwhich; i++) {
        int j = 0;
        while (j < JJ) {
          double this = REAL(VECTOR_ELT(values, j))[valueslens[j]==1 ? 0 : iwhich[i]];
          if (!ISNA(this)) {
            out_ptr[iwhich[i]] = this;
            break;
          }
          j++;
        }
      }
    }
  } break;
  case STRSXP: {
    for (int i=0; i<nwhich; i++) {
      int j = 0;
      while (j < JJ) {
        SEXP this = STRING_ELT(VECTOR_ELT(values, j), valueslens[j]==1 ? 0 : iwhich[i]);
        if (this != NA_STRING) {
          SET_STRING_ELT(out, iwhich[i], this);
          break;
        }
        j++;
      }
    }
  } break;
  default: {
    error("Internal error in %s -- incompatible type should %s should have been caught by now; please report.", __func__, type2char(TYPEOF(x))); // # nocov
  }
  }
  if (verbose) Rprintf("%s: loop over NA indices of x took %.3fs\n", __func__, omp_get_wtime()-tic);

  UNPROTECT(protecti);
  return out;
}
