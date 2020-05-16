#include "data.table.h"

// like base::p{max,min}, but for sum
SEXP psum(SEXP x, SEXP narmArg) {
  if (!isNewList(x)) error(_("Internal error: x must be a list")); // # nocov
  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL) error(_("na.rm must be TRUE or FALSE"));

  int J=LENGTH(x);
  if (J == 0) {
    error(_("Empty input"));
  } else if (J == 1) {
    SEXP xj0 = VECTOR_ELT(x, 0);
    if (TYPEOF(xj0) == VECSXP) { // e.g. psum(.SD)
      x = duplicate(xj0);
      J = LENGTH(xj0);
    } else {
      // na.rm doesn't matter -- input --> output
      return duplicate(xj0);
    }
  }
  SEXPTYPE outtype = INTSXP;
  int n = -1;
  for (int j=0; j<J; j++) {
    SEXP xj=VECTOR_ELT(x, j);
    switch(TYPEOF(xj)) {
    case LGLSXP: case INTSXP:
      if (isFactor(xj)) {
        error(_("%s not meaningful for factors"), "psum");
      }
      break;
    case REALSXP:
      if (INHERITS(xj, char_integer64)) {
        error(_("integer64 input not supported"));
      }
      if (outtype == INTSXP) { // bump if this is the first numeric we've seen
        outtype = REALSXP;
      }
      break;
    case CPLXSXP:
      if (outtype != CPLXSXP) { // only bump if we're not already complex
        outtype = CPLXSXP;
      }
      break;
    default:
      error(_("Only logical, numeric and complex inputs are supported for %s"), "psum");
    }
    if (n >= 0) {
      int nj = LENGTH(xj);
      if (n == 1 && nj > 1) {
        n = nj; // singleton comes first, vector comes later [psum(1, 1:4)]
      } else if (nj != 1 && nj != n) {
        error(_("Inconsistent input lengths -- first found %d, but %d element has length %d. Only singletons will be recycled."), n, j+1, nj);
      }
    } else { // initialize
      n = LENGTH(xj);
    }
  }

  SEXP out = PROTECT(allocVector(outtype, n));
  if (n == 0) {
    UNPROTECT(1);
    return(out);
  }
  if (LOGICAL(narmArg)[0]) {
    // initialize to NA to facilitate all-NA rows --> NA output
    writeNA(out, 0, n);
    switch (outtype) {
    case INTSXP: {
      int *outp = INTEGER(out);
      for (int j=0; j<J; j++) {
        SEXP xj = VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        int *xjp = INTEGER(xj); // INTEGER is the same as LOGICAL
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i; // recycling for singletons
          if (xjp[xi] != NA_INTEGER) { // NA_LOGICAL is the same
            if (outp[i] == NA_INTEGER) {
              outp[i] = xjp[xi];
            } else {
              if ((xjp[xi] > 0 && INT_MAX - xjp[xi] < outp[i]) ||
                  (xjp[xi] < 0 && INT_MIN - xjp[xi] > outp[i])) { // overflow
                error(_("Inputs have exceeded .Machine$integer.max=%d in absolute value; please cast to numeric first and try again"), INT_MAX);
              }
              outp[i] += xjp[xi];
            }
          } // else remain as NA
        }
      }
    } break;
    case REALSXP: { // REALSXP; special handling depending on whether each input is int/numeric
      double *outp = REAL(out);
      for (int j=0; j<J; j++) {
        SEXP xj = VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (xjp[xi] != NA_INTEGER) {
              outp[i] = ISNAN(outp[i]) ? xjp[xi] : outp[i] + xjp[xi];
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (!ISNAN(xjp[xi])) {
              outp[i] = ISNAN(outp[i]) ? xjp[xi] : outp[i] + xjp[xi];
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    case CPLXSXP: {
      Rcomplex *outp = COMPLEX(out);
      for (int j=0; j<J; j++) {
        SEXP xj = VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: { // integer/numeric only increment the real part
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (xjp[xi] != NA_INTEGER) {
              if (ISNAN_COMPLEX(outp[i])) {
                outp[i].r = xjp[xi];
                outp[i].i = 0;
              } else {
                outp[i].r += xjp[xi];
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (!ISNAN(xjp[xi])) {
              if (ISNAN_COMPLEX(outp[i])) {
                outp[i].r = xjp[xi];
                outp[i].i = 0;
              } else {
                outp[i].r += xjp[xi];
              }
            }
          }
        } break;
        case CPLXSXP: {
          Rcomplex *xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            // can construct complex vectors with !is.na(Re) & is.na(Im) --
            //   seems dubious to me, since print(z) only shows NA, not 3 + NAi
            if (!ISNAN_COMPLEX(xjp[xi])) {
              outp[i].r = ISNAN(outp[i].r) ? xjp[xi].r : outp[i].r + xjp[xi].r;
              outp[i].i = ISNAN(outp[i].i) ? xjp[xi].i : outp[i].i + xjp[xi].i;
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    default:
      error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
    }
  } else { // na.rm=FALSE
    switch (outtype) {
    case INTSXP: {
      int *outp = INTEGER(out);
      SEXP xj0 = VECTOR_ELT(x, 0);
      int nj0 = LENGTH(xj0);
      int *xj0p = INTEGER(xj0);
      for (int i=0; i<n; i++) outp[i] = xj0p[nj0 == 1 ? 0 : i];
      for (int j=1; j<J; j++) { // J>=2 by now
        SEXP xj = VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          if (outp[i] == NA_INTEGER) {
            continue;
          }
          int xi = nj == 1 ? 0 : i;
          if (xjp[xi] == NA_INTEGER) {
            outp[i] = NA_INTEGER;
          } else if ((xjp[xi] > 0 && INT_MAX - xjp[xi] < outp[i]) ||
                     (xjp[xi] < 0 && INT_MIN - xjp[xi] > outp[i])) {
            warning(_("Inputs have exceeded .Machine$integer.max=%d in absolute value; returning NA. Please cast to numeric first to avoid this."), INT_MAX);
            outp[i] = NA_INTEGER;
          } else {
            outp[i] += xjp[xi];
          }
        }
      }
    } break;
    case REALSXP: {
      double *outp = REAL(out);
      SEXP xj0 = VECTOR_ELT(x, 0);
      int nj0 = LENGTH(xj0);
      if (TYPEOF(xj0) == REALSXP) {
        double *xj0p = REAL(xj0);
        for (int i=0; i<n; i++) outp[i] = xj0p[nj0 == 1 ? 0 : i];
      } else {
        int *xj0p = INTEGER(xj0);
        for (int i=0; i<n; i++) outp[i] = xj0p[nj0 == 1 ? 0 : i];
      }
      for (int j=1; j<J; j++) {
        SEXP xj=VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (ISNAN(outp[i])) {
              continue;
            }
            int xi = nj == 1 ? 0 : i;
            outp[i] = xjp[xi] == NA_INTEGER ? NA_REAL : outp[i] + xjp[xi];
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (ISNAN(outp[i])) {
              continue;
            }
            int xi = nj == 1 ? 0 : i;
            outp[i] = ISNAN(xjp[xi]) ? NA_REAL : outp[i] + xjp[xi];
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    case CPLXSXP: {
      Rcomplex *outp = COMPLEX(out);
      SEXP xj0 = VECTOR_ELT(x, 0);
      int nj0 = LENGTH(xj0);
      switch (TYPEOF(xj0)) {
      case LGLSXP: case INTSXP: {
        int *xj0p = INTEGER(xj0);
        for (int i=0; i<n; i++) {
          outp[i].r = xj0p[nj0 == 1 ? 0 : i];
          outp[i].i = 0;
        }
      } break;
      case REALSXP: {
        double *xj0p = REAL(xj0);
        for (int i=0; i<n; i++) {
          outp[i].r = xj0p[nj0 == 1 ? 0 : i];
          outp[i].i = 0;
        }
      } break;
      case CPLXSXP: {
        Rcomplex *xj0p = COMPLEX(xj0);
        for (int i=0; i<n; i++) {
          outp[i].r = xj0p[nj0 == 1 ? 0 : i].r;
          outp[i].i = xj0p[nj0 == 1 ? 0 : i].i;
        }
      } break;
      default:
        error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
      }
      for (int j=1; j<J; j++) {
        SEXP xj=VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              int xi = nj == 1 ? 0 : i;
              if (xjp[xi] == NA_INTEGER) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else {
                outp[i].r += xjp[xi];
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              int xi = nj == 1 ? 0 : i;
              if (ISNAN(xjp[xi])) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else {
                outp[i].r += xjp[xi];
              }
            }
          }
        } break;
        case CPLXSXP: {
          Rcomplex *xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              int xi = nj == 1 ? 0 : i;
              if (ISNAN_COMPLEX(xjp[xi])) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else {
                outp[i].r += xjp[xi].r;
                outp[i].i += xjp[xi].i;
              }
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    default:
      error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
    }
  }

  UNPROTECT(1);
  return out;
}

SEXP pprod(SEXP x, SEXP narmArg) {
  if (!isNewList(x)) error(_("Internal error: x must be a list")); // # nocov
  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL) error(_("na.rm must be TRUE or FALSE"));

  int J=LENGTH(x);
  if (J == 0) {
    error(_("Empty input"));
  } else if (J == 1) {
    SEXP xj0 = VECTOR_ELT(x, 0);
    if (TYPEOF(xj0) == VECSXP) { // e.g. pprod(.SD)
      x = duplicate(xj0);
      J = LENGTH(xj0);
    } else {
      // na.rm doesn't matter -- input --> output
      return duplicate(xj0);
    }
  }
  SEXPTYPE outtype = INTSXP;
  int n = -1;
  for (int j=0; j<J; j++) {
    SEXP xj=VECTOR_ELT(x, j);
    switch(TYPEOF(xj)) {
    case LGLSXP: case INTSXP:
      if (isFactor(xj)) {
        error(_("%s not meaningful for factors"), "pprod");
      }
      break;
    case REALSXP:
      if (INHERITS(xj, char_integer64)) {
        error(_("integer64 input not supported"));
      }
      if (outtype == INTSXP) { // bump if this is the first numeric we've seen
        outtype = REALSXP;
      }
      break;
    case CPLXSXP:
      if (outtype != CPLXSXP) { // only bump if we're not already complex
        outtype = CPLXSXP;
      }
      break;
    default:
      error(_("Only logical, numeric and complex inputs are supported for %s"), "pprod");
    }
    if (n >= 0) {
      int nj = LENGTH(xj);
      if (n == 1 && nj > 1) {
        n = nj; // singleton comes first, vector comes later [pprod(1, 1:4)]
      } else if (nj != 1 && nj != n) {
        error(_("Inconsistent input lengths -- first found %d, but %d element has length %d. Only singletons will be recycled."), n, j+1, nj);
      }
    } else { // initialize
      n = LENGTH(xj);
    }
  }

  SEXP out = PROTECT(allocVector(outtype, n));
  if (n == 0) {
    UNPROTECT(1);
    return(out);
  }
  if (LOGICAL(narmArg)[0]) {
    // initialize to NA to facilitate all-NA rows --> NA output
    writeNA(out, 0, n);
    switch (outtype) {
    case INTSXP: {
      int *outp = INTEGER(out);
      for (int j=0; j<J; j++) {
        SEXP xj = VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        int *xjp = INTEGER(xj); // INTEGER is the same as LOGICAL
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i; // recycling for singletons
          if (xjp[xi] != NA_INTEGER) { // NA_LOGICAL is the same
            if (outp[i] == NA_INTEGER) {
              outp[i] = xjp[xi];
            } else {
              if ((outp[i] > 0 && (xjp[xi] > INT_MAX/outp[i] || xjp[xi] < INT_MIN/outp[i])) || // overflow -- be careful of inequalities and flipping signs
                  (outp[i] < 0 && (xjp[xi] < INT_MAX/outp[i] || xjp[xi] > INT_MIN/outp[i]))) {
                error(_("Inputs have exceeded .Machine$integer.max=%d in absolute value; please cast to numeric first and try again"), INT_MAX);
              }
              outp[i] *= xjp[xi];
            }
          } // else remain as NA
        }
      }
    } break;
    case REALSXP: { // REALSXP; special handling depending on whether each input is int/numeric
      double *outp = REAL(out);
      for (int j=0; j<J; j++) {
        SEXP xj = VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (xjp[xi] != NA_INTEGER) {
              outp[i] = ISNAN(outp[i]) ? xjp[xi] : outp[i] * xjp[xi];
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (!ISNAN(xjp[xi])) {
              outp[i] = ISNAN(outp[i]) ? xjp[xi] : outp[i] * xjp[xi];
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    case CPLXSXP: {
      Rcomplex *outp = COMPLEX(out);
      for (int j=0; j<J; j++) {
        SEXP xj = VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: { // integer/numeric only increment the real part
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (xjp[xi] != NA_INTEGER) {
              if (ISNAN_COMPLEX(outp[i])) {
                outp[i].r = xjp[xi];
                outp[i].i = 0;
              } else {
                outp[i].r *= xjp[xi];
                outp[i].i *= xjp[xi];
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (!ISNAN(xjp[xi])) {
              if (ISNAN_COMPLEX(outp[i])) {
                outp[i].r = xjp[xi];
                outp[i].i = 0;
              } else {
                outp[i].r *= xjp[xi];
                outp[i].i *= xjp[xi];
              }
            }
          }
        } break;
        case CPLXSXP: {
          Rcomplex *xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            // can construct complex vectors with !is.na(Re) & is.na(Im) --
            //   seems dubious to me, since print(z) only shows NA, not 3 + NAi
            if (!ISNAN_COMPLEX(xjp[xi])) {
              if (ISNAN(outp[i].r) || ISNAN(outp[i].i)) {
                outp[i].r = xjp[xi].r;
                outp[i].i = xjp[xi].i;
              } else {
                double tmp=outp[i].r; // can't simultaneously assign Re&Im, need to remember Re pre-update
                outp[i].r = outp[i].r * xjp[xi].r - outp[i].i * xjp[xi].i;
                outp[i].i = outp[i].i * xjp[xi].r + tmp * xjp[xi].i;
              }
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    default:
      error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
    }
  } else { // na.rm=FALSE
    switch (outtype) {
    case INTSXP: {
      int *outp = INTEGER(out);
      SEXP xj0 = VECTOR_ELT(x, 0);
      int nj0 = LENGTH(xj0);
      int *xj0p = INTEGER(xj0);
      for (int i=0; i<n; i++) outp[i] = xj0p[nj0 == 1 ? 0 : i];
      for (int j=1; j<J; j++) { // J>=2 by now
        SEXP xj = VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          if (outp[i] == NA_INTEGER) {
            continue;
          }
          int xi = nj == 1 ? 0 : i;
          if (xjp[xi] == NA_INTEGER) {
            outp[i] = NA_INTEGER;
          } else if ((outp[i] > 0 && (xjp[xi] > INT_MAX/outp[i] || xjp[xi] < INT_MIN/outp[i])) || // overflow -- be careful of inequalities and flipping signs
                     (outp[i] < 0 && (xjp[xi] < INT_MAX/outp[i] || xjp[xi] > INT_MIN/outp[i]))) {
            warning(_("Inputs have exceeded .Machine$integer.max=%d in absolute value; returning NA. Please cast to numeric first to avoid this."), INT_MAX);
            outp[i] = NA_INTEGER;
          } else {
            outp[i] *= xjp[xi];
          }
        }
      }
    } break;
    case REALSXP: {
      double *outp = REAL(out);
      SEXP xj0 = VECTOR_ELT(x, 0);
      int nj0 = LENGTH(xj0);
      if (TYPEOF(xj0) == REALSXP) {
        double *xj0p = REAL(xj0);
        for (int i=0; i<n; i++) outp[i] = xj0p[nj0 == 1 ? 0 : i];
      } else {
        int *xj0p = INTEGER(xj0);
        for (int i=0; i<n; i++) outp[i] = xj0p[nj0 == 1 ? 0 : i];
      }
      for (int j=1; j<J; j++) {
        SEXP xj=VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (ISNAN(outp[i])) {
              continue;
            }
            int xi = nj == 1 ? 0 : i;
            outp[i] = xjp[xi] == NA_INTEGER ? NA_REAL : outp[i] * xjp[xi];
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (ISNAN(outp[i])) {
              continue;
            }
            int xi = nj == 1 ? 0 : i;
            outp[i] = ISNAN(xjp[xi]) ? NA_REAL : outp[i] * xjp[xi];
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    case CPLXSXP: {
      Rcomplex *outp = COMPLEX(out);
      SEXP xj0 = VECTOR_ELT(x, 0);
      int nj0 = LENGTH(xj0);
      switch (TYPEOF(xj0)) {
      case LGLSXP: case INTSXP: {
        int *xj0p = INTEGER(xj0);
        for (int i=0; i<n; i++) {
          outp[i].r = xj0p[nj0 == 1 ? 0 : i];
          outp[i].i = 0;
        }
      } break;
      case REALSXP: {
        double *xj0p = REAL(xj0);
        for (int i=0; i<n; i++) {
          outp[i].r = xj0p[nj0 == 1 ? 0 : i];
          outp[i].i = 0;
        }
      } break;
      case CPLXSXP: {
        Rcomplex *xj0p = COMPLEX(xj0);
        for (int i=0; i<n; i++) {
          outp[i].r = xj0p[nj0 == 1 ? 0 : i].r;
          outp[i].i = xj0p[nj0 == 1 ? 0 : i].i;
        }
      } break;
      default:
        error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
      }
      for (int j=1; j<J; j++) {
        SEXP xj=VECTOR_ELT(x, j);
        int nj = LENGTH(xj);
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              int xi = nj == 1 ? 0 : i;
              if (xjp[xi] == NA_INTEGER) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else {
                outp[i].r *= xjp[xi];
                outp[i].i *= xjp[xi];
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              int xi = nj == 1 ? 0 : i;
              if (ISNAN(xjp[xi])) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else {
                outp[i].r *= xjp[xi];
                outp[i].i *= xjp[xi];
              }
            }
          }
        } break;
        case CPLXSXP: {
          Rcomplex *xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              int xi = nj == 1 ? 0 : i;
              if (ISNAN_COMPLEX(xjp[xi])) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else {
                double tmp=outp[i].r; // see na.rm=TRUE branch
                outp[i].r = outp[i].r * xjp[xi].r - outp[i].i * xjp[xi].i;
                outp[i].i = tmp * xjp[xi].i + outp[i].i * xjp[xi].r;
              }
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    default:
      error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
    }
  }

  UNPROTECT(1);
  return out;
}

SEXP pany(SEXP x, SEXP narmArg) {
  if (!isNewList(x)) error(_("Internal error: x must be a list")); // # nocov
  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL) error(_("na.rm must be TRUE or FALSE"));

  int J=LENGTH(x);
  if (J == 0) {
    error(_("Empty input"));
  } else if (J == 1) {
    SEXP xj0 = VECTOR_ELT(x, 0);
    if (TYPEOF(xj0) == VECSXP) { // e.g. psum(.SD)
      x = duplicate(xj0);
      J = LENGTH(xj0);
    } else {
      // maybe need to do coercion to logical
      if (TYPEOF(xj0) == LGLSXP) {
        return duplicate(xj0);
      } else {
        SEXP out = PROTECT(coerceVector(xj0, LGLSXP));
        UNPROTECT(1);
        return out;
      }
    }
  }
  int n = -1;
  for (int j=0; j<J; j++) {
    SEXP xj=VECTOR_ELT(x, j);
    switch(TYPEOF(xj)) {
    case LGLSXP: case INTSXP:
      if (isFactor(xj)) {
        error(_("%s not meaningful for factors"), "pany");
      }
      break;
    case REALSXP:
      if (INHERITS(xj, char_integer64)) {
        error(_("integer64 input not supported"));
      }
      break;
    case CPLXSXP:
      break;
    default:
      error(_("Only logical, numeric and complex inputs are supported for %s"), "pany");
    }
    if (n >= 0) {
      int nj = LENGTH(xj);
      if (n == 1 && nj > 1) {
        n = nj; // singleton comes first, vector comes later [psum(1, 1:4)]
      } else if (nj != 1 && nj != n) {
        error(_("Inconsistent input lengths -- first found %d, but %d element has length %d. Only singletons will be recycled."), n, j+1, nj);
      }
    } else { // initialize
      n = LENGTH(xj);
    }
  }

  SEXP out = PROTECT(allocVector(LGLSXP, n));
  int *outp = LOGICAL(out);
  if (n == 0) {
    UNPROTECT(1);
    return(out);
  }
  if (LOGICAL(narmArg)[0]) {
    // initialize to NA to facilitate all-NA rows --> NA output
    writeNA(out, 0, n);
    for (int j=0; j<J; j++) {
      SEXP xj = VECTOR_ELT(x, j);
      int nj = LENGTH(xj);
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i;
          if (xjp[xi] != NA_INTEGER) {
            if (outp[i] == NA_LOGICAL) {
              outp[i] = xjp[xi] == 0 ? 0 : 1;
            } else if (outp[i] == 0 && xjp[xi] != 0) { // no change if already 1 or next is 0
              outp[i] = 1;
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (!ISNAN(xjp[xi])) {
              if (outp[i] == NA_LOGICAL) {
                outp[i] = xjp[xi] == 0 ? 0 : 1;
              } else if (outp[i] == 0 && xjp[xi] != 0) {
                outp[i] = 1;
              }
            }
          }
        } break;
        case CPLXSXP: {
          Rcomplex *xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (!ISNAN_COMPLEX(xjp[xi])) {
              if (outp[i] == NA_LOGICAL) {
                outp[i] = xjp[xi].r == 0 && xjp[xi].i == 0 ? 0 : 1;
              } else if (outp[i] == 0 && (xjp[xi].r != 0 || xjp[xi].i != 0)) { // no change if already 1 or next is 0
                outp[i] = 1;
              }
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
  } else { // na.rm=FALSE
    SEXP xj0 = VECTOR_ELT(x, 0);
    int nj0 = LENGTH(xj0);
    switch (TYPEOF(xj0)) {
    case LGLSXP: case INTSXP: {
      int *xj0p = INTEGER(xj0);
      for (int i=0; i<n; i++) {
        int xi = nj0 == 1 ? 0 : i;
        outp[i] = xj0p[xi] == 0 ? 0 : (xj0p[xi] == NA_INTEGER ? NA_LOGICAL : 1);
      }
    } break;
    case REALSXP: {
      double *xj0p = REAL(xj0);
      for (int i=0; i<n; i++) {
        int xi = nj0 == 1 ? 0 : i;
        outp[i] = xj0p[xi] == 0 ? 0 : (ISNAN(xj0p[xi]) ? NA_LOGICAL : 1);
      }
    } break;
    case CPLXSXP: {
      Rcomplex *xj0p = COMPLEX(xj0);
      for (int i=0; i<n; i++) {
        int xi = nj0 == 1 ? 0 : i;
        outp[i] = xj0p[xi].r == 0 && xj0p[xi].i == 0 ? 0 :
              (ISNAN(xj0p[xi].r || ISNAN(xj0p[xi].i)) ? NA_LOGICAL : 1);
      }
    } break;
    default:
      error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
    }
    for (int j=1; j<J; j++) {
      SEXP xj=VECTOR_ELT(x, j);
      int nj = LENGTH(xj);
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i;
          if (xjp[xi] == NA_INTEGER && outp[i] == 0) {
            outp[i] = NA_LOGICAL;
          } else if (outp[i] != 1 && xjp[xi] != 0) { // outp[i] NA also erased by xjp[xi] != 0
            outp[i] = 1;
          }
        }
      } break;
      case REALSXP: {
        double *xjp = REAL(xj);
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i;
          if (ISNAN(xjp[xi]) && outp[i] == 0) {
            outp[i] = NA_LOGICAL;
          } else if (outp[i] != 1 && xjp[xi] != 0) {
            outp[i] = 1;
          }
        }
      } break;
      case CPLXSXP: {
        Rcomplex *xjp = COMPLEX(xj);
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i;
          if (ISNAN_COMPLEX(xjp[xi]) && outp[i] == 0) {
            outp[i] = NA_LOGICAL;
          } else if (outp[i] !=1 && (xjp[xi].r != 0 || xjp[xi].i != 0)) {
            outp[i] = 1;
          }
        }
      } break;
      default:
        error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
      }
    }
  }

  UNPROTECT(1);
  return out;
}

SEXP pall(SEXP x, SEXP narmArg) {
  if (!isNewList(x)) error(_("Internal error: x must be a list")); // # nocov
  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL) error(_("na.rm must be TRUE or FALSE"));

  int J=LENGTH(x);
  if (J == 0) {
    error(_("Empty input"));
  } else if (J == 1) {
    SEXP xj0 = VECTOR_ELT(x, 0);
    if (TYPEOF(xj0) == VECSXP) {
      x = duplicate(xj0);
      J = LENGTH(xj0);
    } else {
      // maybe need to do coercion to logical
      if (TYPEOF(xj0) == LGLSXP) {
        return duplicate(xj0);
      } else {
        SEXP out = PROTECT(coerceVector(xj0, LGLSXP));
        UNPROTECT(1);
        return out;
      }
    }
  }
  int n = -1;
  for (int j=0; j<J; j++) {
    SEXP xj=VECTOR_ELT(x, j);
    switch(TYPEOF(xj)) {
    case LGLSXP: case INTSXP:
      if (isFactor(xj)) {
        error(_("%s not meaningful for factors"), "pall");
      }
      break;
    case REALSXP:
      if (INHERITS(xj, char_integer64)) {
        error(_("integer64 input not supported"));
      }
      break;
    case CPLXSXP:
      break;
    default:
      error(_("Only logical, numeric and complex inputs are supported for %s"), "pall");
    }
    if (n >= 0) {
      int nj = LENGTH(xj);
      if (n == 1 && nj > 1) {
        n = nj; // singleton comes first, vector comes later [psum(1, 1:4)]
      } else if (nj != 1 && nj != n) {
        error(_("Inconsistent input lengths -- first found %d, but %d element has length %d. Only singletons will be recycled."), n, j+1, nj);
      }
    } else { // initialize
      n = LENGTH(xj);
    }
  }

  SEXP out = PROTECT(allocVector(LGLSXP, n));
  int *outp = LOGICAL(out);
  if (n == 0) {
    UNPROTECT(1);
    return(out);
  }
  if (LOGICAL(narmArg)[0]) {
    // initialize to NA to facilitate all-NA rows --> NA output
    writeNA(out, 0, n);
    for (int j=0; j<J; j++) {
      SEXP xj = VECTOR_ELT(x, j);
      int nj = LENGTH(xj);
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i;
          if (xjp[xi] != NA_INTEGER) {
            if (outp[i] == NA_LOGICAL) {
              outp[i] = xjp[xi] == 0 ? 0 : 1;
            } else if (outp[i] == 1 && xjp[xi] == 0) { // no change if already 0 or next is 1
              outp[i] = 0;
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (!ISNAN(xjp[xi])) {
              if (outp[i] == NA_LOGICAL) {
                outp[i] = xjp[xi] == 0 ? 0 : 1;
              } else if (outp[i] == 1 && xjp[xi] == 0) {
                outp[i] = 1;
              }
            }
          }
        } break;
        case CPLXSXP: {
          Rcomplex *xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            int xi = nj == 1 ? 0 : i;
            if (!ISNAN_COMPLEX(xjp[xi])) {
              if (outp[i] == NA_LOGICAL) {
                outp[i] = xjp[xi].r == 0 && xjp[xi].i == 0 ? 0 : 1;
              } else if (outp[i] == 1 && xjp[xi].r == 0 && xjp[xi].i == 0) { // no change if already 0 or next is 1
                outp[i] = 0;
              }
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
  } else { // na.rm=FALSE
    SEXP xj0 = VECTOR_ELT(x, 0);
    int nj0 = LENGTH(xj0);
    switch (TYPEOF(xj0)) {
    case LGLSXP: case INTSXP: {
      int *xj0p = INTEGER(xj0);
      for (int i=0; i<n; i++) {
        int xi = nj0 == 1 ? 0 : i;
        outp[i] = xj0p[xi] == 0 ? 0 : (xj0p[xi] == NA_INTEGER ? NA_LOGICAL : 1);
      }
    } break;
    case REALSXP: {
      double *xj0p = REAL(xj0);
      for (int i=0; i<n; i++) {
        int xi = nj0 == 1 ? 0 : i;
        outp[i] = xj0p[xi] == 0 ? 0 : (ISNAN(xj0p[xi]) ? NA_LOGICAL : 1);
      }
    } break;
    case CPLXSXP: {
      Rcomplex *xj0p = COMPLEX(xj0);
      for (int i=0; i<n; i++) {
        int xi = nj0 == 1 ? 0 : i;
        outp[i] = xj0p[xi].r == 0 && xj0p[xi].i == 0 ? 0 :
              (ISNAN(xj0p[xi].r || ISNAN(xj0p[xi].i)) ? NA_LOGICAL : 1);
      }
    } break;
    default:
      error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
    }
    for (int j=1; j<J; j++) {
      SEXP xj=VECTOR_ELT(x, j);
      int nj = LENGTH(xj);
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i;
          if (xjp[xi] == NA_INTEGER && outp[i] == 1) { // NA stays NA, 0 stays 0
            outp[i] = NA_LOGICAL;
          } else if (outp[i] != 0 && xjp[xi] == 0) { // NA and 1 become 0
            outp[i] = 0;
          }
        }
      } break;
      case REALSXP: {
        double *xjp = REAL(xj);
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i;
          if (ISNAN(xjp[xi]) && outp[i] == 1) {
            outp[i] = NA_LOGICAL;
          } else if (outp[i] != 0 && xjp[xi] == 0) {
            outp[i] = 0;
          }
        }
      } break;
      case CPLXSXP: {
        Rcomplex *xjp = COMPLEX(xj);
        for (int i=0; i<n; i++) {
          int xi = nj == 1 ? 0 : i;
          if (ISNAN_COMPLEX(xjp[xi]) && outp[i] == 1) {
            outp[i] = NA_LOGICAL;
          } else if (outp[i] != 0 && xjp[xi].r == 0 && xjp[xi].i == 0) {
            outp[i] = 0;
          }
        }
      } break;
      default:
        error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
      }
    }
  }

  UNPROTECT(1);
  return out;
}
