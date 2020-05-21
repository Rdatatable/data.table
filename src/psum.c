#include "data.table.h"

// like base::p{max,min}, but for sum
SEXP psum(SEXP x, SEXP narmArg) {
  if (!isNewList(x))
    error(_("Internal error: x must be a list")); // # nocov

  SEXP xj;
  int J=LENGTH(x);
  if (J == 0) {
    error(_("Empty input"));
  } else if (J == 1) {
    xj = VECTOR_ELT(x, 0);
    if (TYPEOF(xj) == VECSXP) { // e.g. psum(.SD)
      return psum(xj, narmArg);
    } else {
      // na.rm doesn't matter -- input --> output
      return xj;
    }
  }

  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL)
    error(_("na.rm must be TRUE or FALSE"));

  SEXPTYPE outtype = INTSXP;
  int n = -1, nj;
  for (int j=0; j<J; j++) {
    xj = VECTOR_ELT(x, j);
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
      nj = LENGTH(xj);
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
      int *outp = INTEGER(out), *xjp;
      for (int j=0; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        xjp = INTEGER(xj); // INTEGER is the same as LOGICAL
        for (int i=0; i<n; i++) {
          if (xjp[i & mask] != NA_INTEGER) { // NA_LOGICAL is the same
            if (outp[i] == NA_INTEGER) {
              outp[i] = xjp[i & mask];
            } else {
              if ((xjp[i & mask] > 0 && INT_MAX - xjp[i & mask] < outp[i]) ||
                  (xjp[i & mask] < 0 && INT_MIN - xjp[i & mask] > outp[i])) { // overflow
                error(_("Inputs have exceeded .Machine$integer.max=%d in absolute value; please cast to numeric first and try again"), INT_MAX);
              }
              outp[i] += xjp[i & mask];
            }
          } // else remain as NA
        }
      }
    } break;
    case REALSXP: { // REALSXP; special handling depending on whether each input is int/numeric
      double *outp = REAL(out), *xjp; // since outtype is REALSXP, there's at least one REAL column
      for (int j=0; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (xjp[i & mask] != NA_INTEGER) {
              outp[i] = ISNAN(outp[i]) ? xjp[i & mask] : outp[i] + xjp[i & mask];
            }
          }
        } break;
        case REALSXP: {
          xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN(xjp[i & mask])) {
              outp[i] = ISNAN(outp[i]) ? xjp[i & mask] : outp[i] + xjp[i & mask];
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    case CPLXSXP: {
      Rcomplex *outp = COMPLEX(out), *xjp;
      for (int j=0; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: { // integer/numeric only increment the real part
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (xjp[i & mask] != NA_INTEGER) {
              if (ISNAN_COMPLEX(outp[i])) {
                outp[i].r = xjp[i & mask];
                outp[i].i = 0;
              } else {
                outp[i].r += xjp[i & mask];
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN(xjp[i & mask])) {
              if (ISNAN_COMPLEX(outp[i])) {
                outp[i].r = xjp[i & mask];
                outp[i].i = 0;
              } else {
                outp[i].r += xjp[i & mask];
              }
            }
          }
        } break;
        case CPLXSXP: {
          xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            // can construct complex vectors with !is.na(Re) & is.na(Im) --
            //   seems dubious to me, since print(z) only shows NA, not 3 + NAi
            if (!ISNAN_COMPLEX(xjp[i & mask])) {
              outp[i].r = ISNAN(outp[i].r) ? xjp[i & mask].r : outp[i].r + xjp[i & mask].r;
              outp[i].i = ISNAN(outp[i].i) ? xjp[i & mask].i : outp[i].i + xjp[i & mask].i;
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
      int *outp = INTEGER(out), *xjp;
      xj = VECTOR_ELT(x, 0);
      nj = LENGTH(xj);
      const int mask = nj == 1 ? 0 : INT_MAX;
      xjp = INTEGER(xj);
      for (int i=0; i<n; i++) outp[i] = xjp[i & mask];
      for (int j=1; j<J; j++) { // J>=2 by now
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          if (outp[i] == NA_INTEGER) {
            continue;
          }
          if (xjp[i & mask] == NA_INTEGER) {
            outp[i] = NA_INTEGER;
          } else if ((xjp[i & mask] > 0 && INT_MAX - xjp[i & mask] < outp[i]) ||
                     (xjp[i & mask] < 0 && INT_MIN - xjp[i & mask] > outp[i])) {
            warning(_("Inputs have exceeded .Machine$integer.max=%d in absolute value; returning NA. Please cast to numeric first to avoid this."), INT_MAX);
            outp[i] = NA_INTEGER;
          } else {
            outp[i] += xjp[i & mask];
          }
        }
      }
    } break;
    case REALSXP: {
      double *outp = REAL(out), *xjp;
      xj = VECTOR_ELT(x, 0);
      nj = LENGTH(xj);
      const int mask = nj == 1 ? 0 : INT_MAX;
      if (TYPEOF(xj) == REALSXP) {
        xjp = REAL(xj);
        for (int i=0; i<n; i++) outp[i] = xjp[nj == 1 ? 0 : i];
      } else {
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          outp[i] = xjp[i & mask] == NA_INTEGER ? NA_REAL : xjp[i & mask];
        }
      }
      for (int j=1; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (ISNAN(outp[i])) {
              continue;
            }
            outp[i] = xjp[i & mask] == NA_INTEGER ? NA_REAL : outp[i] + xjp[i & mask];
          }
        } break;
        case REALSXP: {
          xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (ISNAN(outp[i])) {
              continue;
            }
            outp[i] = ISNAN(xjp[i & mask]) ? NA_REAL : outp[i] + xjp[i & mask];
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    case CPLXSXP: {
      Rcomplex *outp = COMPLEX(out), *xjp;
      xj = VECTOR_ELT(x, 0);
      nj = LENGTH(xj);
      const int mask = nj == 1 ? 0 : INT_MAX;
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          outp[i].r = xjp[i & mask] == NA_INTEGER ? NA_REAL : xjp[i & mask];
          outp[i].i = xjp[i & mask] == NA_INTEGER ? NA_REAL : 0;
        }
      } break;
      case REALSXP: {
        double *xjp = REAL(xj);
        for (int i=0; i<n; i++) {
          outp[i].r = xjp[i & mask];
          outp[i].i = ISNAN(xjp[i & mask]) ? NA_REAL : 0;
        }
      } break;
      case CPLXSXP: {
        xjp = COMPLEX(xj);
        for (int i=0; i<n; i++) {
          outp[i].r = xjp[i & mask].r;
          outp[i].i = xjp[i & mask].i;
        }
      } break;
      default:
        error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
      }
      for (int j=1; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              if (xjp[i & mask] == NA_INTEGER) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else {
                outp[i].r += xjp[i & mask];
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              if (ISNAN(xjp[i & mask])) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else {
                outp[i].r += xjp[i & mask];
              }
            }
          }
        } break;
        case CPLXSXP: {
          xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              if (ISNAN_COMPLEX(xjp[i & mask])) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else {
                outp[i].r += xjp[i & mask].r;
                outp[i].i += xjp[i & mask].i;
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
  if (!isNewList(x))
    error(_("Internal error: x must be a list")); // # nocov

  SEXP xj;
  int J=LENGTH(x);
  if (J == 0) {
    error(_("Empty input"));
  } else if (J == 1) {
    SEXP xj = VECTOR_ELT(x, 0);
    if (TYPEOF(xj) == VECSXP) {
      return pprod(xj, narmArg);
    } else {
      // na.rm doesn't matter -- input --> output
      return xj;
    }
  }

  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL)
    error(_("na.rm must be TRUE or FALSE"));

  SEXPTYPE outtype = INTSXP;
  int n = -1, nj;
  for (int j=0; j<J; j++) {
    xj = VECTOR_ELT(x, j);
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
      nj = LENGTH(xj);
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
      int *outp = INTEGER(out), *xjp;
      for (int j=0; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        xjp = INTEGER(xj); // INTEGER is the same as LOGICAL
        for (int i=0; i<n; i++) {
          if (xjp[i & mask] != NA_INTEGER) { // NA_LOGICAL is the same
            if (outp[i] == NA_INTEGER) {
              outp[i] = xjp[i & mask];
            } else {
              if ((outp[i] > 0 && (xjp[i & mask] > INT_MAX/outp[i] || xjp[i & mask] < INT_MIN/outp[i])) || // overflow -- be careful of inequalities and flipping signs
                  (outp[i] == -1 && (xjp[i & mask] > INT_MAX || xjp[i & mask] <= INT_MIN)) ||              // ASSUMPTION: INT_MIN= -INT_MAX - 1
                  (outp[i] < -1 && (xjp[i & mask] < INT_MAX/outp[i] || xjp[i & mask] > INT_MIN/outp[i]))) {
                error(_("Inputs have exceeded .Machine$integer.max=%d in absolute value; please cast to numeric first and try again"), INT_MAX);
              }
              outp[i] *= xjp[i & mask];
            }
          } // else remain as NA
        }
      }
    } break;
    case REALSXP: { // REALSXP; special handling depending on whether each input is int/numeric
      double *outp = REAL(out), *xjp;
      for (int j=0; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (xjp[i & mask] != NA_INTEGER) {
              outp[i] = ISNAN(outp[i]) ? xjp[i & mask] : outp[i] * xjp[i & mask];
            }
          }
        } break;
        case REALSXP: {
          xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN(xjp[i & mask])) {
              outp[i] = ISNAN(outp[i]) ? xjp[i & mask] : outp[i] * xjp[i & mask];
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    case CPLXSXP: {
      Rcomplex *outp = COMPLEX(out), *xjp;
      for (int j=0; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: { // integer/numeric only increment the real part
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (xjp[i & mask] != NA_INTEGER) {
              if (ISNAN_COMPLEX(outp[i])) {
                outp[i].r = xjp[i & mask];
                outp[i].i = 0;
              } else {
                outp[i].r *= xjp[i & mask];
                outp[i].i *= xjp[i & mask];
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN(xjp[i & mask])) {
              if (ISNAN_COMPLEX(outp[i])) {
                outp[i].r = xjp[i & mask];
                outp[i].i = 0;
              } else {
                outp[i].r *= xjp[i & mask];
                outp[i].i *= xjp[i & mask];
              }
            }
          }
        } break;
        case CPLXSXP: {
          xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            // can construct complex vectors with !is.na(Re) & is.na(Im) --
            //   seems dubious to me, since print(z) only shows NA, not 3 + NAi
            if (!ISNAN_COMPLEX(xjp[i & mask])) {
              if (ISNAN(outp[i].r) || ISNAN(outp[i].i)) {
                outp[i].r = xjp[i & mask].r;
                outp[i].i = xjp[i & mask].i;
              } else {
                double tmp=outp[i].r; // can't simultaneously assign Re&Im, need to remember Re pre-update
                outp[i].r = outp[i].r * xjp[i & mask].r - outp[i].i * xjp[i & mask].i;
                outp[i].i = outp[i].i * xjp[i & mask].r + tmp * xjp[i & mask].i;
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
      int *outp = INTEGER(out), *xjp;
      xj = VECTOR_ELT(x, 0);
      nj = LENGTH(xj);
      const int mask = nj == 1 ? 0 : INT_MAX;
      xjp = INTEGER(xj);
      for (int i=0; i<n; i++) outp[i] = xjp[i & mask];
      for (int j=1; j<J; j++) { // J>=2 by now
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          if (outp[i] == NA_INTEGER) {
            continue;
          }
          if (xjp[i & mask] == NA_INTEGER) {
            outp[i] = NA_INTEGER;
          } else if (outp[i] == 0) {
            continue; // 0 is a steady state, except when xjp[i & mask] is missing
          } else {
            if ((outp[i] > 0 && (xjp[i & mask] > INT_MAX/outp[i] || xjp[i & mask] < INT_MIN/outp[i])) || // overflow -- be careful of inequalities and flipping signs
                (outp[i] == -1 && (xjp[i & mask] > INT_MAX || xjp[i & mask] <= INT_MIN)) ||              // ASSUMPTION: INT_MIN= -INT_MAX - 1
                (outp[i] < -1 && (xjp[i & mask] < INT_MAX/outp[i] || xjp[i & mask] > INT_MIN/outp[i]))) {
              warning(_("Inputs have exceeded .Machine$integer.max=%d in absolute value; returning NA. Please cast to numeric first to avoid this."), INT_MAX);
              outp[i] = NA_INTEGER;
            } else {
              outp[i] *= xjp[i & mask];
            }
          }
        }
      }
    } break;
    case REALSXP: {
      double *outp = REAL(out), *xjp;
      xj = VECTOR_ELT(x, 0);
      nj = LENGTH(xj);
      const int mask = nj == 1 ? 0 : INT_MAX;
      if (TYPEOF(xj) == REALSXP) {
        xjp = REAL(xj);
        for (int i=0; i<n; i++) outp[i] = xjp[nj == 1 ? 0 : i];
      } else {
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          outp[i] = xjp[i & mask] == NA_INTEGER ? NA_REAL : xjp[i & mask];
        }
      }
      for (int j=1; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (ISNAN(outp[i])) {
              continue;
            }
            if (xjp[i & mask] == NA_INTEGER) {
              outp[i] = NA_REAL;
            } else if (outp[i] == 0) {
              continue;
            } else {
              outp[i] *= xjp[i & mask];
            }
          }
        } break;
        case REALSXP: {
          xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (ISNAN(outp[i])) {
              continue;
            }
            if (ISNAN(xjp[i & mask])) {
              outp[i] = NA_REAL;
            } else if (outp[i] == 0) {
              continue;
            } else {
              outp[i] *= xjp[i & mask];
            }
          }
        } break;
        default:
          error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
        }
      }
    } break;
    case CPLXSXP: {
      Rcomplex *outp = COMPLEX(out), *xjp;
      xj = VECTOR_ELT(x, 0);
      nj = LENGTH(xj);
      const int mask = nj == 1 ? 0 : INT_MAX;
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        for (int i=0; i<n; i++) {
          outp[i].r = xjp[i & mask];
          outp[i].i = xjp[i & mask] == NA_INTEGER ? NA_REAL : 0;
        }
      } break;
      case REALSXP: {
        double *xjp = REAL(xj);
        for (int i=0; i<n; i++) {
          outp[i].r = xjp[i & mask];
          outp[i].i = ISNAN(xjp[i & mask]) ? NA_REAL : 0;
        }
      } break;
      case CPLXSXP: {
        xjp = COMPLEX(xj);
        for (int i=0; i<n; i++) {
          outp[i].r = xjp[i & mask].r;
          outp[i].i = xjp[i & mask].i;
        }
      } break;
      default:
        error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
      }
      for (int j=1; j<J; j++) {
        xj = VECTOR_ELT(x, j);
        nj = LENGTH(xj);
        const int mask = nj == 1 ? 0 : INT_MAX;
        switch (TYPEOF(xj)) {
        case LGLSXP: case INTSXP: {
          int *xjp = INTEGER(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              if (xjp[i & mask] == NA_INTEGER) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else if (outp[i].r == 0 && outp[i].i == 0) {
                continue;
              } else {
                outp[i].r *= xjp[i & mask];
                outp[i].i *= xjp[i & mask];
              }
            }
          }
        } break;
        case REALSXP: {
          double *xjp = REAL(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              if (ISNAN(xjp[i & mask])) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else if (outp[i].r == 0 && outp[i].i == 0) {
                continue;
              } else {
                outp[i].r *= xjp[i & mask];
                outp[i].i *= xjp[i & mask];
              }
            }
          }
        } break;
        case CPLXSXP: {
          xjp = COMPLEX(xj);
          for (int i=0; i<n; i++) {
            if (!ISNAN_COMPLEX(outp[i])) {
              if (ISNAN_COMPLEX(xjp[i & mask])) {
                outp[i].r = NA_REAL;
                outp[i].i = NA_REAL;
              } else if (outp[i].r == 0 && outp[i].i == 0) {
                continue;
              } else {
                double tmp=outp[i].r; // see na.rm=TRUE branch
                outp[i].r = outp[i].r * xjp[i & mask].r - outp[i].i * xjp[i & mask].i;
                outp[i].i = tmp * xjp[i & mask].i + outp[i].i * xjp[i & mask].r;
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
  if (!isNewList(x))
    error(_("Internal error: x must be a list")); // # nocov

  SEXP xj, out;
  int J=LENGTH(x);
  if (J == 0) {
    error(_("Empty input"));
  } else if (J == 1) {
    xj = VECTOR_ELT(x, 0);
    if (TYPEOF(xj) == VECSXP) {
      return pany(xj, narmArg);
    } else {
      // maybe need to do coercion to logical
      if (TYPEOF(xj) == LGLSXP) {
        return xj;
      } else {
        out = PROTECT(coerceVector(xj, LGLSXP));
        UNPROTECT(1);
        return out;
      }
    }
  }

  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL)
    error(_("na.rm must be TRUE or FALSE"));

  int n = -1, nj;
  for (int j=0; j<J; j++) {
    xj = VECTOR_ELT(x, j);
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
      nj = LENGTH(xj);
      if (n == 1 && nj > 1) {
        n = nj; // singleton comes first, vector comes later [psum(1, 1:4)]
      } else if (nj != 1 && nj != n) {
        error(_("Inconsistent input lengths -- first found %d, but %d element has length %d. Only singletons will be recycled."), n, j+1, nj);
      }
    } else { // initialize
      n = LENGTH(xj);
    }
  }

  out = PROTECT(allocVector(LGLSXP, n));
  int *outp = LOGICAL(out);
  if (n == 0) {
    UNPROTECT(1);
    return(out);
  }
  if (LOGICAL(narmArg)[0]) {
    // initialize to NA to facilitate all-NA rows --> NA output
    writeNA(out, 0, n);
    /* Logic table for any, na.rm=TRUE. > -_> do nothing; @b --> update to b
     *    | x: ||  0 |  1 | NA |
     *    +----||----|----|----|
     *  o |  0 ||  > | @1 |  > |
     *  u |  1 ||  > |  > |  > |
     *  t | NA || @0 | @1 |  > |
     */
    for (int j=0; j<J; j++) {
      xj = VECTOR_ELT(x, j);
      nj = LENGTH(xj);
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        // for any, if there's a scalar 1, we're done, otherwise, skip
        if (nj == 1) {
          if (xjp[0] == NA_INTEGER)
            continue;
          if (xjp[0] != 0) {
            for (int i=0; i<n; i++) outp[i] = 1;
            break;
          }
          for (int i=0; i<n; i++) {
            if (outp[i] == NA_INTEGER) {
              outp[i] = 0;
            }
          }
          continue;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 1 || xjp[i] == NA_INTEGER)
            continue;
          if (xjp[i] != 0) {
            outp[i] = 1;
            continue;
          }
          // besides maybe in edge cases, it's faster to just
          //   write 0 than branch on outp[i]==NA
          outp[i] = 0;
        }
      } break;
      case REALSXP: {
        double *xjp = REAL(xj);
        if (nj == 1) {
          if (ISNAN(xjp[0]))
            continue;
          if (xjp[0] != 0) {
            for (int i=0; i<n; i++) outp[i] = 1;
            break;
          }
          for (int i=0; i<n; i++) {
            if (outp[i] == NA_INTEGER) {
              outp[i] = 0;
            }
          }
          continue;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 1 || ISNAN(xjp[i]))
            continue;
          if (xjp[i] != 0) {
            outp[i] = 1;
            continue;
          }
          outp[i] = 0;
        }
      } break;
      case CPLXSXP: {
        Rcomplex *xjp = COMPLEX(xj);
        if (nj == 1) {
          if (ISNAN_COMPLEX(xjp[0]))
            continue;
          if (xjp[0].r != 0 || xjp[0].i != 0) {
            for (int i=0; i<n; i++) outp[i] = 1;
            break;
          }
          for (int i=0; i<n; i++) {
            if (outp[i] == NA_INTEGER) {
              outp[i] = 0;
            }
          }
          continue;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 1 || ISNAN_COMPLEX(xjp[i]))
            continue;
          if (xjp[i].r != 0 || xjp[i].i != 0) {
            outp[i] = 1;
            continue;
          }
          outp[i] = 0;
        }
      } break;
      default:
        error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
      }
    }
  } else { // na.rm=FALSE
    /* Logic table for any, na.rm=FALSE. > -_> do nothing; @b --> update to b
     *    | x: || 0 |  1 |  NA |
     *    +----||---|----|-----|
     *  o |  0 || > | @1 | @NA |
     *  u |  1 || > |  > |   > |
     *  t | NA || > | @1 |   > |
     */
    xj = VECTOR_ELT(x, 0);
    nj = LENGTH(xj);
    const int mask = nj == 1 ? 0 : INT_MAX;
    switch (TYPEOF(xj)) {
    case LGLSXP: case INTSXP: {
      int *xjp = INTEGER(xj);
      for (int i=0; i<n; i++) {
        outp[i] = xjp[i & mask] == 0 ? 0 : (xjp[i & mask] == NA_INTEGER ? NA_LOGICAL : 1);
      }
    } break;
    case REALSXP: {
      double *xjp = REAL(xj);
      for (int i=0; i<n; i++) {
        outp[i] = xjp[i & mask] == 0 ? 0 : (ISNAN(xjp[i & mask]) ? NA_LOGICAL : 1);
      }
    } break;
    case CPLXSXP: {
      Rcomplex *xjp = COMPLEX(xj);
      for (int i=0; i<n; i++) {
        outp[i] = xjp[i & mask].r == 0 && xjp[i & mask].i == 0 ? 0 :
          (ISNAN(xjp[i & mask].r) || ISNAN(xjp[i & mask].i) ? NA_LOGICAL : 1);
      }
    } break;
    default:
      error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
    }
    for (int j=1; j<J; j++) {
      xj = VECTOR_ELT(x, j);
      nj = LENGTH(xj);
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        if (nj == 1) {
          if (xjp[0] == 0)
            continue;
          if (xjp[0] == NA_INTEGER) {
            for (int i=0; i<n; i++) {
              if (outp[i] == 0) {
                outp[i] = NA_INTEGER;
              }
            }
            continue;
          }
          // xjp[0] is 1
          for (int i=0; i<n; i++) outp[i] = 1;
          break;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 1 || xjp[i] == 0)
            continue;
          if (xjp[i] == NA_INTEGER) {
            // write even if it's already NA_INTEGER
            outp[i] = NA_INTEGER;
            continue;
          }
          outp[i] = 1;
        }
      } break;
      case REALSXP: {
        double *xjp = REAL(xj);
        if (nj == 1) {
          if (xjp[0] == 0)
            continue;
          if (ISNAN(xjp[0])) {
            for (int i=0; i<n; i++) {
              if (outp[i] == 0) {
                outp[i] = NA_INTEGER;
              }
            }
            continue;
          }
          // xjp[0] is non-zero, non-NA
          for (int i=0; i<n; i++) outp[i] = 1;
          break;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 1 || xjp[i] == 0)
            continue;
          if (ISNAN(xjp[i])) {
            // write even if it's already NA_INTEGER
            outp[i] = NA_INTEGER;
            continue;
          }
          outp[i] = 1;
        }
      } break;
      case CPLXSXP: {
        Rcomplex *xjp = COMPLEX(xj);
        if (nj == 1) {
          if (xjp[0].r == 0 && xjp[0].i == 0)
            continue;
          if (ISNAN_COMPLEX(xjp[0])) {
            for (int i=0; i<n; i++) {
              if (outp[i] == 0) {
                outp[i] = NA_INTEGER;
              }
            }
            continue;
          }
          // xjp[0] is non-zero, non-NA
          for (int i=0; i<n; i++) outp[i] = 1;
          break;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 1 || (xjp[i].r == 0 && xjp[i].i == 0))
            continue;
          if (ISNAN_COMPLEX(xjp[i])) {
            // write even if it's already NA_INTEGER
            outp[i] = NA_INTEGER;
            continue;
          }
          outp[i] = 1;
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
  if (!isNewList(x))
    error(_("Internal error: x must be a list")); // # nocov

  SEXP xj, out;
  int J=LENGTH(x);
  if (J == 0) {
    error(_("Empty input"));
  } else if (J == 1) {
    xj = VECTOR_ELT(x, 0);
    if (TYPEOF(xj) == VECSXP) {
      return pall(xj, narmArg);
    } else {
      // maybe need to do coercion to logical
      if (TYPEOF(xj) == LGLSXP) {
        return xj;
      } else {
        out = PROTECT(coerceVector(xj, LGLSXP));
        UNPROTECT(1);
        return out;
      }
    }
  }

  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL)
    error(_("na.rm must be TRUE or FALSE"));

  int n = -1, nj;
  for (int j=0; j<J; j++) {
    xj = VECTOR_ELT(x, j);
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
      nj = LENGTH(xj);
      if (n == 1 && nj > 1) {
        n = nj; // singleton comes first, vector comes later [psum(1, 1:4)]
      } else if (nj != 1 && nj != n) {
        error(_("Inconsistent input lengths -- first found %d, but %d element has length %d. Only singletons will be recycled."), n, j+1, nj);
      }
    } else { // initialize
      n = LENGTH(xj);
    }
  }

  out = PROTECT(allocVector(LGLSXP, n));
  int *outp = LOGICAL(out);
  if (n == 0) {
    UNPROTECT(1);
    return(out);
  }
  if (LOGICAL(narmArg)[0]) {
    // initialize to NA to facilitate all-NA rows --> NA output
    writeNA(out, 0, n);
    /* Logic table for all, na.rm=TRUE. > -_> do nothing; @b --> update to b
     *    | x: ||  0 |  1 | NA |
     *    +----||----|----|----|
     *  o |  0 ||  > |  > |  > |
     *  u |  1 || @0 |  > |  > |
     *  t | NA || @0 | @1 |  > |
     */
    for (int j=0; j<J; j++) {
      xj = VECTOR_ELT(x, j);
      nj = LENGTH(xj);
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        // for all, if there's a scalar 0, we're done, otherwise, skip
        if (nj == 1) {
          if (xjp[0] == NA_INTEGER)
            continue;
          if (xjp[0] == 0) {
            for (int i=0; i<n; i++) outp[i] = 0;
            break;
          }
          for (int i=0; i<n; i++) {
            if (outp[i] == NA_INTEGER) {
              outp[i] = 1;
            }
          }
          continue;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 0 || xjp[i] == NA_INTEGER)
            continue;
          if (xjp[i] == 0) {
            outp[i] = 0;
            continue;
          }
          // besides maybe in edge cases, it's faster to just
          //   write 1 than branch on outp[i]==NA
          outp[i] = 1;
        }
      } break;
      case REALSXP: {
        double *xjp = REAL(xj);
        if (nj == 1) {
          if (ISNAN(xjp[0]))
            continue;
          if (xjp[0] == 0) {
            for (int i=0; i<n; i++) outp[i] = 0;
            break;
          }
          for (int i=0; i<n; i++) {
            if (outp[i] == NA_INTEGER) {
              outp[i] = 1;
            }
            continue;
          }
          continue;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 0 || ISNAN(xjp[i]))
            continue;
          if (xjp[i] == 0) {
            outp[i] = 0;
            continue;
          }
          outp[i] = 1;
        }
      } break;
      case CPLXSXP: {
        Rcomplex *xjp = COMPLEX(xj);
        if (nj == 1) {
          if (ISNAN_COMPLEX(xjp[0]))
            continue;
          if (xjp[0].r == 0 && xjp[0].i == 0) {
            for (int i=0; i<n; i++) outp[i] = 0;
            break;
          }
          for (int i=0; i<n; i++) {
            if (outp[i] == NA_INTEGER) {
              outp[i] = 1;
            }
            continue;
          }
          continue;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 0 || ISNAN_COMPLEX(xjp[i]))
            continue;
          if (xjp[i].r == 0 && xjp[i].i == 0) {
            outp[i] = 0;
            continue;
          }
          outp[i] = 1;
        }
      } break;
      default:
        error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
      }
    }
  } else { // na.rm=FALSE
    /* Logic table for all, na.rm=FALSE. > -_> do nothing; @b --> update to b
     *    | x: ||  0 | 1 |  NA |
     *    +----||----|---|-----|
     *  o |  0 ||  > | > |   > |
     *  u |  1 || @0 | > | @NA |
     *  t | NA || @0 | > |   > |
     */
    xj = VECTOR_ELT(x, 0);
    nj = LENGTH(xj);
    const int mask = nj == 1 ? 0 : INT_MAX;
    switch (TYPEOF(xj)) {
    case LGLSXP: case INTSXP: {
      int *xjp = INTEGER(xj);
      for (int i=0; i<n; i++) {
        outp[i] = xjp[i & mask] == 0 ? 0 : (xjp[i & mask] == NA_INTEGER ? NA_LOGICAL : 1);
      }
    } break;
    case REALSXP: {
      double *xjp = REAL(xj);
      for (int i=0; i<n; i++) {
        outp[i] = xjp[i & mask] == 0 ? 0 : (ISNAN(xjp[i & mask]) ? NA_LOGICAL : 1);
      }
    } break;
    case CPLXSXP: {
      Rcomplex *xjp = COMPLEX(xj);
      for (int i=0; i<n; i++) {
        outp[i] = xjp[i & mask].r == 0 && xjp[i & mask].i == 0 ? 0 :
          (ISNAN(xjp[i & mask].r) || ISNAN(xjp[i & mask].i) ? NA_LOGICAL : 1);
      }
    } break;
    default:
      error(_("Internal error: should have caught non-INTSXP/REALSXP input by now")); // # nocov
    }
    for (int j=1; j<J; j++) {
      xj = VECTOR_ELT(x, j);
      nj = LENGTH(xj);
      switch (TYPEOF(xj)) {
      case LGLSXP: case INTSXP: {
        int *xjp = INTEGER(xj);
        if (nj == 1) {
          if (xjp[0] == NA_INTEGER) {
            for (int i=0; i<n; i++) {
              if (outp[i] != 0) {
                outp[i] = NA_LOGICAL;
              }
            }
            continue;
          }
          if (xjp[0] != 0)
            continue;
          // xjp[0] is 0
          for (int i=0; i<n; i++) outp[i] = 0;
          break;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 0)
            continue;
          if (xjp[i] == 0) {
            outp[i] = 0;
            continue;
          }
          if (xjp[i] == NA_INTEGER) {
            // write even if it's already NA_INTEGER
            outp[i] = NA_INTEGER;
            continue;
          }
        }
      } break;
      case REALSXP: {
        double *xjp = REAL(xj);
        if (nj == 1) {
          if (ISNAN(xjp[0])) {
            for (int i=0; i<n; i++) {
              if (outp[i] != 0) {
                outp[i] = NA_LOGICAL;
              }
            }
            continue;
          }
          if (xjp[0] != 0)
            continue;
          // xjp[0] is 0
          for (int i=0; i<n; i++) outp[i] = 0;
          break;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 0)
            continue;
          if (xjp[i] == 0) {
            outp[i] = 0;
            continue;
          }
          if (ISNAN(xjp[i])) {
            // write even if it's already NA_INTEGER
            outp[i] = NA_INTEGER;
            continue;
          }
        }
      } break;
      case CPLXSXP: {
        Rcomplex *xjp = COMPLEX(xj);
        if (nj == 1) {
          if (ISNAN_COMPLEX(xjp[0])) {
            for (int i=0; i<n; i++) {
              if (outp[i] != 0) {
                outp[i] = NA_LOGICAL;
              }
            }
            continue;
          }
          if (xjp[0].r != 0 || xjp[0].i != 0)
            continue;
          // xjp[0] is 0
          for (int i=0; i<n; i++) outp[i] = 0;
          break;
        }
        for (int i=0; i<n; i++) {
          if (outp[i] == 0)
            continue;
          if (xjp[i].r == 0 && xjp[i].i == 0) {
            outp[i] = 0;
            continue;
          }
          if (ISNAN_COMPLEX(xjp[i])) {
            // write even if it's already NA_INTEGER
            outp[i] = NA_INTEGER;
            continue;
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
