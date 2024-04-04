#include "data.table.h"
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

SEXP dt_na(SEXP x, SEXP cols) {
  int n=0, elem;

  if (!isNewList(x)) error(_("Internal error. Argument '%s' to %s is type '%s' not '%s'"), "x", "Cdt_na", type2char(TYPEOF(x)), "list"); // # nocov
  if (!isInteger(cols)) error(_("Internal error. Argument '%s' to %s is type '%s' not '%s'"), "cols", "Cdt_na", type2char(TYPEOF(cols)), "integer"); // # nocov
  for (int i=0; i<LENGTH(cols); ++i) {
    elem = INTEGER(cols)[i];
    if (elem<1 || elem>LENGTH(x))
      error(_("Item %d of 'cols' is %d which is outside 1-based range [1,ncol(x)=%d]"), i+1, elem, LENGTH(x));
    if (!n) n = length(VECTOR_ELT(x, elem-1));
  }
  SEXP ans = PROTECT(allocVector(LGLSXP, n));
  int *ians = LOGICAL(ans);
  for (int i=0; i<n; ++i) ians[i]=0;
  for (int i=0; i<LENGTH(cols); ++i) {
    SEXP v = VECTOR_ELT(x, INTEGER(cols)[i]-1);
    if (!length(v) || isList(v)) continue; // like stats:::na.omit.data.frame, skip pairlist columns
    if (n != length(v))
      error(_("Column %d of input list x is length %d, inconsistent with first column of that item which is length %d."), i+1,length(v),n);
    switch (TYPEOF(v)) {
    case LGLSXP: {
      const int *iv = LOGICAL(v);
      for (int j=0; j<n; ++j) ians[j] |= (iv[j] == NA_LOGICAL);
    }
      break;
    case INTSXP: {
      const int *iv = INTEGER(v);
      for (int j=0; j<n; ++j) ians[j] |= (iv[j] == NA_INTEGER);
    }
      break;
    case STRSXP: {
      const SEXP *sv = STRING_PTR(v);
      for (int j=0; j<n; ++j) ians[j] |= (sv[j] == NA_STRING);
    }
      break;
    case REALSXP: {
      if (INHERITS(v, char_integer64)) {
        const int64_t *dv = (int64_t *)REAL(v);
        for (int j=0; j<n; ++j) ians[j] |= (dv[j] == NA_INTEGER64);
      } else {
        const double *dv = REAL(v);
        for (int j=0; j<n; ++j) ians[j] |= ISNAN(dv[j]);
      }
    }
      break;
    case RAWSXP: {
      // no such thing as a raw NA
      // vector already initialised to all 0's
    }
      break;
    case CPLXSXP: {
      // taken from https://github.com/wch/r-source/blob/d75f39d532819ccc8251f93b8ab10d5b83aac89a/src/main/coerce.c
      for (int j=0; j<n; ++j) ians[j] |= (ISNAN(COMPLEX(v)[j].r) || ISNAN(COMPLEX(v)[j].i));
    }
      break;
    case VECSXP: {
      // is.na(some_list) returns TRUE only for elements which are
      // scalar NA.
      for (int j=0; j<n; ++j) {
        SEXP list_element = VECTOR_ELT(v, j);
        switch (TYPEOF(list_element)) {
        case LGLSXP: {
          ians[j] |= (length(list_element)==1 && LOGICAL(list_element)[0] == NA_LOGICAL);
        }
          break;
        case INTSXP: {
          ians[j] |= (length(list_element)==1 && INTEGER(list_element)[0] == NA_INTEGER);
        }
          break;
        case STRSXP: {
          ians[j] |= (length(list_element)==1 && STRING_ELT(list_element,0) == NA_STRING);
        }
          break;
        case CPLXSXP: {
          if (length(list_element)==1) {
            Rcomplex first_complex = COMPLEX(list_element)[0];
            ians[j] |= (ISNAN(first_complex.r) || ISNAN(first_complex.i));
          }
        }
          break;
        case REALSXP: {
          if (length(list_element)==1) {
            if (INHERITS(list_element, char_integer64)) {
              ians[j] |= ((const int64_t *)REAL(list_element))[0] == NA_INTEGER64;
            } else {
              ians[j] |= ISNAN(REAL(list_element)[0]);
            }
          }
        }
          break;
        }
      }
    }
      break;
    default:
      error(_("Unsupported column type '%s'"), type2char(TYPEOF(v)));
    }
  }
  UNPROTECT(1);
  return(ans);
}

SEXP frank(SEXP xorderArg, SEXP xstartArg, SEXP xlenArg, SEXP ties_method) {
  const int *xstart = INTEGER(xstartArg), *xlen = INTEGER(xlenArg), *xorder = INTEGER(xorderArg);
  enum {MEAN, MAX, MIN, DENSE, SEQUENCE, LAST} ties; // RUNLENGTH

  const char *pties = CHAR(STRING_ELT(ties_method, 0));
  if (!strcmp(pties, "average"))  ties = MEAN;
  else if (!strcmp(pties, "max")) ties = MAX;
  else if (!strcmp(pties, "min")) ties = MIN;
  else if (!strcmp(pties, "dense")) ties = DENSE;
  else if (!strcmp(pties, "sequence")) ties = SEQUENCE;
  else if (!strcmp(pties, "last")) ties = LAST;
  // else if (!strcmp(pties, "runlength")) ties = RUNLENGTH;
  else error(_("Internal error: invalid ties.method for frankv(), should have been caught before. please report to data.table issue tracker")); // # nocov
  const int n = length(xorderArg);
  SEXP ans = PROTECT(allocVector(ties==MEAN ? REALSXP : INTSXP, n));
  int *ians=NULL;
  double *dans=NULL;
  if (ties==MEAN) dans=REAL(ans); else ians=INTEGER(ans);
  if (n>0) {
    switch (ties) {
    case MEAN :
      for (int i=0; i<length(xstartArg); ++i) {
        for (int j=xstart[i]-1; j<xstart[i]+xlen[i]-1; ++j)
          dans[xorder[j]-1] = (2*xstart[i]+xlen[i]-1)/2.0;
      }
      break;
    case MAX :
      for (int i=0; i<length(xstartArg); ++i) {
        for (int j=xstart[i]-1; j<xstart[i]+xlen[i]-1; ++j)
          ians[xorder[j]-1] = xstart[i]+xlen[i]-1;
      }
      break;
    case MIN :
      for (int i=0; i<length(xstartArg); ++i) {
        for (int j=xstart[i]-1; j<xstart[i]+xlen[i]-1; ++j)
          ians[xorder[j]-1] = xstart[i];
      }
      break;
    case DENSE : {
      int k=1;
      for (int i=0; i<length(xstartArg); ++i) {
        for (int j=xstart[i]-1; j<xstart[i]+xlen[i]-1; ++j)
          ians[xorder[j]-1] = k;
        ++k;
      }
    } break;
    case SEQUENCE :
      for (int i=0; i<length(xstartArg); ++i) {
        int k=1;
        for (int j=xstart[i]-1; j<xstart[i]+xlen[i]-1; ++j)
          ians[xorder[j]-1] = k++;
      }
      break;
    case LAST :
      for (int i=0; i<length(xstartArg); ++i) {
        // alternatively, also loop k from xstart[i]+xlen[i]-1 and decrement
        int offset = 2*xstart[i]+xlen[i]-2;
        for (int j=xstart[i]-1; j<xstart[i]+xlen[i]-1; ++j) {
          ians[xorder[j]-1] = offset-j;
        }
      }
      break;
    // case RUNLENGTH :
    //   for (int i=0; i<length(xstartArg); ++i) {
    //     k=1;
    //     for (int j=xstart[i]-1; j<xstart[i]+xlen[i]-1; ++j)
    //       INTEGER(ans)[xorder[j]-1] = k++;
    //   }
    //   break;
    default: error(_("Internal error: unknown ties value in frank: %d"), ties); // #nocov
    }
  }
  UNPROTECT(1);
  return(ans);
}

// internal version of anyNA for data.tables
SEXP anyNA(SEXP x, SEXP cols) {
  int n=0;
  if (!isNewList(x)) error(_("Internal error. Argument '%s' to %s is type '%s' not '%s'"), "x", "CanyNA", type2char(TYPEOF(x)), "list"); // #nocov
  if (!isInteger(cols)) error(_("Internal error. Argument '%s' to %s is type '%s' not '%s'"), "cols", "CanyNA", type2char(TYPEOF(cols)), "integer"); // # nocov
  for (int i=0; i<LENGTH(cols); ++i) {
    const int elem = INTEGER(cols)[i];
    if (elem<1 || elem>LENGTH(x))
      error(_("Item %d of 'cols' is %d which is outside 1-based range [1,ncol(x)=%d]"), i+1, elem, LENGTH(x));
    if (!n) n = length(VECTOR_ELT(x, elem-1));
  }
  int j=0; // did we get to the end of the column without finding any NA in it?
  for (int i=0; i<LENGTH(cols); ++i) {
    SEXP v = VECTOR_ELT(x, INTEGER(cols)[i]-1);
    if (!length(v) || isNewList(v) || isList(v)) continue; // like stats:::na.omit.data.frame, skip list/pairlist columns
    if (n != length(v))
      error(_("Column %d of input list x is length %d, inconsistent with first column of that item which is length %d."), i+1,length(v),n);
    j=0;
    switch (TYPEOF(v)) {
    case LGLSXP: {
      const int *iv = LOGICAL(v);
      while(j<n && iv[j]!=NA_LOGICAL) j++;
    } break;
    case INTSXP: {
      const int *iv = INTEGER(v);
      while(j<n && iv[j]!=NA_INTEGER) j++;
    } break;
    case STRSXP: {
      const SEXP *sv = STRING_PTR(v);
      while (j<n && sv[j]!=NA_STRING) j++;
    } break;
    case REALSXP:
      if (INHERITS(v, char_integer64)) {
        const int64_t *dv = (int64_t *)REAL(v);
        while (j<n && dv[j]!=NA_INTEGER64) j++;
      } else {
        const double *dv = REAL(v);
        while (j<n && !ISNAN(dv[j])) j++;
      }
      break;
    case RAWSXP:
      // no such thing as a raw NA; vector already initialised to all 0's
      j = n;
      break;
    case CPLXSXP: {
      const Rcomplex *cv = COMPLEX(v);
      // taken from https://github.com/wch/r-source/blob/d75f39d532819ccc8251f93b8ab10d5b83aac89a/src/main/coerce.c
      while (j<n && !ISNAN(cv[j].r) && !ISNAN(cv[j].i)) j++;
    } break;
    default:
      error(_("Unsupported column type '%s'"), type2char(TYPEOF(v)));
    }
    if (j<n) break; // don't look at any more columns; return true early
  }
  return ScalarLogical(j<n);
}
