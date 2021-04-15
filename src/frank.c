#include "data.table.h"
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

SEXP dt_na(SEXP x, SEXP cols) {
  int n=0, elem;

  if (!isNewList(x)) error(_("Internal error. Argument 'x' to Cdt_na is type '%s' not 'list'"), type2char(TYPEOF(x))); // # nocov
  if (!isInteger(cols)) error(_("Internal error. Argument 'cols' to Cdt_na is type '%s' not 'integer'"), type2char(TYPEOF(cols))); // # nocov
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
    if (!length(v) || isNewList(v) || isList(v)) continue; // like stats:::na.omit.data.frame, skip list/pairlist columns
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
      const double *dv = REAL(v);
      if (INHERITS(v, char_integer64)) {
        for (int j=0; j<n; ++j) {
          ians[j] |= (DtoLL(dv[j]) == NA_INT64_LL);   // TODO: can be == NA_INT64_D directly
        }
      } else {
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
    //   for (i = 0; i < length(xstartArg); i++) {
    //     k=1;
    //     for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
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
  int i, j, n=0, elem;

  if (!isNewList(x)) error(_("Internal error. Argument 'x' to CanyNA is type '%s' not 'list'"), type2char(TYPEOF(x))); // #nocov
  if (!isInteger(cols)) error(_("Internal error. Argument 'cols' to CanyNA is type '%s' not 'integer'"), type2char(TYPEOF(cols))); // # nocov
  for (i=0; i<LENGTH(cols); i++) {
    elem = INTEGER(cols)[i];
    if (elem<1 || elem>LENGTH(x))
      error(_("Item %d of 'cols' is %d which is outside 1-based range [1,ncol(x)=%d]"), i+1, elem, LENGTH(x));
    if (!n) n = length(VECTOR_ELT(x, elem-1));
  }
  SEXP ans = PROTECT(allocVector(LGLSXP, 1));
  LOGICAL(ans)[0]=0;
  for (i=0; i<LENGTH(cols); i++) {
    SEXP v = VECTOR_ELT(x, INTEGER(cols)[i]-1);
    if (!length(v) || isNewList(v) || isList(v)) continue; // like stats:::na.omit.data.frame, skip list/pairlist columns
    if (n != length(v))
      error(_("Column %d of input list x is length %d, inconsistent with first column of that item which is length %d."), i+1,length(v),n);
    j=0;
    switch (TYPEOF(v)) {
    case LGLSXP: {
      const int *iv = LOGICAL(v);
      while(j < n && iv[j] != NA_LOGICAL) j++;
      if (j < n) LOGICAL(ans)[0] = 1;
    }
      break;
    case INTSXP: {
      const int *iv = INTEGER(v);
      while(j < n && iv[j] != NA_INTEGER) j++;
      if (j < n) LOGICAL(ans)[0] = 1;
    }
      break;
    case STRSXP: {
      while (j < n && STRING_ELT(v, j) != NA_STRING) j++;
      if (j < n) LOGICAL(ans)[0] = 1;
    }
      break;
    case REALSXP: {
      const double *dv = REAL(v);
      if (INHERITS(v, char_integer64)) {
        for (j=0; j<n; j++) {
          if (DtoLL(dv[j]) == NA_INT64_LL) {
            LOGICAL(ans)[0] = 1;
            break;
          }
        }
      } else {
        while(j < n && !ISNAN(dv[j])) j++;
        if (j < n) LOGICAL(ans)[0] = 1;
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
      while (j < n && !ISNAN(COMPLEX(v)[j].r) && !ISNAN(COMPLEX(v)[j].i)) j++;
      if (j < n) LOGICAL(ans)[0] = 1;
    }
      break;
    default:
      error(_("Unsupported column type '%s'"), type2char(TYPEOF(v)));
    }
    if (LOGICAL(ans)[0]) break;
  }
  UNPROTECT(1);
  return(ans);
}
