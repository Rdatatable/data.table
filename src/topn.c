#include "data.table.h"

static inline void swap(int *a, int *b) { int tmp=*a; *a=*b; *b=tmp; }

static inline bool icmp(const int a, const int b, const bool min, const bool nalast) {
  if (a==NA_INTEGER) return(nalast);
  if (b==NA_INTEGER) return(!nalast);
  return(min ? a < b : a > b);
}
static inline bool dcmp(const double a, const double b, const bool min, const bool nalast) {
  if (isnan(a)) return(nalast);
  if (isnan(b)) return(!nalast);
  return(min ? a < b : a > b);
}

static inline bool i64cmp(const int64_t a, const int64_t b, const bool min, const bool nalast) {
  if (a==NA_INTEGER64) return(nalast);
  if (b==NA_INTEGER64) return(!nalast);
  return(min ? a < b : a > b);
}

static inline bool scmp(SEXP a, SEXP b, const bool min, bool nalast) {
  if (a==NA_STRING) return(nalast);
  if (b==NA_STRING) return(!nalast);
  return(min ? strcmp(CHAR(a),CHAR(b))<0 : strcmp(CHAR(a),CHAR(b)))>0;
}

// compare value with both childs and sift value down if child value smaller
// than parent (for minheap)
#undef SIFT
#define SIFT(CMP) {                                                   \
  int smallest, l, r;                                                 \
  while(true) {                                                       \
    smallest = k;                                                     \
    l = (k << 1) + 1;                                                 \
    r = (k << 1) + 2;                                                 \
    if (l < len && CMP(VAL[IND[l]],VAL[IND[smallest]],min,nalast))    \
      smallest = l;                                                   \
    if (r < len && CMP(VAL[IND[r]],VAL[IND[smallest]],min,nalast))    \
      smallest = r;                                                   \
    if (smallest != k) {                                              \
      swap(&IND[k], &IND[smallest]);                                  \
      k = smallest;                                                   \
    } else {                                                          \
      break;                                                          \
    }                                                                 \
  }                                                                   \
}

// for finding decreasing topn build minheap and add values if they exceed
// minimum by overwriting minimum and following down sifting
#undef TOPN
#define TOPN(CTYPE, RTYPE, CMP) {                         \
  const CTYPE *restrict VAL = (const CTYPE *)RTYPE(x);    \
  for (int i=n/2; i>=0; --i) { k=i; len=n; SIFT(CMP); }   \
  for (int i=n; i<xlen; ++i) {                            \
    if (CMP(VAL[IND[0]],VAL[i],min,nalast)) {             \
      IND[0] = i;                                         \
      k=0; len=n; SIFT(CMP);                              \
    }                                                     \
  }                                                       \
  for (int i=0; i<n; ++i) {                               \
    swap(&IND[0], &IND[n-1-i]);                           \
    k=0; len=n-1-i; SIFT(CMP);                            \
    ians[n-1-i] = IND[n-1-i]+1;                           \
  }                                                       \
  free(IND);                                              \
}

SEXP topn(SEXP x, SEXP nArg, SEXP naArg, SEXP ascArg) {
  if (!isInteger(nArg) || LENGTH(nArg)!=1 || INTEGER(nArg)[0]<=0 || INTEGER(nArg)[0]==NA_INTEGER) error(_("topn(x,n) only implemented for n > 0."));
  if (!IS_TRUE_OR_FALSE(ascArg)) error(_("%s must be TRUE or FALSE"), "decreasing");
  if (!IS_TRUE_OR_FALSE(naArg)) error(_("%s must be TRUE or FALSE"), "na.last");

  const int xlen = LENGTH(x);
  const int n = INTEGER(nArg)[0];
  if (n > xlen) error(_("TODO"));

  const bool min = LOGICAL(ascArg)[0];
  const bool nalast = LOGICAL(naArg)[0];

  SEXP ans;
  int k, len;
  ans = PROTECT(allocVector(INTSXP, n));
  int *restrict ians = INTEGER(ans);
  int *restrict IND = malloc(n*sizeof(int));
  for (int i=0; i<n; ++i) IND[i] = i;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {          TOPN(int,     INTEGER,    icmp); } break;
  case REALSXP: {
    if (INHERITS(x, char_integer64)) { TOPN(int64_t, REAL,       i64cmp); }
    else {                             TOPN(double,  REAL,       dcmp); } break; }
  case STRSXP: {                       TOPN(SEXP,    STRING_PTR, scmp); } break;
  default:
    free(IND); error(_("Type '%s' not supported by topn"), type2char(TYPEOF(x)));
  }
  UNPROTECT(1);
  return(ans);
}
