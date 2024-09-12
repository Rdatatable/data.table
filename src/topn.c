#include "data.table.h"

static inline void swap(int *a, int *b) { int tmp=*a; *a=*b; *b=tmp; }

static inline bool icmp(const int *x, int i, int j, bool min, bool nalast) {
  if (x[i]==x[j]) return i > j;
  if (x[i]==NA_INTEGER) return nalast;
  if (x[j]==NA_INTEGER) return !nalast;
  return min ? x[i] < x[j] : x[i] > x[j];
}

static inline bool dcmp(const double *x, int i, int j, bool min, bool nalast) {
  if (x[i]==x[j] || (isnan(x[i]) && isnan(x[j]))) return i > j;
  if (isnan(x[i])) return nalast;
  if (isnan(x[j])) return !nalast;
  return min ? x[i] < x[j] : x[i] > x[j];
}

static inline bool i64cmp(const int64_t *x, int i, int j, bool min, bool nalast) {
  if (x[i]==x[j]) return i > j;
  if (x[i]==NA_INTEGER64) return nalast;
  if (x[j]==NA_INTEGER64) return !nalast;
  return min ? x[i] < x[j] : x[i] > x[j];
}

static inline bool scmp(const SEXP *restrict x, int i, int j, bool min, bool nalast) {
  if (strcmp(CHAR(x[i]), CHAR(x[j])) == 0) return i > j;
  if (x[i]==NA_STRING) return nalast;
  if (x[j]==NA_STRING) return !nalast;
  return min ? strcmp(CHAR(x[i]),CHAR(x[j]))<0 : strcmp(CHAR(x[i]),CHAR(x[j]))>0;
}

static inline bool ccmp(const Rcomplex *x, int i, int j, bool min, bool nalast) {
  if (ISNAN_COMPLEX(x[i]) && ISNAN_COMPLEX(x[j])) return i > j;
  if (x[i].r==x[j].r) {
    if (x[i].i==x[j].i) return i > j;
    return min ? x[i].i < x[j].i : x[i].i > x[j].i;
  }
  if (ISNAN_COMPLEX(x[i])) return nalast;
  if (ISNAN_COMPLEX(x[j])) return !nalast;
  return min ? x[i].r < x[j].r : x[i].r > x[j].r;
}

// compare value with both childs and sift value down if child value smaller
// than parent (for minheap)
#undef SIFT
#define SIFT(CMP) {                                                   \
  int smallest, l, r;                                                 \
  while(true) {                                                       \
    smallest = k;                                                     \
    l = (k << 1) + 1;                                                 \
    r = l+1;                                                          \
    if (l < len && CMP(VAL,INDEX[l],INDEX[smallest],min,nalast))      \
      smallest = l;                                                   \
    if (r < len && CMP(VAL,INDEX[r],INDEX[smallest],min,nalast))      \
      smallest = r;                                                   \
    if (smallest != k) {                                              \
      swap(&INDEX[k], &INDEX[smallest]);                              \
      k = smallest;                                                   \
    } else {                                                          \
      break;                                                          \
    }                                                                 \
  }                                                                   \
}

// for finding decreasing topn build minheap and add values if they exceed
// minimum by overwriting minimum and following down sifting
#undef TOPN
#define TOPN(CTYPE, RTYPE, CMP, SORTED) {                 \
  const CTYPE *restrict VAL = (const CTYPE *)RTYPE(x);    \
  for (int i=n/2; i>=0; --i) { k=i; len=n; SIFT(CMP); }   \
  for (int i=n; i<xlen; ++i) {                            \
    if (CMP(VAL,INDEX[0],i,min,nalast)) {                 \
      INDEX[0] = i;                                       \
      k=0; len=n; SIFT(CMP);                              \
    }                                                     \
  }                                                       \
  if (SORTED) {                                           \
    for (int i=0; i<n; ++i) {                             \
      swap(&INDEX[0], &INDEX[n-1-i]);                     \
      k=0; len=n-1-i; SIFT(CMP);                          \
      ians[n-1-i] = INDEX[n-1-i]+1;                       \
    }                                                     \
  } else {                                                \
    for (int i=0; i<n; ++i) {                             \
      ians[i] = INDEX[i]+1;                               \
    }                                                     \
  }                                                       \
  free(INDEX);                                            \
}

SEXP topn(SEXP x, SEXP nArg, SEXP naArg, SEXP ascArg, SEXP sortedArg) {
  if (!isInteger(nArg) || LENGTH(nArg)!=1 || INTEGER(nArg)[0]<=0 || INTEGER(nArg)[0]==NA_INTEGER) error(_("topn(x,n) only implemented for n > 0."));
  if (!IS_TRUE_OR_FALSE(ascArg)) error(_("%s must be TRUE or FALSE"), "decreasing");
  if (!IS_TRUE_OR_FALSE(naArg)) error(_("%s must be TRUE or FALSE"), "na.last");
  if (!IS_TRUE_OR_FALSE(sortedArg)) error(_("%s must be TRUE or FALSE"), "sorted");

  const int xlen = LENGTH(x);
  int n = INTEGER(nArg)[0];
  if (n > xlen) {
    warning(_("n should be smaller or equal than length(x) but provided n=%d and length(x)=%d.\n  Coercing n to length(x)."), n, xlen);
    n = xlen;
  }

  const bool min = LOGICAL(ascArg)[0];
  const bool nalast = LOGICAL(naArg)[0];
  const bool sorted = LOGICAL(sortedArg)[0];

  SEXP ans;
  int k, len;
  ans = PROTECT(allocVector(INTSXP, n));
  int *restrict ians = INTEGER(ans);
  int *restrict INDEX = malloc(n*sizeof(int));
  for (int i=0; i<n; ++i) INDEX[i] = i;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {          TOPN(int,      INTEGER,    icmp,   sorted); } break;
  case REALSXP: {
    if (INHERITS(x, char_integer64)) { TOPN(int64_t,  REAL,       i64cmp, sorted); }
    else {                             TOPN(double,   REAL,       dcmp,   sorted); } break; }
  case CPLXSXP: {                      TOPN(Rcomplex, COMPLEX,    ccmp,   sorted); } break;
  case STRSXP: {                       TOPN(SEXP,     STRING_PTR, scmp,   sorted); } break;
  default:
    free(INDEX); error(_("Type '%s' not supported by topn."), type2char(TYPEOF(x)));
  }
  UNPROTECT(1);
  return(ans);
}
