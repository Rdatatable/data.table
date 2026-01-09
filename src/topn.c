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

// compare value of node with values of left/right child nodes and sift value down if value of child node is smaller than parent (for minheap)
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
#undef HEAPN
#define HEAPN(CTYPE, RTYPE, CMP, SORTED) {                \
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
  int *restrict INDEX = (int *)R_alloc(n, sizeof(int));
  for (int i=0; i<n; ++i) INDEX[i] = i;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {          HEAPN(int,      INTEGER_RO,    icmp,   sorted); } break;
  case REALSXP: {
    if (INHERITS(x, char_integer64)) { HEAPN(int64_t,  REAL_RO,       i64cmp, sorted); }
    else {                             HEAPN(double,   REAL_RO,       dcmp,   sorted); } break; }
  case CPLXSXP: {                      HEAPN(Rcomplex, COMPLEX_RO,    ccmp,   sorted); } break;
  case STRSXP: {                       HEAPN(SEXP,     STRING_PTR_RO, scmp,   sorted); } break;
  default:
    error(_("Type '%s' not supported by topn."), type2char(TYPEOF(x)));
  }
  UNPROTECT(1);
  return(ans);
}

#undef QUICKN
#define QUICKN(CTYPE, RTYPE, CMP, SWAP)             \
  CTYPE *ix = (CTYPE *)RTYPE(x);                    \
  CTYPE *ians = (CTYPE *)RTYPE(ans);                \
  unsigned long l = 0, ir = xlen - 1;               \
  for (;;) {                                        \
    if (ir <= l + 1) {                              \
      if (ir == l + 1 && CMP(ix,l,ir,min,nalast)) { \
        SWAP(ix+l, ix+ir);                          \
      }                                             \
      break;                                        \
    } else {                                        \
      unsigned long mid = (l + ir) >> 1;            \
      SWAP(ix+mid, ix+l + 1);                       \
      if (CMP(ix,l,ir,min,nalast)) {                \
        SWAP(ix+l, ix+ir);                          \
      }                                             \
      if (CMP(ix,l+1,ir,min,nalast)) {              \
        SWAP(ix+l+1, ix+ir);                        \
      }                                             \
      if (CMP(ix,l,l+1,min,nalast)) {               \
        SWAP(ix+l, ix+l+1);                         \
      }                                             \
      unsigned long i = l + 1, j = ir;              \
      for (;;) {                                    \
        do i++; while (CMP(ix,l+1,i,min,nalast));   \
        do j--; while (CMP(ix,j,l+1,min,nalast));   \
        if (j < i) break;                           \
        SWAP(ix+i, ix+j);                           \
      }                                             \
      SWAP(ix+l+1, ix+j);                           \
      if (j >= n) ir = j - 1;                       \
      if (j <= n) l = i;                            \
    }                                               \
  }                                                 \
  memcpy(ians, ix, n * sizeof(CTYPE))               

static inline void iswap(int *a, int *b)           {int      tmp=*a; *a=*b; *b=tmp;}
static inline void dswap(double *a, double *b)     {double   tmp=*a; *a=*b; *b=tmp;}
static inline void i64swap(int64_t *a, int64_t *b) {int64_t  tmp=*a; *a=*b; *b=tmp;}
static inline void cswap(Rcomplex *a, Rcomplex *b) {Rcomplex tmp=*a; *a=*b; *b=tmp;}
static inline void sswap(SEXP *a, SEXP *b)         {SEXP     tmp=*a; *a=*b; *b=tmp;}

SEXP quickn(SEXP x, SEXP nArg, SEXP naArg, SEXP ascArg) {
  if (!isInteger(nArg) || LENGTH(nArg)!=1 || INTEGER(nArg)[0]<=0 || INTEGER(nArg)[0]==NA_INTEGER) error(_("topn(x,n) only implemented for n > 0."));
  if (!IS_TRUE_OR_FALSE(ascArg)) error(_("%s must be TRUE or FALSE"), "decreasing");
  if (!IS_TRUE_OR_FALSE(naArg)) error(_("%s must be TRUE or FALSE"), "na.last");

  const int xlen = LENGTH(x);
  int n = INTEGER(nArg)[0];
  x = PROTECT(duplicate(x));

  const bool min = LOGICAL(ascArg)[0];
  const bool nalast = LOGICAL(naArg)[0];

  SEXP ans;
  ans = PROTECT(allocVector(TYPEOF(x), n));
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {          QUICKN(int,      INTEGER,    icmp, iswap); } break;
  case REALSXP: {
    if (INHERITS(x, char_integer64)) { QUICKN(int64_t,  REAL,       i64cmp, i64swap); }
    else {                             QUICKN(double,   REAL,       dcmp,   dswap); } break; }
  case CPLXSXP: {                      QUICKN(Rcomplex, COMPLEX,    ccmp,   cswap); } break;
  case STRSXP: {                       QUICKN(SEXP,     STRING_PTR_RO, scmp,   sswap); } break;
  default:
    error(_("Type '%s' not supported by quickn."), type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  UNPROTECT(2);
  return(ans);
}
