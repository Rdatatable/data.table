#include "data.table.h"

static inline void swap(int *a, int *b) { int tmp=*a; *a=*b; *b=tmp; }

static inline bool icmp(const int *x, const int a, const int b, const bool min, const bool nalast) {
  if (x[a]==x[b]) return(a > b);
  if (x[a]==NA_INTEGER) return(nalast);
  if (x[b]==NA_INTEGER) return(!nalast);
  return(min ? x[a] < x[b] : x[a] > x[b]);
}
static inline bool dcmp(const double *x, const int a, const int b, const bool min, const bool nalast) {
  if (x[a]==x[b] || isnan(x[a]) && isnan(x[b])) return(a > b);
  if (isnan(x[a])) return(nalast);
  if (isnan(x[b])) return(!nalast);
  return(min ? x[a] < x[b] : x[a] > x[b]);
}

static inline bool i64cmp(const int64_t *x, const int a, const int b, const bool min, const bool nalast) {
  if (x[a]==x[b]) return(a > b);
  if (x[a]==NA_INTEGER64) return(nalast);
  if (x[b]==NA_INTEGER64) return(!nalast);
  return(min ? x[a] < x[b] : x[a] > x[b]);
}

static inline bool scmp(const SEXP *restrict x, const int a, const int b, const bool min, const bool nalast) {
  if (strcmp(CHAR(x[a]), CHAR(x[b])) == 0) return (a > b);
  if (x[a]==NA_STRING) return(nalast);
  if (x[b]==NA_STRING) return(!nalast);
  return(min ? strcmp(CHAR(x[a]),CHAR(x[b]))<0 : strcmp(CHAR(x[a]),CHAR(x[b])))>0;
}

static inline bool ccmp(const Rcomplex *x, const int a, const int b, const bool min, const bool nalast) {
  if ((isnan(x[a].r) || isnan(x[a].i)) && (isnan(x[b].r) || isnan(x[b].i))) return (a > b);
  if (x[a].r==x[b].r) {
    if (x[a].i==x[b].i) return(a > b);
    return(min ? x[a].i < x[b].i : x[a].i > x[b].i);
  }
  if (isnan(x[a].r) || isnan(x[a].i)) return(nalast);
  if (isnan(x[b].r) || isnan(x[b].i)) return(!nalast);
  return(min ? x[a].r < x[b].r : x[a].r > x[b].r);
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
    if (l < len && CMP(VAL,IND[l],IND[smallest],min,nalast))          \
      smallest = l;                                                   \
    if (r < len && CMP(VAL,IND[r],IND[smallest],min,nalast))          \
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
    if (CMP(VAL,IND[0],i,min,nalast)) {             \
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
  case LGLSXP: case INTSXP: {          TOPN(int,      INTEGER,    icmp); } break;
  case REALSXP: {
    if (INHERITS(x, char_integer64)) { TOPN(int64_t,  REAL,       i64cmp); }
    else {                             TOPN(double,   REAL,       dcmp); } break; }
  case CPLXSXP: {                      TOPN(Rcomplex, COMPLEX,    ccmp); } break;
  case STRSXP: {                       TOPN(SEXP,     STRING_PTR, scmp); } break;
  default:
    free(IND); error(_("Type '%s' not supported by topn"), type2char(TYPEOF(x)));
  }
  UNPROTECT(1);
  return(ans);
}
