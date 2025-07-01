#include "data.table.h"

// from good ol' Numerical Recipes in C

static inline void iswap(int *a, int *b)           {int     tmp=*a; *a=*b; *b=tmp;}
static inline void dswap(double *a, double *b)     {double  tmp=*a; *a=*b; *b=tmp;}
static inline void i64swap(int64_t *a, int64_t *b) {int64_t tmp=*a; *a=*b; *b=tmp;}

#undef BODY
#define BODY(SWAP)                          \
  if (n==0) return NA_REAL;                 \
  int med = n/2 - (n%2==0);                 \
  unsigned long ir=n-1, l=0;                \
  for(;;) {                                 \
    if (ir <= l+1) {                        \
      if (ir == l+1 && x[ir] < x[l]) {      \
        SWAP(x+l, x+ir);                    \
      }                                     \
      break;                                \
    } else {                                \
      unsigned long mid=(l+ir) >> 1;        \
      SWAP(x+mid, x+l+1);                   \
      if (x[l] > x[ir]) {                   \
        SWAP(x+l, x+ir);                    \
      }                                     \
      if (x[l+1] > x[ir]) {                 \
        SWAP(x+l+1, x+ir);                  \
      }                                     \
      if (x[l] > x[l+1]) {                  \
        SWAP(x+l, x+l+1);                   \
      }                                     \
      unsigned long i=l+1, j=ir;            \
      a=x[l+1];                             \
      for (;;) {                            \
        do i++; while (x[i] < a);           \
        do j--; while (x[j] > a);           \
        if (j < i) break;                   \
          SWAP(x+i, x+j);                   \
      }                                     \
      x[l+1]=x[j];                          \
      x[j]=a;                               \
      if (j >= med) ir=j-1;                 \
      if (j <= med) l=i;                    \
    }                                       \
  }                                         \
  a = x[med];                               \
  if (n%2 == 1) {                           \
    return (double)a;                       \
  } else {                                  \
    b = x[med+1];                           \
    for (int i=med+2; i<n; ++i) {           \
      if (x[i]<b) b=x[i];                   \
    }                                       \
    return ((double)a+(double)b)/2.0;       \
  }

double dquickselect(double *x, int n) {
  double a, b;
  BODY(dswap);
}

double iquickselect(int *x, int n) {
  int a, b;
  BODY(iswap);
}

double i64quickselect(int64_t *x, int n) {
  int64_t a, b;
  BODY(i64swap);
}
