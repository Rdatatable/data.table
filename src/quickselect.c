#include "data.table.h"

// from good ol' Numerical Recipes in C

static inline void iswap(int *a, int *b)           { int     tmp = *a; *a = *b; *b = tmp; }
static inline void dswap(double *a, double *b)     { double  tmp = *a; *a = *b; *b = tmp; }
static inline void i64swap(int64_t *a, int64_t *b) { int64_t tmp = *a; *a = *b; *b = tmp; }

#undef BODY
#define BODY(SWAP)                          \
  if (n == 0) return NA_REAL;               \
  int med = n / 2 - (n % 2 == 0);           \
  unsigned long ir = n - 1, l = 0;          \
  for(;;) {                                 \
    if (ir <= l + 1) {                      \
      if (ir == l + 1 && x[ir] < x[l]) {    \
        SWAP(x + l, x + ir);                \
      }                                     \
      break;                                \
    } else {                                \
      unsigned long mid = (l + ir) >> 1;    \
      SWAP(x + mid, x + l + 1);             \
      if (x[l] > x[ir]) {                   \
        SWAP(x + l, x + ir);                \
      }                                     \
      if (x[l + 1] > x[ir]) {               \
        SWAP(x + l + 1, x + ir);            \
      }                                     \
      if (x[l] > x[l + 1]) {                \
        SWAP(x + l, x + l + 1);             \
      }                                     \
      unsigned long i = l + 1, j = ir;      \
      a = x[l + 1];                         \
      for (;;) {                            \
        do i++; while (x[i] < a);           \
        do j--; while (x[j] > a);           \
        if (j < i) break;                   \
          SWAP(x + i, x + j);               \
      }                                     \
      x[l + 1] = x[j];                      \
      x[j] = a;                             \
      if (j >= med) ir = j - 1;             \
      if (j <= med) l = i;                  \
    }                                       \
  }                                         \
  a = x[med];                               \
  if (n % 2 == 1) {                         \
    return (double)a;                       \
  } else {                                  \
    b = x[med + 1];                         \
    for (int i = med + 2; i < n; i++) {     \
      if (x[i] < b) b = x[i];               \
    }                                       \
    return ((double)a + (double)b) / 2.0;   \
  }

double dquickselect(double *x, int n)
{
  double a, b;
  BODY(dswap);
}

double iquickselect(int *x, int n)
{
  int a, b;
  BODY(iswap);
}

double i64quickselect(int64_t *x, int n)
{
  int64_t a, b;
  BODY(i64swap);
}

// Floyd-Rivest selection algorithm
// https://en.wikipedia.org/wiki/Floyd%E2%80%93Rivest_algorithm

#undef FLOYD_RIVEST_BODY
#define FLOYD_RIVEST_BODY(SWAP, TYPE)                                      \
  if (n == 0) return NA_REAL;                                              \
  unsigned long med = n / 2 - (n % 2 == 0);                                \
  unsigned long l = 0, ir = n - 1;                                         \
  while (ir > l) {                                                         \
    if (ir - l > 600) {                                                    \
      unsigned long nn = ir - l + 1;                                       \
      unsigned long i = med - l + 1;                                       \
      double z = log((double)nn);                                          \
      double s = 0.5 * exp(2.0*z/3.0);                                     \
      double sd = 0.5 * sqrt(z*s*(nn-s)/nn);                               \
      if (i < nn/2) sd = -sd;                                              \
      unsigned long newL = (unsigned long)(MAX((long)l, (long)(med - i*s/nn + sd))); \
      unsigned long newR = (unsigned long)(MIN((long)ir, (long)(med + (nn-i)*s/nn + sd))); \
      l = newL;                                                            \
      ir = newR;                                                           \
    }                                                                      \
    TYPE pivot = x[med];                                                   \
    unsigned long i = l, j = ir;                                           \
    SWAP(x + l, x + med);                                                  \
    if (x[ir] > pivot) {                                                   \
      SWAP(x + ir, x + l);                                                 \
      pivot = x[l];                                                        \
    }                                                                      \
    while (i < j) {                                                        \
      SWAP(x + i, x + j);                                                  \
      i++; j--;                                                            \
      while (x[i] < pivot) i++;                                            \
      while (x[j] > pivot) j--;                                            \
    }                                                                      \
    if (x[l] == pivot) {                                                   \
      SWAP(x + l, x + j);                                                  \
    } else {                                                               \
      j++;                                                                 \
      SWAP(x + j, x + ir);                                                 \
    }                                                                      \
    if (j <= med) l = j + 1;                                               \
    if (med <= j) ir = j - 1;                                              \
  }                                                                        \
  a = x[med];                                                              \
  if (n % 2 == 1) {                                                        \
    return (double)a;                                                      \
  } else {                                                                 \
    b = x[med + 1];                                                        \
    for (unsigned long i = med + 2; i < n; i++) {                          \
      if (x[i] < b) b = x[i];                                              \
    }                                                                      \
    return ((double)a + (double)b) / 2.0;                                  \
  }

double dfloyd_rivest(double *x, int n)
{
  double a, b;
  FLOYD_RIVEST_BODY(dswap, double);
}

double ifloyd_rivest(int *x, int n)
{
  int a, b;
  FLOYD_RIVEST_BODY(iswap, int);
}

double i64floyd_rivest(int64_t *x, int n)
{
  int64_t a, b;
  FLOYD_RIVEST_BODY(i64swap, int64_t);
}
