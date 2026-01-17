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

#undef DEFINE_FLOYD_RIVEST_SELECT_FN
#define DEFINE_FLOYD_RIVEST_SELECT_FN(SELECT_FN, TYPE, SWAP)               \
  static void SELECT_FN(TYPE *x, unsigned long left, unsigned long right, unsigned long k) { \
    while (right > left) {                                                 \
      if (right - left > 600) {                                            \
        unsigned long n = right - left + 1;                                \
        unsigned long i = k - left + 1;                                    \
        double z = log((double)n);                                         \
        double s = 0.5 * exp(2.0 * z / 3.0);                               \
        double sd = 0.5 * sqrt(z * s * (n - s) / n);                       \
        long delta = (long)i - (long)(n / 2);                              \
        if (delta < 0) sd = -sd;                                           \
        else if (delta == 0) sd = 0.0;                                     \
        double new_left = (double)k - (double)i * s / n + sd;              \
        double new_right = (double)k + (double)(n - i) * s / n + sd;       \
        unsigned long newL = (unsigned long)MAX((long)left, (long)new_left); \
        unsigned long newR = (unsigned long)MIN((long)right, (long)new_right); \
        SELECT_FN(x, newL, newR, k);                                       \
      }                                                                    \
      TYPE pivot = x[k];                                                   \
      unsigned long i = left, j = right;                                   \
      SWAP(x + left, x + k);                                               \
      if (x[right] > pivot) {                                              \
        SWAP(x + right, x + left);                                         \
      }                                                                    \
      while (i < j) {                                                      \
        SWAP(x + i, x + j);                                                \
        i++; j--;                                                          \
        while (x[i] < pivot) i++;                                          \
        while (x[j] > pivot) j--;                                          \
      }                                                                    \
      if (x[left] == pivot) {                                              \
        SWAP(x + left, x + j);                                             \
      } else {                                                             \
        j++;                                                               \
        SWAP(x + j, x + right);                                            \
      }                                                                    \
      if (j <= k) left = j + 1;                                            \
      if (k <= j) right = j - 1;                                           \
    }                                                                      \
  }

DEFINE_FLOYD_RIVEST_SELECT_FN(dfloyd_rivest_select, double, dswap)
DEFINE_FLOYD_RIVEST_SELECT_FN(ifloyd_rivest_select, int, iswap)
DEFINE_FLOYD_RIVEST_SELECT_FN(i64floyd_rivest_select, int64_t, i64swap)

#undef FLOYD_RIVEST_BODY
#define FLOYD_RIVEST_BODY(TYPE, SELECTFN)                                  \
  if (n == 0) return NA_REAL;                                              \
  unsigned long med = n / 2 - (n % 2 == 0);                                \
  SELECTFN(x, 0, n - 1, med);                                              \
  a = x[med];                                                              \
  if (n % 2 == 1) {                                                        \
    return (double)a;                                                      \
  } else {                                                                 \
    b = x[med + 1];                                                        \
    for (unsigned long i = med + 2; i < (unsigned long)n; i++) {           \
      if (x[i] < b) b = x[i];                                              \
    }                                                                      \
    return ((double)a + (double)b) / 2.0;                                  \
  }

double dfloyd_rivest(double *x, int n)
{
  double a, b;
  FLOYD_RIVEST_BODY(double, dfloyd_rivest_select);
}

double ifloyd_rivest(int *x, int n)
{
  int a, b;
  FLOYD_RIVEST_BODY(int, ifloyd_rivest_select);
}

double i64floyd_rivest(int64_t *x, int n)
{
  int64_t a, b;
  FLOYD_RIVEST_BODY(int64_t, i64floyd_rivest_select);
}
