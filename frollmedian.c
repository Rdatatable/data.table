#include "frollmedian.h"

#ifdef MIN
#  undef MIN
#endif
#define MIN(a,b) (((a)<(b))?(a):(b))

SEXP frollmedianR(SEXP x, SEXP k, SEXP narm, SEXP verbose) {
  double *xp = REAL(x);
  uint64_t nx = LENGTH(x);
  int ik = INTEGER(k)[0];
  int inarm = INTEGER(narm)[0];
  if (inarm!=0 && inarm!=1) {
    error("'inarm' must be TRUE or FALSE");
  }
  int iverbose = LOGICAL(verbose)[0];
  if (iverbose!=0 && iverbose!=1) {
    error("'verbose' must be TRUE or FALSE");
  }
  SEXP ans = PROTECT(allocVector(REALSXP, nx));
  double *ansp = REAL(ans);
  if (ik <= nx) {
    frollmedianFast(xp, nx, ansp, ik, /*fill=*/NA_REAL, /*narm=*/inarm, /*hasnf=*/0, /*verbose=*/iverbose);
  } else {
    for (int i=0; i<nx; i++) ansp[i] = NA_REAL;
  }
  UNPROTECT(1);
  return ans;
}

// initialize prev and next pointers for list L
static void setlinks(int *o, int *next, int *prev, int tail) {
  int p = tail;
  int k = tail;
  int q;
  for (int i=0; i<k; i++) {
    q = o[i];
    next[p] = q;
    prev[q] = p;
    p = q;
  }
  next[p] = tail;
  prev[tail] = p;
}

// Return the first large element in block, or Inf if all elements are small
static double peek(double *x, int m, int tail) {
  return m == tail ? R_PosInf : x[m];
}
#define PEEK(j) peek(&x[(j)*k], m[(j)], tail)
#define PEEK2(j) peek(&x[(j)*k], n[(j)], tail)
// return median, for even k, from x_A[m_a], x_A[n_A], x_B[m_B], x_B[n_B] - having two sets of sorted 2 elements, take two smallest and give their mean
#define MED(A, B) MIN(PEEK(A), PEEK(B))
static double med2(double *xa, double *xb, int ma, int na, int mb, int nb, int tail) {
  double xam = ma==tail ? R_PosInf : xa[ma];
  double xan = na==tail ? R_PosInf : xa[na];
  double xbm = mb==tail ? R_PosInf : xb[mb];
  double xbn = nb==tail ? R_PosInf : xb[nb];
  if (xam==xbm) {
    return xam;
  } else if (xam<xbm) { // so xam<xbn
    if (xan<=xbm) { // so xan<=xbn
      return (xam+xan)/2;
    } else {
      return (xam+xbm)/2;
    }
  } else { // xbm<xam
    if (xbn<=xam) {
      return (xbm+xbn)/2;
    } else {
      return (xbm+xam)/2;
    }
  }
}
#define MED2(A, B) med2(&x[(A)*k], &x[(B)*k], m[(A)], n[(A)], m[(B)], n[(B)], tail)
// print debug info about currently rolling window
/*void debug(int j, int i, int k, double *x, int *m, int *n, int *s, int tail, bool even) {
  Rprintf("window for %d element using block", j*k+i);
  j ? Rprintf("s A=%d B=%d", j-1, j) : Rprintf(" B=%d", j);
  // x
  Rprintf("\n  x_A:       [");
  if (j) for (int ii=0; ii<k; ii++) ii<k-1 ? Rprintf("%.0f, ", x[(j-1)*k+ii]) : Rprintf("%.0f", x[(j-1)*k+ii]);
  Rprintf("]");
  Rprintf("\n  x_B:       [");
  for (int ii=0; ii<k; ii++) ii<k-1 ? Rprintf("%.0f, ", x[j*k+ii]) : Rprintf("%.0f", x[j*k+ii]);
  Rprintf("]");
  // L // not sorted
  Rprintf("\n  L_A:       [");
  //for (int ii=i+1; ii<k; ii++) Rprintf("%d, ", (j-1)*k+ii);
  for (int ii=i+1; ii<k; ii++) ii<k-1 ? Rprintf("%.0f, ", x[(j-1)*k+ii]) : Rprintf("%.0f", x[(j-1)*k+ii]);
  Rprintf("]");
  Rprintf("\n  L_B:       [");
  //for (int ii=0; ii<i+1; ii++) Rprintf("%d, ", j*k+ii);
  for (int ii=0; ii<i+1; ii++) ii<i ? Rprintf("%.0f, ", x[j*k+ii]) : Rprintf("%.0f", x[j*k+ii]);
  Rprintf("]");
  // L_A + L_B // not sorted
  Rprintf("\n  L_A + L_B: [");
  //for (int ii=i-k+1; ii<i+1; ii++) Rprintf("%d, ", ii);
  for (int ii=i-k+1; ii<i+1; ii++) ii<i ? Rprintf("%.0f, ", x[j*k+ii]) : Rprintf("%.0f", x[j*k+ii]);
  Rprintf("]");
  // s
  Rprintf("\n  s_A:       %d", j ? s[j-1] : 0);
  Rprintf("\n  s_B:       %d", s[j]);
  // m
  Rprintf("\n  m_A:       ");
  if (j) {
    if (m[j-1]==k) Rprintf("END");
    else Rprintf("%d; x_A[%d] = %.0f", m[j-1], m[j-1], x[(j-1)*k+m[j-1]]);
  } else {
    Rprintf("NA");
  }
  Rprintf("\n  n_A:       ");
  if (j && even) {
    if (n[j-1]==k) Rprintf("END");
    else Rprintf("%d; x_A[%d] = %.0f", n[j-1], n[j-1], x[(j-1)*k+n[j-1]]);
  } else {
    Rprintf("NA");
  }
  Rprintf("\n  m_B:       ");
  if (m[j]==k) Rprintf("END");
  else Rprintf("%d; x_B[%d] = %.0f", m[j], m[j], x[j*k+m[j]]);
  Rprintf("\n  n_B:       ");
  if (even) {
    if (n[j]==k) Rprintf("END");
    else Rprintf("%d; x_B[%d] = %.0f", n[j], n[j], x[j*k+n[j]]);
  } else {
    Rprintf("NA");
  }
  // median
  if (j) {
    if (even) {
      Rprintf("\n  PEEK(A):   %.1f", PEEK(j-1));
      Rprintf("\n  PEEK2(A):  %.1f", PEEK2(j-1));
      Rprintf("\n  PEEK(B):   %.1f", PEEK(j));
      Rprintf("\n  PEEK2(B):  %.1f", PEEK2(j));
      Rprintf("\n  MED2:      %.1f", MED2(j-1, j));
      Rprintf("\n  Y[%d]:      %.1f", j*k+i, MED2(j-1, j));
    } else {
      Rprintf("\n  Y[%d]:      %.1f", j*k+i, MED(j-1, j));
    }
  } else {
    Rprintf("\n  PEEK(B):   %.1f", PEEK(j));
    Rprintf("\n  PEEK2(B):  %.1f", PEEK2(j));
    Rprintf("\n  Y[%d]:      %.1f", j*k+i, even ? (PEEK(j)+PEEK2(j))/2 : PEEK(j));  
  }
  Rprintf("\n");
}
#define DEBUG(j, i) debug(j, i, k, x, m, n, s, tail, even);*/
// delete all elements from B in reverse order
static void unwind(int *prev, int *next, int *m, int *s, int k, int tail) {
  for (int i=k-1; i>=0; i--) {
    next[prev[i]] = next[i];
    prev[next[i]] = prev[i];
  }
  m[0] = tail;
  s[0] = 0;
}
#define UNWIND(j) unwind(&prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &s[(j)], k, tail);
static void unwind2(int *prev, int *next, int *m, int *n, int *s, int k, int tail, bool even) {
  for (int i=k-1; i>=0; i--) {
    next[prev[i]] = next[i];
    prev[next[i]] = prev[i];
  }
  m[0] = tail;
  if (even) n[0] = tail;
  s[0] = 0;
}
#define UNWIND2(j) unwind2(&prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &n[(j)], &s[(j)], k, tail, even);
// checks if object i is "small" - smaller than value(s) used for median answer
bool small(int i, double *x, int m, int tail) {
  return (m == tail) || (x[i] < x[m]) || ((x[i] == x[m]) && (i < m));
}
#define SMALL(i) small((i), x, m[0], tail)
#define SMALL2(i) small((i), x, n[0], tail)
// delete element from A
static void delete(int i, double *x, int *prev, int *next, int *m, int *s, int tail) {
  next[prev[i]] = next[i];
  prev[next[i]] = prev[i];
  if (SMALL(i)) {
    s[0]--;
  } else {
    if (m[0] == i) {
      m[0] = next[m[0]];
    }
    if (s[0] > 0) {
      m[0] = prev[m[0]];
      s[0]--;
    }
  }
}
#define DELETE(j) delete(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &s[(j)], tail)
static void delete2(int i, double *x, int *prev, int *next, int *m, int *n, int *s, int tail, bool even) {
  next[prev[i]] = next[i];
  prev[next[i]] = prev[i];
  if (SMALL(i)) {
    s[0]--;
  } else {
    if (m[0] == i) {
      m[0] = next[m[0]];
      if (even) n[0] = next[n[0]];
    } else if (even && n[0] == i) {
      n[0] = next[n[0]];
    }
    if (s[0] > 0) {
      m[0] = prev[m[0]];
      if (even) n[0] = prev[n[0]];
      s[0]--;
    }
  }
}
#define DELETE2(j) delete2(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &n[(j)], &s[(j)], tail, even)
// undelete element from B
static void undelete(int i, double *x, int *prev, int *next, int *m, int tail) {
  next[prev[i]] = i;
  prev[next[i]] = i;
  if (SMALL(i)) {
    m[0] = prev[m[0]];
  }
}
#define UNDELETE(j) undelete(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], tail)
static void undelete2(int i, double *x, int *prev, int *next, int *m, int *n, int tail, bool even) {
  next[prev[i]] = i;
  prev[next[i]] = i;
  if (SMALL(i)) {
    m[0] = prev[m[0]];
    if (even) n[0] = prev[n[0]];
  } else if (even && SMALL2(i)) {
    n[0] = prev[n[0]];
  }
}
#define UNDELETE2(j) undelete2(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &n[(j)], tail, even)
// advance - balance block after delete/undelete, fixed m (and n) pointers and s counter
static void advance(int *next, int* m, int *s) {
  m[0] = next[m[0]];
  s[0]++;
}
#define ADVANCE(j) advance(&next[(j)*(k+1)], &m[(j)], &s[(j)])
static void advance2(int *next, int* m, int* n, int *s, bool even) {
  m[0] = next[m[0]];
  if (even) n[0] = next[n[0]];
  s[0]++;
}
#define ADVANCE2(j) advance2(&next[(j)*(k+1)], &m[(j)], &n[(j)], &s[(j)], even)

void frollmedianFast(double *x, uint64_t nx, /*to be ans_t*/double *ans, int k, double fill, bool narm, int hasnf, bool verbose) {

  double tic = 0;
  // special case for k==1 and k==2
  bool esc = false;
  if (k==1) {
    if (verbose)
      Rprintf("k==1\n");
    for (int i=0; i<nx; i++) // na.rm included
      ans[i] = x[i];
    esc = true;
  } else if (k==2) {
    if (verbose)
      Rprintf("k==2\n");
    ans[0] = fill;
    if (narm) {
      for (int i=1; i<nx; i++) {
        if (ISNAN(x[i])) {
          if (ISNAN(x[i-1])) {
            ans[i] = NA_REAL;
          } else {
            ans[i] = x[i-1];
          }
        } else {
          if (ISNAN(x[i-1])) {
            ans[i] = x[i];
          } else {
            ans[i] = (x[i]+x[i-1])/2;
          }
        }
      }
    } else {
      for (int i=1; i<nx; i++)
        ans[i] = (x[i]+x[i-1])/2;
    }
    esc = true;
  }
  if (esc) {
    return;
  }

  // handling of nx not multiple of k
  int nx_mod_k = nx % k;
  if (nx_mod_k) {
    if (verbose)
      Rprintf("%s: nx=%"PRIu64" is not multiple of k=%d, padding with %d elements, new nx=%"PRIu64"\n", __func__, nx, k, k-nx_mod_k, nx+(k-nx_mod_k));
    nx = nx + (k-nx_mod_k);
    // TODO
    // x = realloc(x, nx); // to avoid memory sanitizer issues, possibly we could branch forder calls and then no need to realloc but only new nx is enough
    // OR
    // handle last block finding order when nx%k > 0, otherwise memory sanitizers will warn, *x is initialized for original nx, not padded
  }

  int b = nx/k;
  bool even = !(k % 2);
  int h = even ? k/2-1 : (k-1)/2;
  int tail = k;

  // single array for all blocks
  if (verbose)
    tic = omp_get_wtime();
  // TODO turn to malloc so this function is 1) thread safe 2) not terminate other than via ans_t signal
  // permutation that sorts input vector x
  int *o = (int*)R_alloc(nx, sizeof(int));
  // pointer to median candidate in that L, initialized to first large element in L
  int *m = (int*)R_alloc(b, sizeof(int));
  // pointer to median candidate of a pair mid values in that L for even k, initialized to second large element
  int *n = (int*)R_alloc(b, sizeof(int));
  // counter between 0 and k, number of 'small' elements in list L
  int *s = (int*)R_alloc(b, sizeof(int));
  // pointer to previous element in list L
  int* prev = (int*)R_alloc(b*(k+1), sizeof(int));
  // pointer to next element in list L
  int* next = (int*)R_alloc(b*(k+1), sizeof(int));
  if (verbose)
    Rprintf("%s: allocations took %.3fs\n", __func__, omp_get_wtime()-tic);

  if (verbose)
    tic = omp_get_wtime();
  // TODO: replace R_orderVector1 by R agnostic routine, use parallel for
  SEXP rx = PROTECT(allocVector(REALSXP, k));
  double *rxp = REAL(rx);
  for (int j=0; j<b; j++) {
    memcpy(rxp, &x[j*k], k*sizeof(double));
    R_orderVector1(&o[j*k], k, rx, 1, 0);
  }
  UNPROTECT(1); // rx
  // end of TODO
  if (verbose)
    Rprintf("%s: finding order for %d blocks took %.3fs\n", __func__, b, omp_get_wtime()-tic);

  if (verbose)
    tic = omp_get_wtime();
  for (int j=0; j<b; j++) { // easily parallel now but nothing to gain
    m[j] = o[j*k+h];
    if (even) n[j] = o[j*k+h+1];
    s[j] = h;
    setlinks(&o[j*k], &next[j*(k+1)], &prev[j*(k+1)], tail);
  }
  if (verbose)
    Rprintf("%s: initialization for %d blocks took %.3fs\n", __func__, b, omp_get_wtime()-tic);

  for (int i=0; i<k-1; i++)
    ans[i] = fill;
  if (verbose)
    tic = omp_get_wtime();
  // even-k-aware supports uneven-k as well without much overhead, disabled branch is left for potential future internal use and for reference of the algo as in paper
  // TODO add a switch to fall back to this uneven-k only version for testing in case of uneven k and no NAs
  if (false && !even) {
    ans[k-1] = PEEK(0);
    if (verbose) {
      Rprintf("%s: k=%d is an uneven window size, running sort-median as described in the paper by Jukka Suomela\n", __func__, k);
    }
    for (int j=1; j<b; j++) { //Rprintf("j=%d\n", j);
      int A = j-1, B = j;
      UNWIND(B);
      /*stopifnot*/ if (!(s[A] == h)) {
        REprintf("'s[A] == h' is not true\n");
        return;
      }
      /*stopifnot*/ if (!(s[B] == 0)) {
        REprintf("'s[B] == 0' is not true\n");
        return;
      }
      for (int i=0; i<k; i++) { //Rprintf("i=%d\n", i);
        if (nx_mod_k && j==b-1) {
          if (verbose && i==nx_mod_k)
            Rprintf("%s: skip rolling for %d padded elements\n", __func__, k-nx_mod_k);
          if (i>=nx_mod_k)
            continue;
        }
        DELETE(A);
        UNDELETE(B);
        /*stopifnot*/ if (!(s[A] + s[B] <= h)) {
          REprintf("'s[A] + s[B] <= h' is not true\n");
          return;
        }
        if (s[A] + s[B] < h) {
          if (PEEK(A) <= PEEK(B)) {
            ADVANCE(A);
          } else {
            ADVANCE(B);
          }
        }
        /*stopifnot*/ if (!(s[A] + s[B] == h)) {
          REprintf("'s[A] + s[B] == h' is not true\n");
          return;
        }
        ans[j*k+i] = MED(A, B);
      }
    }
  } else {
    ans[k-1] = even ? (PEEK(0) + PEEK2(0)) / 2 : PEEK(0);
    if (verbose) {
      Rprintf("%s: k=%d is an even window size, running more experimental - even-k aware - version\n", __func__, k);
    }
    for (int j=1; j<b; j++) { //Rprintf("j=%d\n", j);
      int A = j-1, B = j;
      //Rprintf("before unwind\n");
      UNWIND2(B);
      /*stopifnot*/ if (!(s[A] == h)) {
        REprintf("'s[A] == h' is not true\n");
        return;
      }
      /*stopifnot*/ if (!(s[B] == 0)) {
        REprintf("'s[B] == 0' is not true\n");
        return;
      }
      for (int i=0; i<k; i++) { //Rprintf("i=%d\n", i);
        //bool debug = k; //j*k+i==5;
        //if (debug) Rprintf("  # after\t\tA\t\tB\n", m[A], n[A], m[B], n[B]);
        //if (debug) Rprintf("  # init\t\tm=%d, n=%d, s=%d\tm=%d, n=%d, s=%d\n", m[A], n[A], s[A], m[B], n[B], s[B]);
        if (nx_mod_k && j==b-1) {
          if (verbose && i==nx_mod_k)
            Rprintf("%s: skip rolling for %d padded elements\n", __func__, k-nx_mod_k);
          if (i>=nx_mod_k)
            continue;
        }
        //Rprintf("before delete\n");
        DELETE2(A);
        //if (debug) Rprintf("  # delete\t\tm=%d, n=%d, s=%d\n", m[A], n[A], s[A]);
        //Rprintf("before undelete\n");
        UNDELETE2(B);
        //if (debug) Rprintf("  # undelete\t\t\t\tm=%d, n=%d, s=%d\n", m[B], n[B], s[B]);
        /*stopifnot*/ if (!(s[A] + s[B] <= h)) {
          REprintf("'s[A] + s[B] <= h' is not true\n");
          return;
        }
        //Rprintf("before advance\n");
        if (s[A] + s[B] < h) {
          if (PEEK(A) <= PEEK(B)) {
            ADVANCE2(A);
          } else {
            ADVANCE2(B);
          }
        }
        /*stopifnot*/ if (!(s[A] + s[B] == h)) {
          REprintf("'s[A] + s[B] == h' is not true\n");
          return;
        }
        if (n[A]!=tail && m[A] == n[A]) {
          n[A] = tail;
        }
        if (n[B]!=tail && m[B] == n[B]) {
          n[B] = tail;
        }
        //if (debug) Rprintf("  # advance\t\tm=%d, n=%d, s=%d\tm=%d, n=%d, s=%d\n", m[A], n[A], s[A], m[B], n[B], s[B]);
        //Rprintf("before debug\n");
        //DEBUG(B, i);
        ans[j*k+i] = even ? MED2(A, B) : MED(A, B);
      }
    }
  }
  if (verbose)
    Rprintf("%s: rolling took %.3fs\n", __func__, omp_get_wtime()-tic);
}
