#include "frollmedian.h"

#ifdef MIN
#  undef MIN
#endif
#define MIN(a,b) (((a)<(b))?(a):(b))

SEXP frollmedianR(SEXP x, SEXP k) {
  //Rprintf("frollmedianR\n");
  double *xp = REAL(x);
  uint64_t nx = LENGTH(x);
  int ik = INTEGER(k)[0];

  //Rprintf("ans alloc\n");
  SEXP ans = PROTECT(allocVector(REALSXP, nx));
  double *ansp = REAL(ans);
  //Rprintf("ans set 0\n");
  for (int i=0; i<nx; i++) ansp[i] = 0; //temp
  //Rprintf("frollmedianFast\n");
  frollmedianFast(xp, nx, ansp, ik, /*fill=*/NA_REAL, /*narm=*/0, /*hasnf=*/0, /*verbose=*/0);

  UNPROTECT(1);
  return ans;
}

// DEBUG
void printB(double *x, int *o, int *next, int *prev, int m, int n, int s, int k, int j) {
  Rprintf("block #%d", j);
  Rprintf("\n  x: ");
  for (int i=0; i<k; i++) Rprintf("%.0f, ", x[i]);
  /*Rprintf("\n  o: ");
  for (int i=0; i<k; i++) Rprintf("%d, ", o[i]);
  Rprintf("\n  prev: ");
  for (int i=0; i<k+1; i++) Rprintf("%d, ", prev[i]);
  Rprintf("\n  next: ");
  for (int i=0; i<k+1; i++) Rprintf("%d, ", next[i]);*/
  Rprintf("\n  m: %d", m);
  //Rprintf("\n  n: %d", n);
  Rprintf("\n  s: %d", s);
  Rprintf("\n");
}
#define PRINT_B(j) (printB(&x[(j)*k], &o[(j)*k], &next[(j)*(k+1)], &prev[(j)*(k+1)], m[(j)], n[(j)], s[(j)], k, (j)))

// initialize blocks prev and next pointers
// TODO check if we really need that for blocks j>1 because unwind may be already destroying that
static void links(int *o, int *next, int *prev, int tail) {
  int verbose = 0;
  int p = tail;
  if (verbose) Rprintf("p: %d\n", p);
  int k = tail;
  int q;
  for (int i=0; i<k; i++) {
    if (verbose) Rprintf("## i: %d\n", i);
    q = o[i];
    if (verbose) Rprintf("q: %d\n", q);
    next[p] = q;
    if (verbose) Rprintf("next[p]: %d\n", next[p]);
    prev[q] = p;
    if (verbose) Rprintf("prev[q]: %d\n", prev[q]);
    p = q;
    if (verbose) Rprintf("p: %d\n", p);
  }
  next[p] = tail;
  if (verbose) Rprintf("next[p]: %d\n", next[p]);
  prev[tail] = p;
  if (verbose) Rprintf("prev[tail]: %d\n", prev[tail]);
}

// "Return the first large element, or Inf if all elements are small"
static double peek(double *x, int m, int tail) {
  if (m == tail)
    return R_PosInf;
  else
    return x[m];
}
#define PEEK(j) peek(&x[(j)*k], m[(j)], tail)
#define PEEK2(j) peek(&x[(j)*k], n[(j)], tail)

// k even - get median
// x_A[m_a], x_A[n_A], x_B[m_B], x_B[n_B]
// having two sets of sorted 2 elements, take two smallest and give their mean
static double med2(double *xa, double *xb, int ma, int na, int mb, int nb, int tail) {
  double xam = ma==tail ? R_PosInf : xa[ma];
  double xan = na==tail ? R_PosInf : xa[na];
  double xbm = mb==tail ? R_PosInf : xb[mb];
  double xbn = nb==tail ? R_PosInf : xb[nb];
  if (xam==xbm) {
    return xam;
  } else if (xam<xbm) { // so xam<xbn
    //Rprintf("\nxam<xbm");
    if (xan<=xbm) { // so xan<=xbn
      //Rprintf("\nxan<=xbm");
      return (xam+xan)/2;
    } else {
      //Rprintf("\nxan>xbm");
      return (xam+xbm)/2;
    }
  } else { // xbm<xam
    //Rprintf("\nxbm<xam\n");
    if (xbn<=xam) {
      return (xbm+xbn)/2;
    } else {
      return (xbm+xam)/2;
      //Rprintf("\nxbn>xam\n");
      /*if (xbn<=xan) {
        return (xbm+xam)/2;
      } else {
        //Rprintf("\nxbn>xan\n");
        return (xbm+xam)/2; // take xam as it is always <= than xan
      }*/
    }
  }
}
#define MED2 med2(&x[(j-1)*k], &x[(j)*k], m[(j-1)], n[(j-1)], m[(j)], n[(j)], tail)

void printW(int j, int i, int k, double *x, int *m, int *n, int *s, int tail) {
  bool even = !(k%2);
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
      Rprintf("\n  MED2:      %.1f", MED2);
      Rprintf("\n  Y[%d]:      %.1f", j*k+i, MED2);
    } else {
      Rprintf("\n  Y[%d]:      %.1f", j*k+i, MIN(PEEK(j-1), PEEK(j)));
    }
  } else {
    Rprintf("\n  PEEK(B):   %.1f", PEEK(j));
    Rprintf("\n  PEEK2(B):  %.1f", PEEK2(j));
    Rprintf("\n  Y[%d]:      %.1f", j*k+i, even ? (PEEK(j)+PEEK2(j))/2 : PEEK(j));  
  }
  Rprintf("\n");
}
#define PRINT_W(j, i) printW(j, i, k, x, m, n, s, tail);

static void unwind(int *prev, int *next, int *m, int *n, int *s, int k, int tail) {
  for (int i=k-1; i>=0; i--) {
    next[prev[i]] = next[i];
    prev[next[i]] = prev[i];
  }
  m[0] = tail;
  n[0] = tail;
  s[0] = 0;
}
#define UNWIND(j) unwind(&prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &n[(j)], &s[(j)], k, tail);

bool small(int i, double *x, int m, int tail) {
  return (m == tail) || (x[i] < x[m]) || ((x[i] == x[m]) && (i < m));
  //if (m == tail) return true;
  //if (x[i] < x[m]) return true;
  //if ((x[i] == x[m]) && (i < m)) return true;
  //return false;
}

static void delete(int i, double *x, int *prev, int *next, int *m, int *n, int *s, int tail) {
  next[prev[i]] = next[i];
  prev[next[i]] = prev[i];
  if (small(i, x, m[0], tail)) {
    s[0]--;
  } else {
    //Rprintf("\nELSE\n");
    if (m[0] == i) {
      //Rprintf("m[0] == i\n");
      m[0] = next[m[0]];
      n[0] = next[n[0]];
    } else if (n[0] == i) {
      //Rprintf("n[0] == i\n");
      n[0] = next[n[0]];
    }
    if (s[0] > 0) {
      //Rprintf("s[0] > 0\n");
      m[0] = prev[m[0]];
      n[0] = prev[n[0]];
      s[0]--;
    }
  }
}
#define DELETE(j) delete(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &n[(j)], &s[(j)], tail)
static void undelete(int i, double *x, int *prev, int *next, int *m, int *n, int tail) {
  next[prev[i]] = i;
  prev[next[i]] = i;
  //if (i==2) Rprintf("undelete i=2: small(i, x, m[0], tail): %d\n", small(i, x, m[0], tail));
  if (small(i, x, m[0], tail)) {
    //Rprintf("undeleted object %d in B was small\nold m=%d, new m=%d\nold n=%d, new n=%d\n", i, m[0], prev[m[0]], n[0], prev[n[0]]);
    m[0] = prev[m[0]];
    n[0] = prev[n[0]]; // TODO can segfault for uneven k
  } else if (small(i, x, n[0], tail)) {
    n[0] = prev[n[0]]; // TODO can segfault for uneven k
  }
  //if (i==2) Rprintf("undelete i=2: small(i, x, n[0], tail): %d\n", small(i, x, n[0], tail));
}
#define UNDELETE(j) undelete(i, &x[(j)*k], &prev[(j)*(k+1)], &next[(j)*(k+1)], &m[(j)], &n[(j)], tail)
static void advance(int *next, int* m, int* n, int *s) {
  ///Rprintf("advance A or B\nold m=%d, new m=%d\nold n=%d, new n=%d\n", m[0], next[m[0]], n[0], next[n[0]]);
  m[0] = next[m[0]];
  n[0] = next[n[0]]; // TODO can segfault for uneven k
  s[0]++;
}
#define ADVANCE(j) advance(&next[(j)*(k+1)], &m[(j)], &n[(j)], &s[(j)])

void frollmedianFast(double *x, uint64_t nx, /*to be ans_t*/double *ans, int k, double fill, bool narm, int hasnf, bool verbose) {

  double tic = 0;
  // special case for k==1 and k==2
  bool esc = false;
  if (k==1) {
    if (verbose)
      Rprintf("k==1\n");
    if (narm) {
      // TODO na.rm handling
    } else {
      for (int i=0; i<nx; i++)
        ans[i] = x[i];
    }
    esc = true;
  } else if (k==2) {
    Rprintf("k==2\n");
    ans[0] = fill;
    if (narm) {
      // TODO na.rm handling
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
  int h = 0;
  bool even = !(k % 2);
  if (even) {
    if (verbose)
      Rprintf("%s: k=%d is an even window size...TODO\n", __func__, k);
    // TODO: what exactly todo is the mystery still
    h = k/2-1;
    /*
     k = 4
     L = 1, 2, 3, 4

     design decision:
     s = 1, m = 2, n = 3 (at that moment this)
     OR
     s = 2, m = 2, n = 3

     */
  } else {
    h = (k-1)/2;
  }
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
  // for uneven k, new block has s = h = (k-1)/2
  // for even k, s = h = k/2-1
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
  //#pragma omp parallel for num_threads(8)
  for (int j=0; j<b; j++) { // easily parallel now but nothing to gain
    m[j] = o[j*k+h];
    n[j] = o[j*k+h+1];
    s[j] = h;
    links(&o[j*k], &next[j*(k+1)], &prev[j*(k+1)], tail);
  }
  if (verbose)
    Rprintf("%s: initialization for %d blocks took %.3fs\n", __func__, b, omp_get_wtime()-tic);

  for (int i=0; i<k-1; i++)
    ans[i] = fill;

  //PRINT_W(0, k-1);
  //ans[k-1] = PEEK(0);
  // that's the question, or not yet for iter=0, but later in the rolling loop
  // how to get median having 4 values, 2 from A and 2 from B, or do we really have A:2 and B:2? maybe A:3 and B:1 or even A:0 and B:4
  ans[k-1] = even ? (PEEK(0) + PEEK2(0)) / 2 : PEEK(0);

  if (verbose)
    tic = omp_get_wtime();
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
      bool debug = 0; //j*k+i==5; //12 || j*k+i==13; //j==3 && (/*i==0 || i==1 || */i==0);
      if (debug) Rprintf("  # after\t\tA\t\tB\n", m[A], n[A], m[B], n[B]);
      if (debug) Rprintf("  # init\t\tm=%d, n=%d, s=%d\tm=%d, n=%d, s=%d\n", m[A], n[A], s[A], m[B], n[B], s[B]);
      if (nx_mod_k && j==b-1) {
        if (verbose && i==nx_mod_k)
          Rprintf("%s: skip rolling for %d padded elements\n", __func__, k-nx_mod_k);
        if (i>=nx_mod_k)
          continue;
      }
      DELETE(A);
      if (debug) Rprintf("  # delete\t\tm=%d, n=%d, s=%d\n", m[A], n[A], s[A]);
      UNDELETE(B);
      if (debug) Rprintf("  # undelete\t\t\t\tm=%d, n=%d, s=%d\n", m[B], n[B], s[B]);
      /*stopifnot*/ if (!(s[A] + s[B] <= h)) {
        REprintf("'s[A] + s[B] <= h' is not true\n");
        return;
      }
      // s[A]+s[B] could be now h or h-1, because advance increments only by one
      /*stopifnot*/ if (!((s[A] + s[B] == h) || (s[A] + s[B] == h-1))) {
        REprintf("'(s[A] + s[B] == h) || (s[A] + s[B] == h-1)' is not true\n");
        return;
      }
      if (s[A] + s[B] < h) {
        // if first large A is <= first large B
        // if median candidate of A is <= median candidate of B
        // x_A[m] >= x_B[m]
        // maybe no need to adjust here anything for even?
        if (PEEK(A) <= PEEK(B)) {
          //if (j==4 && i>=0) Rprintf("before advance A....\n");
          ADVANCE(A);
        } else {
          //if (j==4 && i>=0) Rprintf("before advance B....\n");
          ADVANCE(B);
        }
      }
      if (n[A]!=tail && m[A] == n[A]) {
        //Rprintf("m[A]==n[A], reset n\n");
        n[A] = tail;
      }
      if (n[B]!=tail && m[B] == n[B]) {
        //Rprintf("m[B]==n[B], reset n\n");
        n[B] = tail;
      }
      if (debug) Rprintf("  # advance\t\tm=%d, n=%d, s=%d\tm=%d, n=%d, s=%d\n", m[A], n[A], s[A], m[B], n[B], s[B]);
      /*stopifnot*/ if (!(s[A] + s[B] == h)) {
        REprintf("'s[A] + s[B] == h' is not true\n");
        return;
      }
      //PRINT_W(B, i);
      //ans[j*k+i] = MIN(PEEK(A), PEEK(B));
      // REVISIT
      if (even) {
        ans[j*k+i] = MED2;
      } else {
        ans[j*k+i] = MIN(PEEK(A), PEEK(B));
      }
    }
  }
  if (verbose)
    Rprintf("%s: rolling took %.3fs\n", __func__, omp_get_wtime()-tic);
}
