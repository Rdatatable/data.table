#include "data.table.h"

/*
 * sort-merge join
 * 
 * join on a single integer column
 * sort LHS and RHS
 * split LHS into equal batches, RHS into corresponding batches by matching upper-lower bounds of LHS batches
 * parallel sort-merge join
 */

// helper to count how many threads were used
int unqN(int *x, int nx) { // x have 0:(nx-1) values
  int ans = 0;
  uint8_t *seen = (uint8_t *)R_alloc(nx, sizeof(uint8_t));
  memset(seen, 0, nx*sizeof(uint8_t));
  for (int i=0; i<nx; ++i) {
    if (!seen[x[i]]++) ans++;
  }
  return ans;
}

// count of duplicated entries
void grpLens(int *starts, int n_starts, int n, /*output*/int *lens) { // #4395
  #pragma omp parallel for schedule(static) num_threads(getDTthreads())
  for (int i=0; i<n_starts-1; ++i)
    lens[i] = starts[i+1]-starts[i];
  lens[n_starts-1] = n - starts[n_starts-1]+1;
  return;
}

// workhorse join that runs in parallel on batches
void smjoin1(int *xs, int *xl, int nxs, int *ys, int *yl, int nys, int *x, int *y, int *starts_y, int *lens_y) {
  int i=0, j=0, is=0, js=0, x_i, y_j;
  while (is<nxs && js<nys) { //Rprintf("============= is=%d\n", is);
    i = xs[is]-1, j = ys[js]-1;
    x_i = x[i], y_j = y[j];
    if (x_i == y_j) { //Rprintf("x[%d]==y[%d]: %d\n", i, j, x_i);
      int j1 = j+1; //Rprintf("x_lens[is]=x_lens[%d]=%d\n",is,xl[is]);
      for (int ii=0; ii<xl[is]; ++ii) { //Rprintf("starts_y[%d]=%d\n", i+ii, j1);
        starts_y[i+ii] = j1;
        lens_y[i+ii] = yl[js];
      }
      is++;
    }
    else if (x_i < y_j) is++;
    else if (x_i > y_j) js++;
  }
  return;
}

// those helpers need to be binary search! last matching, and first matching index
static int max_i_match(int *y, int *y_starts, int ny_starts, int val) {
  int i;
  for (i=0; i<ny_starts; ++i) {
    //Rprintf("max_i_match: y[y_starts[%d]-1] >= val: %d >= %d\n", i, y[y_starts[i]-1], val);
    if (y[y_starts[i]-1]==val) return(i);
    else if (y[y_starts[i]-1]>val) return(i-1);
  }
  return(i-1);
}
static int min_i_match(int *y, int *y_starts, int ny_starts, int val) {
  int i;
  for (i=ny_starts-1; i>=0; --i) {
    //Rprintf("min_i_match: y[y_starts[%d]-1] <= val: %d <= %d\n", i, y[y_starts[i]-1], val);
    if (y[y_starts[i]-1]==val) return(i);
    else if (y[y_starts[i]-1]<val) return(i+1);
  }
  return(i+1);
}
/*
 * 'rolling nearest' binary search
 * used to find 'y' lower and upper bounds for each batch
 * side -1: lower bound; side 1: upper bound
 */
static int rollbs(int *x, int *ix, int nix, int val, int side) {
  bool verbose = false;
  if (verbose) Rprintf("rollbs: side=%d; val=%d\n", side, val);
  if (side < 0) { // lower bound early stopping
    if (x[ix[0]-1] == val) {
      if (verbose) Rprintf("rollbs: min elements %d match\n", val);
      return 0;
    }
    if (x[ix[nix-1]-1] < val) {
      if (verbose) Rprintf("rollbs: max element %d is still smaller than %d\n", x[ix[nix-1]-1], val);
      return -1;
    }
    if (x[ix[0]-1] > val) {
      if (verbose) Rprintf("rollbs: min element %d is bigger than %d\n", x[ix[0]-1], val);
      return 0;
    }
  } else if (side > 0) { // upper bound early stopping
    if (x[ix[nix-1]-1] == val) {
      if (verbose) Rprintf("rollbs: max elements %d match\n", val);
      return nix-1;
    }
    if (x[ix[0]-1] > val) {
      if (verbose) Rprintf("rollbs: min element %d is still bigger than %d\n", x[ix[0]-1], val);
      return -1;
    }
    if (x[ix[nix-1]-1] < val) {
      if (verbose) Rprintf("rollbs: max element %d is smaller than %d\n", x[ix[nix-1]-1], val);
      return nix-1;
    }
  }
  int lower=0, upper=nix, i=0;
  while (lower<=upper) {
    i = lower + (upper-lower)/2;
    int thisx = x[ix[i]-1];
    //if (verbose) Rprintf("rollbs: x[ix[%d]-1]=%d ?? %d\n", i, thisx, val);
    if (thisx==val)
      return(i);
    else if (thisx < val)
      lower = i+1;
    else if (thisx > val)
      upper = i-1;
  }
  if (verbose) Rprintf("rollbs: nomatch: i=%d; this=%d; lower=%d, upper=%d; side=%d\n", i, x[ix[i]-1], lower, upper, side);
  if (side < 0) // anyone to stress test this logic?
    return lower;
  else
    return upper;
}

void smjoinC(int *x, int nx, int *x_starts, int nx_starts,
             int *y, int ny, int *y_starts, int ny_starts,
             int *matchn, int *starts_y, int *lens_y) {

  bool verbose = true;
  bool unq_x = nx_starts==nx, unq_y = ny_starts==ny;
  int *x_lens=0, *y_lens=0;

  double t_lens = omp_get_wtime();
  if (!unq_x || true) { // else true as we dont yet have branches for unq cases
    x_lens = (int *)R_alloc(nx_starts, sizeof(int)); // #4395
    grpLens(x_starts, nx_starts, nx, x_lens);
    //for (int i=0; i<nx_starts; ++i) Rprintf("x_lens[%d]=%d\n", i, x_lens[i]);
  }
  if (!unq_y || true) {
    y_lens = (int *)R_alloc(ny_starts, sizeof(int));
    grpLens(y_starts, ny_starts, ny, y_lens);
  }
  t_lens = omp_get_wtime() - t_lens;

  double t_batch = omp_get_wtime(); // this section needs comprehensive testing for correctness of rollbs!
  int nth = getDTthreads();
  int nBatch = 0; // for a single threaded or small unq count use single batch
  if (nth == 1 || nx_starts < 4) {
    nBatch = 1;
  } else {
    nBatch = nth * 2;
  }
  int *th = (int *)R_alloc(nBatch, sizeof(int)); // report threads used
  size_t batchSize = (nx_starts-1)/nBatch + 1;
  size_t lastBatchSize = nx_starts - (nBatch-1)*batchSize;
  //Rprintf("nBatch=%d; batchSize=%d; lastBatchSize=%d\n", nBatch, batchSize, lastBatchSize);
  int *nxsB = (int *)R_alloc(nBatch, sizeof(int)), **xsB = (int **)R_alloc(nBatch, sizeof(int*)), **xlB = (int **)R_alloc(nBatch, sizeof(int*));
  int *nysB = (int *)R_alloc(nBatch, sizeof(int)), **ysB = (int **)R_alloc(nBatch, sizeof(int*)), **ylB = (int **)R_alloc(nBatch, sizeof(int*));
  bool do_not_set_to_false = false; int y_min2, y_max2;
  //#pragma omp parallel for schedule(dynamic) num_threads(nth)
  for (int b=0; b<nBatch-1; ++b) {
    nxsB[b] = batchSize; xsB[b] = x_starts + b*batchSize; xlB[b] = x_lens + b*batchSize;
    if (do_not_set_to_false) y_min2 = min_i_match(y, y_starts, ny_starts, x[xsB[b][0]-1]);
    if (do_not_set_to_false) y_max2 = max_i_match(y, y_starts, ny_starts, x[xsB[b][nxsB[b]-1]-1]);
    //Rprintf("nxsB[b]=%d; y_min=%d; y_max=%d\n", nxsB[b], y_min, y_max);
    int y_min = rollbs(y, y_starts, ny_starts, x[xsB[b][0]-1], -1);
    int y_max = rollbs(y, y_starts, ny_starts, x[xsB[b][nxsB[b]-1]-1], 1);
    //Rprintf("nxsB[b]=%d; y_mn2=%d; y_mx2=%d\n", nxsB[b], y_min2, y_max2);
    if (do_not_set_to_false && y_min!=y_min2) error("y_min different: %d vs %d", y_min2, y_min);
    if (do_not_set_to_false && y_max!=y_max2) error("y_max different: %d vs %d", y_max2, y_max);
    nysB[b] = y_max - y_min + 1; ysB[b] = y_starts + y_min; ylB[b] = y_lens + y_min;
  }
  int b = nBatch-1;
  nxsB[b] = lastBatchSize; xsB[b] = x_starts + nx_starts-lastBatchSize; xlB[b] = x_lens + nx_starts-lastBatchSize;
  if (do_not_set_to_false) y_min2 = min_i_match(y, y_starts, ny_starts, x[xsB[b][0]-1]);
  if (do_not_set_to_false) y_max2 = max_i_match(y, y_starts, ny_starts, x[xsB[b][nxsB[b]-1]-1]);
  //Rprintf("nxsB[b]=%d; y_min=%d; y_max=%d\n", nxsB[b], y_min, y_max);
  int y_min = rollbs(y, y_starts, ny_starts, x[xsB[b][0]-1], -1);
  int y_max = rollbs(y, y_starts, ny_starts, x[xsB[b][nxsB[b]-1]-1], 1);
  //Rprintf("nxsB[b]=%d; y_mn2=%d; y_mx2=%d\n", nxsB[b], y_min2, y_max2);
  if (do_not_set_to_false && y_min!=y_min2) error("y_min different: %d vs %d", y_min2, y_min);
  if (do_not_set_to_false && y_max!=y_max2) error("y_max different: %d vs %d", y_max2, y_max);
  nysB[b] = y_max - y_min + 1; ysB[b] = y_starts + y_min; ylB[b] = y_lens + y_min;
  //for (int b=0; b<nBatch; ++b) Rprintf("b=%d:\n  nx=%d; xs[0]=%d; xl[0]=%d; x[xs[0]-1]=%d\n  ny=%d; ys[0]=%d; yl[0]=%d; y[ys[0]-1]=%d\n", b, nxsB[b], xsB[b][0], xlB[b][0], x[xsB[b][0]-1], nysB[b], ysB[b][0], ylB[b][0], y[ysB[b][0]-1]);
  t_batch = omp_get_wtime() - t_batch;

  double t_join = omp_get_wtime();
  #pragma omp parallel for schedule(dynamic) num_threads(nth)
  for (int b=0; b<nBatch; ++b) {
    smjoin1(
      xsB[b], xlB[b], nxsB[b], ysB[b], ylB[b], nysB[b], // batch specific input: shifted pointers and their sizes
      x, y,                                             // common input
      starts_y, lens_y                                  // common output
    );
    th[b] = omp_get_thread_num();
  }
  t_join = omp_get_wtime() - t_join;
  //for (int b=0; b<nBatch; ++b) Rprintf("th[%d]=%d\n", th[b]);

  if (verbose)
    Rprintf("smjoinC: lens %.3fs; batch %.3fs; join %.3fs; used %dth in %d batch\n", t_lens, t_batch, t_join, unqN(th, nBatch), nBatch);
  matchn[0] = nx;
}

void sortInt(int *x, int nx, int *idx, int *ans) {
  #pragma omp parallel for schedule(static) num_threads(getDTthreads())
  for (int i=0; i<nx; ++i)
    ans[i] = x[idx[i]-1];
  return;
}

SEXP joinOut(int n, int *starts, int *lens, bool x_ord, SEXP out_starts, SEXP out_lens, int *xoop) {
  SEXP ans = PROTECT(allocVector(VECSXP, 2)), ansnames;
  setAttrib(ans, R_NamesSymbol, ansnames=allocVector(STRSXP, 2));
  SET_STRING_ELT(ansnames, 0, char_starts);
  SET_STRING_ELT(ansnames, 1, char_lens);
  if (x_ord) {
    SET_VECTOR_ELT(ans, 0, out_starts);
    SET_VECTOR_ELT(ans, 1, out_lens);
  } else {
    SET_VECTOR_ELT(ans, 0, allocVector(INTSXP, n));
    SET_VECTOR_ELT(ans, 1, allocVector(INTSXP, n));
    sortInt(starts, n, xoop, INTEGER(VECTOR_ELT(ans, 0)));
    sortInt(lens, n, xoop, INTEGER(VECTOR_ELT(ans, 1)));
  }
  UNPROTECT(1);
  return ans;
}

SEXP smjoinR(SEXP x, SEXP y, SEXP x_idx, SEXP y_idx) {

  bool verbose = true;
  if (!isInteger(x) || !isInteger(y))
    error("must be integer");
  int protecti=0, nx = LENGTH(x), ny = LENGTH(y);

  double t_index = omp_get_wtime();
  if (isNull(x_idx)) {
    x_idx = PROTECT(forder(x, R_NilValue, ScalarLogical(TRUE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE))); protecti++;
  }
  if (isNull(y_idx)) {
    y_idx = PROTECT(forder(y, R_NilValue, ScalarLogical(TRUE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE))); protecti++;
  }
  SEXP x_starts = getAttrib(x_idx, sym_starts);
  SEXP y_starts = getAttrib(y_idx, sym_starts);
  if (isNull(x_starts) || isNull(y_starts))
    error("Indices provided to smjoin must carry 'starts' attribute");
  t_index = omp_get_wtime() - t_index;

  double t_sort = omp_get_wtime();
  bool x_ord = !LENGTH(x_idx), y_ord = !LENGTH(y_idx);
  SEXP xoo = R_NilValue; // order of an order of x
  int *xp, *yp, *xoop=0;
  if (!x_ord) {
    xp = (int *)R_alloc(nx, sizeof(int));
    sortInt(INTEGER(x), nx, INTEGER(x_idx), xp);
    xoo = PROTECT(forder(x_idx, R_NilValue, /*retGrp=*/ScalarLogical(FALSE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE))); protecti++;
    xoop = INTEGER(xoo);
  } else xp = INTEGER(x);
  if (!y_ord) {
    yp = (int *)R_alloc(ny, sizeof(int));
    sortInt(INTEGER(y), ny, INTEGER(y_idx), yp);
  } else yp = INTEGER(y);
  t_sort = omp_get_wtime() - t_sort;

  double t_alloc = omp_get_wtime();
  SEXP out_starts = R_NilValue, out_lens = R_NilValue;
  int *starts=0, *lens=0;
  if (x_ord) { // we dont need to reorder results so can save one allocation
    out_starts = PROTECT(allocVector(INTSXP, nx)); protecti++;
    out_lens = PROTECT(allocVector(INTSXP, nx)); protecti++;
    starts = INTEGER(out_starts);
    lens = INTEGER(out_lens);
  } else {
    starts = (int *)R_alloc(nx, sizeof(int));
    lens = (int *)R_alloc(nx, sizeof(int));
  }
  #pragma omp parallel for schedule(static) num_threads(getDTthreads())
  for (int i=0; i<nx; ++i)
    starts[i] = lens[i] = NA_INTEGER;
  t_alloc = omp_get_wtime() - t_alloc;

  double t_smjoin = omp_get_wtime();
  int matchn = 0; // not yet used
  smjoinC(
    xp, nx, INTEGER(x_starts), LENGTH(x_starts),
    yp, ny, INTEGER(y_starts), LENGTH(y_starts),
    &matchn, starts, lens
  );
  t_smjoin = omp_get_wtime() - t_smjoin;

  double t_ans = omp_get_wtime();
  SEXP ans = joinOut(matchn, starts, lens, x_ord, out_starts, out_lens, xoop); // usually also sorts inside
  t_ans = omp_get_wtime() - t_ans;

  if (verbose)
    Rprintf("smjoinR: index %.3fs; sort %.3fs; alloc %.3fs; smjoinC %.3fs; ans %.3fs\n", t_index, t_sort, t_alloc, t_smjoin, t_ans);
  UNPROTECT(protecti);
  return ans;
}
