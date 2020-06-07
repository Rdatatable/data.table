#include "data.table.h"

/*
 * sort-merge join
 *
 * join on a single integer column
 * sort LHS and RHS
 * split LHS into equal batches, RHS into corresponding batches by matching upper-lower bounds of LHS batches
 * parallel sort-merge join
 *
 * for a maximum speed collecting of following statistics can be disabled by undefine SMERGE_ALL
 * y_len1/y_lens1/allLen1:       signals if there where multiple matches in RHS of join, ['s _x_ table
 * x_len1/x_lens1/lhsLen1:       signals if multiple matches in LHS of join, ['s _i_ table
 * xy_len1/xy_lens1/xlLen1:      signals if "many to many" matches between LHS and RHS
 * cnt/nmatch/n_match/n_matchr:  count of matches, taking multiple matches into account, uint64_t
 */
#define SMERGE_ALL

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
void smerge1(int *xs, int *xl, int nxs, int *ys, int *yl, int nys,
             int *x, bool unq_x, int *y, bool unq_y,
             int *starts, int *lens,
             uint64_t *nmatch, bool *xlens1, bool *ylens1, bool *xylens1) {
  int i=0, j=0, is=0, js=0, x_i, y_j;
  uint64_t cnt=0;
  bool xlen1 = true, ylen1 = true, xylen1 = true;
  if (unq_x && unq_y) { //Rprintf("smerge1: unq_x && unq_y\n");
    while (i<nxs && j<nys) {
      x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        starts[i] = j+1;
        lens[i] = 1;
        i++;
#ifdef SMERGE_ALL
        cnt++;
#endif
      } else if (x_i < y_j) {
        i++;
      } else if (x_i > y_j) {
        j++;
      }
    }
  } else if (unq_x) { //Rprintf("smerge1: unq_x && !unq_y\n");
    while (i<nxs && js<nys) {
      j = ys[js]-1;
      x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        starts[i] = j+1;
        int yl1 = yl[js];
        lens[i] = yl1;
        i++;
#ifdef SMERGE_ALL
        if (ylen1 && yl1>1)
          ylen1 = false;
        cnt += (uint64_t)yl1;
#endif
      } else if (x_i < y_j) {
        i++;
      } else if (x_i > y_j) {
        js++;
      }
    }
  } else if (unq_y) { //Rprintf("smerge1: !unq_x && unq_y\n");
    while (is<nxs && j<nys) {
      i = xs[is]-1;
      x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        int j1 = j+1;
        int xl1 = xl[is];
        for (int ii=0; ii<xl1; ++ii) {
          starts[i+ii] = j1;
          lens[i+ii] = 1;
        }
        is++;
#ifdef SMERGE_ALL
        if (xlen1 && xl1>1)
          xlen1 = false;
        cnt += (uint64_t)xl1;
#endif
      } else if (x_i < y_j) {
        is++;
      } else if (x_i > y_j) {
        j++;
      }
    }
  } else { //Rprintf("smerge1: !unq_x && !unq_y\n");
    while (is<nxs && js<nys) { //Rprintf("============= is=%d\n", is);
      i = xs[is]-1, j = ys[js]-1;
      x_i = x[i], y_j = y[j];
      if (x_i == y_j) { //Rprintf("x[%d]==y[%d]: %d\n", i, j, x_i);
        int j1 = j+1, yl1 = yl[js]; //Rprintf("x_lens[is]=x_lens[%d]=%d\n",is,xl[is]);
        int xl1 = xl[is];
        for (int ii=0; ii<xl1; ++ii) { //Rprintf("starts[%d]=%d\n", i+ii, j1);
          starts[i+ii] = j1;
          lens[i+ii] = yl1;
        }
        is++;
#ifdef SMERGE_ALL
        if (xlen1 && xl1>1)
          xlen1 = false;
        if (ylen1 && yl1>1)
          ylen1 = false;
        if (xylen1 && xl1>1 && yl1>1)
          xylen1 = false;
        cnt += (uint64_t)xl1 * (uint64_t)yl1;
#endif
      } else if (x_i < y_j) {
        is++;
      } else if (x_i > y_j) {
        js++;
      }
    }
  }
  xlens1[0] = xlen1; ylens1[0] = ylen1; xylens1[0] = xylen1; nmatch[0] = cnt;
  return;
}

// those two helpers function may be removed once we will confirm rollbs works as expected, they find last matching, and first matching index
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
  bool verbose = false; // develd debug only
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

void smergeC(int *x, int nx, int *x_starts, int nx_starts,
             int *y, int ny, int *y_starts, int ny_starts,
             int *starts, int *lens,
             uint64_t *n_match, int *x_lens1, int *y_lens1, int *xy_lens1,
             int verbose) {

  bool unq_x = nx_starts==nx, unq_y = ny_starts==ny;
  int *x_lens = 0, *y_lens = 0;
  double t = 0;

  t = omp_get_wtime();
  if (!unq_x) { // else true as we dont yet have branches for unq cases
    x_lens = (int *)R_alloc(nx_starts, sizeof(int)); // #4395
    grpLens(x_starts, nx_starts, nx, x_lens);
    //for (int i=0; i<nx_starts; ++i) Rprintf("x_lens[%d]=%d\n", i, x_lens[i]);
  }
  if (!unq_y) {
    y_lens = (int *)R_alloc(ny_starts, sizeof(int));
    grpLens(y_starts, ny_starts, ny, y_lens);
  }
  if (verbose>0)
    Rprintf("smergeC: grpLens took %.3fs\n", omp_get_wtime() - t);

  t = omp_get_wtime(); // this section needs comprehensive testing for correctness of rollbs! whole section of breaking batches should probably be isolated into own function
  int nth = getDTthreads();
  int nBatch = 0; // for a single threaded or small unq count use single batch
  if (nth == 1 || nx_starts < 4) { // this is 4 only for testing, will be probably 1024
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
  if (verbose>0)
    Rprintf("smergeC: preparing %d batches took %.3fs\n", nBatch, omp_get_wtime() - t);

  t = omp_get_wtime();
  bool xlens1 = true, ylens1 = true, xylens1 = true;
  uint64_t nmatch = 0;
  #pragma omp parallel for schedule(dynamic) reduction(&&:xlens1,ylens1,xylens1) reduction(+:nmatch) num_threads(nth)
  for (int b=0; b<nBatch; ++b) {
    smerge1(
      xsB[b], xlB[b], nxsB[b], ysB[b], ylB[b], nysB[b], // batch specific input: shifted pointers and their sizes
      x, unq_x, y, unq_y,                               // common input
      starts, lens,                                     // common output
      &nmatch, &xlens1, &ylens1, &xylens1               // common reduction
    );
    th[b] = omp_get_thread_num();
  }
  x_lens1[0] = (int)xlens1; y_lens1[0] = (int)ylens1; xy_lens1[0] = (int)xylens1; n_match[0] = nmatch;
  //for (int b=0; b<nBatch; ++b) Rprintf("th[%d]=%d\n", b, th[b]); // threads utilization for each batch
  if (verbose>0)
    Rprintf("smergeC: %d calls to smerge1 using %d/%d threads took %.3fs\n", nBatch, unqN(th, nBatch), nth, omp_get_wtime() - t);

  return;
}

void sortInt(int *x, int nx, int *idx, int *ans) {
  #pragma omp parallel for schedule(static) num_threads(getDTthreads())
  for (int i=0; i<nx; ++i)
    ans[i] = x[idx[i]-1];
  return;
}

// this produce output consistent to bmerge, note that bmerge i,x is smerge x,y
SEXP joinOut(int n, int *starts, int *lens, bool x_ord,
             SEXP out_starts, SEXP out_lens,
             SEXP x_idx, SEXP y_idx,
             SEXP n_match, SEXP x_lens1, SEXP y_lens1, SEXP xy_lens1) {
  SEXP ans = PROTECT(allocVector(VECSXP, 10)), ansnames;
  setAttrib(ans, R_NamesSymbol, ansnames=allocVector(STRSXP, 10));
  SET_STRING_ELT(ansnames, 0, char_starts);
  SET_STRING_ELT(ansnames, 1, char_lens);
  SET_STRING_ELT(ansnames, 2, char_indices); SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, 0)); // const for equi join
  SET_STRING_ELT(ansnames, 3, char_allLen1); SET_VECTOR_ELT(ans, 3, y_lens1);
  SET_STRING_ELT(ansnames, 4, char_allGrp1); SET_VECTOR_ELT(ans, 4, ScalarLogical(true));    // const for equi join
  SET_STRING_ELT(ansnames, 5, char_xo);      SET_VECTOR_ELT(ans, 5, y_idx);
  SET_STRING_ELT(ansnames, 6, char_io);      SET_VECTOR_ELT(ans, 6, x_idx);
  SET_STRING_ELT(ansnames, 7, char_lhsLen1); SET_VECTOR_ELT(ans, 7, x_lens1);
  SET_STRING_ELT(ansnames, 8, char_xyLen1);  SET_VECTOR_ELT(ans, 8, xy_lens1);
  SET_STRING_ELT(ansnames, 9, char_nMatch);  SET_VECTOR_ELT(ans, 9, n_match);
  if (x_ord) {
    SET_VECTOR_ELT(ans, 0, out_starts);
    SET_VECTOR_ELT(ans, 1, out_lens);
  } else {
    SET_VECTOR_ELT(ans, 0, allocVector(INTSXP, n));
    SET_VECTOR_ELT(ans, 1, allocVector(INTSXP, n));
    SEXP xoo = PROTECT(forder(x_idx, R_NilValue, /*retGrp=*/ScalarLogical(FALSE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE))); // verbose=verbose-2L after #4533
    sortInt(starts, n, INTEGER(xoo), INTEGER(VECTOR_ELT(ans, 0)));
    sortInt(lens, n, INTEGER(xoo), INTEGER(VECTOR_ELT(ans, 1)));
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return ans;
}

SEXP smergeR(SEXP x, SEXP y, SEXP x_idx, SEXP y_idx) {

  const int verbose = GetVerbose()*2; // remove *2 after #4491
  if (!isInteger(x) || !isInteger(y))
    error("'x' and 'y' must be integer");
  int protecti = 0, nx = LENGTH(x), ny = LENGTH(y);
  double t = 0;

  t = omp_get_wtime();
  bool do_x_idx = isNull(x_idx), do_y_idx = isNull(y_idx);
  if (do_x_idx) {
    x_idx = PROTECT(forder(x, R_NilValue, ScalarLogical(TRUE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE))); protecti++; // verbose=verbose-2L after #4533
  }
  if (do_y_idx) {
    y_idx = PROTECT(forder(y, R_NilValue, ScalarLogical(TRUE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE))); protecti++; // verbose=verbose-2L after #4533
  }
  if (!isInteger(x_idx) || !isInteger(y_idx))
    error("'x.idx' and 'y.idx' must be integer");
  SEXP x_starts = getAttrib(x_idx, sym_starts); SEXP y_starts = getAttrib(y_idx, sym_starts);
  if (isNull(x_starts) || isNull(y_starts))
    error("Indices provided to smerge must carry 'starts' attribute");
  if (verbose>0)
    Rprintf("smergeR: index took %.3fs\n", omp_get_wtime() - t);

  t = omp_get_wtime();
  bool x_ord = !LENGTH(x_idx), y_ord = !LENGTH(y_idx);
  int *xp, *yp;
  if (!x_ord) {
    xp = (int *)R_alloc(nx, sizeof(int));
    sortInt(INTEGER(x), nx, INTEGER(x_idx), xp);
  } else {
    xp = INTEGER(x);
  }
  if (!y_ord) {
    yp = (int *)R_alloc(ny, sizeof(int));
    sortInt(INTEGER(y), ny, INTEGER(y_idx), yp);
  } else {
    yp = INTEGER(y);
  }
  if (verbose>0)
    Rprintf("smergeR: sort took %.3fs\n", omp_get_wtime() - t);

  t = omp_get_wtime();
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
  for (int i=0; i<nx; ++i) {
    starts[i] = NA_INTEGER;
    lens[i] = 1;
  }
  uint64_t n_match = 0;
  SEXP x_lens1 = PROTECT(allocVector(LGLSXP, 1)); protecti++;
  SEXP y_lens1 = PROTECT(allocVector(LGLSXP, 1)); protecti++;
  SEXP xy_lens1 = PROTECT(allocVector(LGLSXP, 1)); protecti++;
  SEXP n_matchr = PROTECT(allocVector(REALSXP, 1)); protecti++;
  if (verbose>0)
    Rprintf("smergeR: alloc of size %d took %.3fs\n", nx, omp_get_wtime() - t);

  t = omp_get_wtime();
  smergeC(
    xp, nx, INTEGER(x_starts), LENGTH(x_starts),
    yp, ny, INTEGER(y_starts), LENGTH(y_starts),
    starts, lens,
    &n_match, INTEGER(x_lens1), INTEGER(y_lens1), INTEGER(xy_lens1),
    verbose-1
  );
  if (n_match > DBL_MAX) {
    REAL(n_matchr)[0] = NA_REAL;
    warning("count of matches exceeds DBL_MAX, returning NA in 'nMatch' field");
  } else {
    REAL(n_matchr)[0] = (double)n_match;
  }
  if (verbose>0)
    Rprintf("smergeR: smergeC of %d x %d => %"PRIu64" took %.3fs\n", nx, ny, n_match, omp_get_wtime() - t);

  t = omp_get_wtime();
  SEXP ans = joinOut(nx, starts, lens, x_ord, out_starts, out_lens, x_idx, y_idx, n_matchr, x_lens1, y_lens1, xy_lens1);
  if (verbose>0)
    Rprintf("smergeR: joinOut (unsort) took %.3fs\n", omp_get_wtime() - t);

  UNPROTECT(protecti);
  return ans;
}
