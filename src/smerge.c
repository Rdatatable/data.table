#include "data.table.h"

/*
 * sort-merge join
 *
 * join on a single integer column
 * sort LHS and RHS
 * split LHS into equal batches, RHS into corresponding batches by matching upper-lower bounds of LHS batches
 * parallel sort-merge join
 *
 * for a maximum speed collecting of following statistics can be disabled by undefine SMERGE_STATS
 * y_len1/y_lens1/allLen1:       signals if there where multiple matches in RHS of join, ['s _x_ table
 * x_len1/x_lens1/lhsLen1:       signals if multiple matches in LHS of join, ['s _i_ table
 * xy_len1/xy_lens1/xlLen1:      signals if "many to many" matches between LHS and RHS
 * cnt/nmatch/n_match/n_matchr:  count of matches, taking multiple matches into account, uint64_t
 */
#define SMERGE_STATS

// shifted pointers so each batch knows where to start and end
struct smBatch {
  int *xs; // x's starts: indices on which unique values can be found
  int *xl; // x's lens:   counts of each unique value
  int nxs; // x's starts length
  int *ys; // same for y
  int *yl;
  int nys;
};

// workhorse join that runs in parallel on batches
static void smerge(struct smBatch *batch,
                   int *x, bool unq_x, int *y, bool unq_y,
                   int *starts, int *lens,
                   uint64_t *nmatch, bool *xlens1, bool *ylens1, bool *xylens1) {
  int *xs=batch->xs, *xl=batch->xl, nxs=batch->nxs; // unpack smBatch struct
  int *ys=batch->ys, *yl=batch->yl, nys=batch->nys;
  int i=0, j=0, is=0, js=0, x_i, y_j;
  uint64_t cnt=0;
  bool xlen1 = true, ylen1 = true, xylen1 = true;
  if (unq_x && unq_y) { //Rprintf("smerge: unq_x && unq_y\n");
    while (i<nxs && j<nys) {
      x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        starts[i] = j+1;
        lens[i] = 1;
        i++;
#ifdef SMERGE_STATS
        cnt++;
#endif
      } else if (x_i < y_j) {
        i++;
      } else if (x_i > y_j) {
        j++;
      }
    }
  } else if (unq_x) { //Rprintf("smerge: unq_x && !unq_y\n");
    while (i<nxs && js<nys) {
      j = ys[js]-1;
      x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        starts[i] = j+1;
        int yl1 = yl[js];
        lens[i] = yl1;
        i++;
#ifdef SMERGE_STATS
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
  } else if (unq_y) { //Rprintf("smerge: !unq_x && unq_y\n");
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
#ifdef SMERGE_STATS
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
  } else { //Rprintf("smerge: !unq_x && !unq_y\n");
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
#ifdef SMERGE_STATS
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

// those two helpers needs to be removed once we will confirm rollbs works as expected, they find "first/last within" index
static int max_i_match(int *y, int *y_starts, int ny_starts, int val) {
  int i;
  for (i=0; i<ny_starts; ++i) { //Rprintf("max_i_match: y[y_starts[%d]-1] >= val: %d >= %d\n", i, y[y_starts[i]-1], val);
    if (y[y_starts[i]-1]==val) return(i);
    else if (y[y_starts[i]-1]>val) return(i-1);
  }
  return(i-1);
}
static int min_i_match(int *y, int *y_starts, int ny_starts, int val) {
  int i;
  for (i=ny_starts-1; i>=0; --i) { //Rprintf("min_i_match: y[y_starts[%d]-1] <= val: %d <= %d\n", i, y[y_starts[i]-1], val);
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
  int verbose = 0; // devel debug only
  if (verbose>0) Rprintf("rollbs: side=%d; val=%d\n", side, val);
  if (side < 0) { // lower bound early stopping
    if (x[ix[0]-1] == val) {
      if (verbose>0) Rprintf("rollbs: min elements %d match\n", val);
      return 0;
    }
    if (x[ix[nix-1]-1] < val) {
      if (verbose>0) Rprintf("rollbs: max element %d is still smaller than %d\n", x[ix[nix-1]-1], val);
      return -1;
    }
    if (x[ix[0]-1] > val) {
      if (verbose>0) Rprintf("rollbs: min element %d is bigger than %d\n", x[ix[0]-1], val);
      return 0;
    }
  } else if (side > 0) { // upper bound early stopping
    if (x[ix[nix-1]-1] == val) {
      if (verbose>0) Rprintf("rollbs: max elements %d match\n", val);
      return nix-1;
    }
    if (x[ix[0]-1] > val) {
      if (verbose>0) Rprintf("rollbs: min element %d is still bigger than %d\n", x[ix[0]-1], val);
      return -1;
    }
    if (x[ix[nix-1]-1] < val) {
      if (verbose>0) Rprintf("rollbs: max element %d is smaller than %d\n", x[ix[nix-1]-1], val);
      return nix-1;
    }
  }
  int lower=0, upper=nix, i=0;
  while (lower<=upper) {
    i = lower + (upper-lower)/2;
    int thisx = x[ix[i]-1];
    //if (verbose>0) Rprintf("rollbs: x[ix[%d]-1]=%d ?? %d\n", i, thisx, val);
    if (thisx==val)
      return(i);
    else if (thisx < val)
      lower = i+1;
    else if (thisx > val)
      upper = i-1;
  }
  if (verbose>0) Rprintf("rollbs: nomatch: i=%d; this=%d; lower=%d, upper=%d; side=%d\n", i, x[ix[i]-1], lower, upper, side);
  if (side < 0) // anyone to stress test this logic?
    return lower;
  else
    return upper;
}

// cuts x_starts into equal batches and binary search corresponding y_starts ranges
static struct smBatch *batching(int nBatch,
                                int *x, int nx, int *x_starts, int nx_starts, int *x_lens,
                                int *y, int ny, int *y_starts, int ny_starts, int *y_lens,
                                int verbose) {
  if (verbose>0)
    Rprintf("batching: %d batches of sorted x y: x[1]<=y[1] && x[nx]>=y[ny]:\n", nBatch);
  struct smBatch *B = (struct smBatch *)R_alloc(nBatch, sizeof(struct smBatch));
  size_t batchSize = (nx_starts-1)/nBatch + 1;
  size_t lastBatchSize = nx_starts - (nBatch-1)*batchSize;
  bool extra_validate = true; // validates against very slow m[in|ax]_i_match // #DEV
  for (int b=0; b<nBatch; ++b) {
    if (b<nBatch-1) {
      B[b].xs = x_starts + b*batchSize; B[b].xl = x_lens + b*batchSize; B[b].nxs = batchSize;
    } else { // last batch
      B[b].xs = x_starts + nx_starts-lastBatchSize; B[b].xl = x_lens + nx_starts-lastBatchSize; B[b].nxs = lastBatchSize;
    }
    int x_i_min = B[b].xs[0];
    int x_i_max = B[b].xs[B[b].nxs-1];
    int y_i_min = rollbs(y, y_starts, ny_starts, x[x_i_min-1], -1);
    int y_i_max = rollbs(y, y_starts, ny_starts, x[x_i_max-1], 1);
    if (extra_validate) {
      int y_i_min2 = min_i_match(y, y_starts, ny_starts, x[x_i_min-1]);
      int y_i_max2 = max_i_match(y, y_starts, ny_starts, x[x_i_max-1]);
      if (y_i_min!=y_i_min2)
        error("y lower bound different: %d vs %d", y_i_min2, y_i_min);
      if (y_i_max!=y_i_max2)
        error("y upper bound different: %d vs %d", y_i_max2, y_i_max);
    }
    B[b].ys = y_starts + y_i_min; B[b].yl = y_lens + y_i_min; B[b].nys = y_i_max - y_i_min + 1;
  }
  if (verbose>0) { // print smBatch objects, 1-indexed! x y sorted! for debugging and verbose
    for (int b=0; b<nBatch; ++b) {
      int x_i_min = B[b].xs[0], y_i_min = B[b].ys[0];
      int x_i_max = B[b].xs[B[b].nxs-1], y_i_max = B[b].ys[B[b].nys-1];
      Rprintf("#### batch[%d]: unq n: x=%d, y=%d\n", b+1, B[b].nxs, B[b].nys);
      Rprintf("## lower: x[%d]: %d <= %d :y[%d]\n", x_i_min, x[x_i_min-1], y[y_i_min-1], y_i_min);
      Rprintf("## upper: x[%d]: %d >= %d :y[%d]\n", x_i_max, x[x_i_max-1], y[y_i_max-1], y_i_max);
    }
  }
  return B;
}

// helper to count how many threads were used
static int unqNth(int *x, int nx) { // x have 0:(nx-1) values
  int ans = 0;
  uint8_t *seen = (uint8_t *)R_alloc(nx, sizeof(uint8_t));
  memset(seen, 0, nx*sizeof(uint8_t));
  for (int i=0; i<nx; ++i) {
    if (!seen[x[i]]++) ans++;
  }
  return ans;
}

// helper for verbose messages to print what was actually computed
static const char *verboseDone(bool x, bool y, const char *not_xy, const char *not_x, const char *not_y, const char *xy) {
  if (!x && !y)
    return(not_xy);
  else if (!x && y)
    return(not_x);
  else if (x && !y)
    return(not_y);
  else
    return(xy);
}

// count of duplicated entries
void grpLens(int *starts, int n_starts, int n, /*output*/int *lens) { // #4395
#pragma omp parallel for schedule(static) num_threads(getDTthreads())
  for (int i=0; i<n_starts-1; ++i)
    lens[i] = starts[i+1]-starts[i];
  lens[n_starts-1] = n - starts[n_starts-1]+1;
  return;
}

// pure C smerge: takes already sorted input, compute grpLens, prepare batches, smerge in parallel
void smergeC(int *x, int nx, int *x_starts, int nx_starts,
             int *y, int ny, int *y_starts, int ny_starts,
             int *starts, int *lens,
             uint64_t *n_match, int *x_lens1, int *y_lens1, int *xy_lens1,
             int verbose) {

  double t = omp_get_wtime();
  int *x_lens = 0, *y_lens = 0;
  bool unq_x = nx_starts==nx, unq_y = ny_starts==ny;
  if (!unq_x) {
    x_lens = (int *)R_alloc(nx_starts, sizeof(int)); // remove after #4395
    grpLens(x_starts, nx_starts, nx, x_lens);
  }
  if (!unq_y) {
    y_lens = (int *)R_alloc(ny_starts, sizeof(int));
    grpLens(y_starts, ny_starts, ny, y_lens);
  }
  if (verbose>0)
    Rprintf("smergeC: grpLens %s took %.3fs\n", verboseDone(!unq_x, !unq_y, "(already unique)", "(y)", "(x)", "(x, y)"), omp_get_wtime() - t);

  t = omp_get_wtime(); // this section needs comprehensive testing for correctness of rollbs! whole section of breaking batches should probably be isolated into own function
  int nth = getDTthreads();
  int nBatch = 0; // for a single threaded or small unq count use single batch
  if (nth == 1 || nx_starts < 4) { // this is 4 only for testing, will be probably 1024 // #DEV
    nBatch = 1;
  } else {
    nBatch = nth * 2;
  }
  int *th = (int *)R_alloc(nBatch, sizeof(int)); // report threads used
  struct smBatch *B = batching(nBatch, x, nx, x_starts, nx_starts, x_lens, y, ny, y_starts, ny_starts, y_lens, verbose-1);
  if (verbose>0)
    Rprintf("smergeC: preparing %d batches took %.3fs\n", nBatch, omp_get_wtime() - t);

  t = omp_get_wtime();
  bool xlens1 = true, ylens1 = true, xylens1 = true;
  uint64_t nmatch = 0;
  #pragma omp parallel for schedule(dynamic) reduction(&&:xlens1,ylens1,xylens1) reduction(+:nmatch) num_threads(nth)
  for (int b=0; b<nBatch; ++b) {
    smerge(
      &B[b],                              // batch input
      x, unq_x, y, unq_y,                 // common input
      starts, lens,                       // common output
      &nmatch, &xlens1, &ylens1, &xylens1 // common reduction output
    );
    th[b] = omp_get_thread_num();
  }
  x_lens1[0] = (int)xlens1; y_lens1[0] = (int)ylens1; xy_lens1[0] = (int)xylens1; n_match[0] = nmatch;
  if (verbose>0)
    Rprintf("smergeC: %d calls to smerge using %d/%d threads took %.3fs\n", nBatch, unqNth(th, nBatch), nth, omp_get_wtime() - t); // all threads may not always be used bc schedule(dynamic)

  return;
}

void sortInt(int *x, int nx, int *idx, int *ans) {
  #pragma omp parallel for schedule(static) num_threads(getDTthreads())
  for (int i=0; i<nx; ++i)
    ans[i] = x[idx[i]-1];
  return;
}

// wrap results into list, bmerge=true this produce bmerge consistent output, note that bmerge i,x is smerge x,y
SEXP outSmergeR(int n, int *starts, int *lens, bool x_ord,
                SEXP out_starts, SEXP out_lens, // used only when x was sorted, saves one allocation
                SEXP x_idx, SEXP y_idx,
                SEXP n_match, SEXP x_lens1, SEXP y_lens1, SEXP xy_lens1,
                bool bmerge) {
  int out_len = bmerge ? 6 : 10;
  SEXP ans = PROTECT(allocVector(VECSXP, out_len)), ansnames;
  setAttrib(ans, R_NamesSymbol, ansnames=allocVector(STRSXP, out_len));
  SET_STRING_ELT(ansnames, 0, char_starts);
  SET_STRING_ELT(ansnames, 1, char_lens);
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
  SET_STRING_ELT(ansnames, 2, char_indices); SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, 0)); // const for equi join
  SET_STRING_ELT(ansnames, 3, char_allLen1); SET_VECTOR_ELT(ans, 3, y_lens1);
  SET_STRING_ELT(ansnames, 4, char_allGrp1); SET_VECTOR_ELT(ans, 4, ScalarLogical(true));    // const for equi join
  if (bmerge) {
    y_idx = shallow_duplicate(y_idx); // possibly improve after #4467
    setAttrib(y_idx, sym_starts, R_NilValue);
    setAttrib(y_idx, sym_maxgrpn, R_NilValue);
    //setAttrib(y_idx, sym_anyna, R_NilValue); // enable after #4386
    //setAttrib(y_idx, sym_anyinfnan, R_NilValue);
    //setAttrib(y_idx, sym_anynotascii, R_NilValue);
    //setAttrib(y_idx, sym_anynotutf8, R_NilValue);
  }
  SET_STRING_ELT(ansnames, 5, char_xo); SET_VECTOR_ELT(ans, 5, y_idx);
  if (!bmerge) {
    SET_STRING_ELT(ansnames, 6, char_io);      SET_VECTOR_ELT(ans, 6, x_idx);
    SET_STRING_ELT(ansnames, 7, char_lhsLen1); SET_VECTOR_ELT(ans, 7, x_lens1);
    SET_STRING_ELT(ansnames, 8, char_xyLen1);  SET_VECTOR_ELT(ans, 8, xy_lens1);
    SET_STRING_ELT(ansnames, 9, char_nMatch);  SET_VECTOR_ELT(ans, 9, n_match);
  }
  UNPROTECT(1);
  return ans;
}

// main interface from R
SEXP smergeR(SEXP x, SEXP y, SEXP x_idx, SEXP y_idx, SEXP out_bmerge) {

  double t_total = omp_get_wtime();
  const int verbose = GetVerbose()*3; // remove *3 after #4491
  if (!isInteger(x) || !isInteger(y))
    error("'x' and 'y' must be integer");
  if (!IS_TRUE_OR_FALSE(out_bmerge))
    error("'out.bmerge' must be TRUE or FALSE");
  int protecti = 0, nx = LENGTH(x), ny = LENGTH(y);

  double t = omp_get_wtime();
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
    Rprintf("smergeR: index %s took %.3fs\n", verboseDone(do_x_idx, do_y_idx, "(already indexed)", "(y)", "(x)", "(x, y)"), omp_get_wtime() - t);

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
    Rprintf("smergeR: sort %s took %.3fs\n", verboseDone(!x_ord, !y_ord, "(already sorted)", "(y)", "(x)", "(x, y)"), omp_get_wtime() - t);

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
  if (n_match > (uint64_t)DBL_MAX) {
    REAL(n_matchr)[0] = NA_REAL;
    warning("count of matches exceeds DBL_MAX, returning NA in 'nMatch' field");
  } else {
    REAL(n_matchr)[0] = (double)n_match;
  }
  if (verbose>0)
    Rprintf("smergeR: smergeC of %d x %d = %"PRIu64"; took %.3fs\n", nx, ny, n_match, omp_get_wtime() - t);

  t = omp_get_wtime();
  SEXP ans = outSmergeR(nx, starts, lens, x_ord, out_starts, out_lens, x_idx, y_idx, n_matchr, x_lens1, y_lens1, xy_lens1, (bool)LOGICAL(out_bmerge)[0]);
  if (verbose>0)
    Rprintf("smergeR: outSmerge %s took %.3fs\n", x_ord ? "(was sorted)" : "(unsort)", omp_get_wtime() - t);
  if (verbose>0)
    Rprintf("smergeR: all took %.3fs\n", omp_get_wtime() - t_total);

  UNPROTECT(protecti);
  return ans;
}
