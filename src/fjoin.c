#include "data.table.h"

/*
 * sort-merge join
 * 
 * split whole 1:nx_starts into numbers of buckets equal to threads*2
 * each thread do binary merge x of first and last element on y, and then sort-merge to a subset of y defined by those matches
 */
static int max_i_match(int *y, int *y_starts, int ny_starts, int val) { // those need to be binary search obviously
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

// materialize counts of duplicated entries
void grpLens(int *starts, int n_starts, int n, int *lens) { // #4395
  for (int i=0; i<n_starts-1; ++i)
    lens[i] = starts[i+1]-starts[i];
  lens[n_starts-1] = n - starts[n_starts-1]+1;
  return;
}

// workhorse join that runs in parallel
void fjoin1(int *xs, int *xl, int nxs, int *ys, int *yl, int nys, int *x, int *y, int *starts_y, int *lens_y) {
  int i=0, j=0, is=0, js=0, x_i, y_j;
  while (is<nxs && js<nys) {
    //Rprintf("============= is=%d\n", is);
    i = xs[is]-1, j = ys[js]-1;
    x_i = x[i], y_j = y[j];
    if (x_i == y_j) {
      //Rprintf("x[%d]==y[%d]: %d\n", i, j, x_i);
      int j1 = j+1;
      //Rprintf("x_lens[is]=x_lens[%d]=%d\n",is,xl[is]);
      for (int ii=0; ii<xl[is]; ++ii) {
        //Rprintf("starts_y[%d]=%d\n", i+ii, j1);
        starts_y[i+ii] = j1; lens_y[i+ii] = yl[js];
      }
      is++;
    } else if (x_i < y_j) is++; else if (x_i > y_j) js++;
  }
  return;
}

void fjoinC(int *x, int nx, int *x_starts, int nx_starts,
           int *y, int ny, int *y_starts, int ny_starts,
           //joinhow how,
           bool *match, int *matchn,
           int *starts_x, int *lens_x,
           int *starts_y, int *lens_y) {
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
  
  double t_batch = omp_get_wtime();
  /*
  int min_i = 0, max_i = x_starts[nx_starts-1]-1;
  Rprintf("x[i]=[%d:%d]\n", min_i, max_i);
  int b1_min_i = min_i, b1_max_i = max_i;
  int b1_min_i = min_i, b1_max_i = x_starts[nx_starts/2]-1;
  int b2_min_i = x_starts[nx_starts/2+1]-1, b2_max_i = max_i;
  Rprintf("x: 1[%d-%d]\n", b1_min_i, b1_max_i);
  Rprintf("x: 2[%d-%d]\n", b2_min_i, b2_max_i);
  */
  int nth = 2;//getDTthreads();
  //int nBatch=nth*2;
  int nBatch=2;
  
  int *xs, *ys, nxs, nys, *xl, *yl;
  /*
  xs = x_starts + 0*sizeof(int);
  nxs = nx_starts;
  ys = y_starts + 0*sizeof(int);
  nys = ny_starts;
   */
  int b1_nxs = nx_starts/2, b2_nxs = nx_starts-b1_nxs;
  int *b1_xs = x_starts, *b2_xs = x_starts + b1_nxs;
  int *b1_xl = x_lens, *b2_xl = x_lens + b1_nxs;
  //Rprintf("batch sizes nx: b1=%d; b2=%d\nidx of first in batch: x_starts[0]: b1=%d; b2=%d\nfirst value in batch: x[x_starts[0]-1]: b1=%d; b2=%d\n\n",
  //                      b1_nxs, b2_nxs,                                 b1_xs[0], b2_xs[0],                         x[b1_xs[0]-1], x[b2_xs[0]-1]);
  //Rprintf("finding bounds of b1 y starts indices\n\n");
  int b1_ylow = min_i_match(y, y_starts, ny_starts, x[b1_xs[0]-1]);
  int b1_yupp = max_i_match(y, y_starts, ny_starts, x[b1_xs[b1_nxs-1]-1]);
  //Rprintf("\nb1_ylow=%d; b1_yupp=%d\ny[y_starts[b1_ylow]-1]=%d; y[ny_starts[b1_yupp]-1]=%d\n", b1_ylow, b1_yupp, y[y_starts[b1_ylow]-1], y[y_starts[b1_yupp]-1]);
  int b2_ylow = min_i_match(y, y_starts, ny_starts, x[b2_xs[0]-1]);
  int b2_yupp = max_i_match(y, y_starts, ny_starts, x[b2_xs[b2_nxs-1]-1]);
  //Rprintf("\nb2_ylow=%d; b2_yupp=%d\ny[y_starts[b2_ylow]-1]=%d; y[ny_starts[b2_yupp]-1]=%d\n", b2_ylow, b2_yupp, y[y_starts[b2_ylow]-1], y[y_starts[b2_yupp]-1]);
  int b1_nys = b1_yupp-b1_ylow+1, b2_nys = b2_yupp-b2_ylow+1;
  int *b1_ys = y_starts, *b2_ys = y_starts + b1_nys;
  int *b1_yl = y_lens, *b2_yl = y_lens + b1_nys;
  //Rprintf("batch sizes ny: b1=%d; b2=%d\nidx of first in batch: y_starts[0]: b1=%d; b2=%d\nfirst value in batch: y[y_starts[0]-1]: b1=%d; b2=%d\n\n",
  //        b1_nys, b2_nys,                                 b1_ys[0], b2_ys[0],                         y[b1_ys[0]-1], y[b2_ys[0]-1]);
  int *th = (int *)R_alloc(nBatch, sizeof(int)); // report threads used
  t_batch = omp_get_wtime() - t_batch;
  double t_join = omp_get_wtime();
  #pragma omp parallel for schedule(dynamic) num_threads(nth)
  for (int b=0; b<nBatch; ++b) {
    if (b==0) {
      xs = b1_xs; nxs = b1_nxs; ys = b1_ys; nys = b1_nys, xl = b1_xl, yl = b1_yl;
    } else if (b==1) {
      xs = b2_xs; nxs = b2_nxs; ys = b2_ys; nys = b2_nys, xl = b2_xl, yl = b2_yl;
    }
    fjoin1(xs, xl, nxs, ys, yl, nys, /*batch specific input: shifted pointers and size*/
           x, y,                     /*common input*/
           starts_y, lens_y);        /*common output*/
    th[b] = omp_get_thread_num();
  }
  for (int b=0; b<nBatch; ++b) Rprintf("th[%d]=%d\n", th[b]);
  t_join = omp_get_wtime() - t_join;

  Rprintf("fjoinC: lens %.3fs; batch %.3fs; join %.3fs; used %dth\n", t_lens, t_batch, t_join, unqN(th, nBatch));
  matchn[0] = nx;
}

SEXP sortInt(SEXP x, SEXP idx) {
  if (!LENGTH(idx))
    error("already sorted, should be escaped");
  SEXP ans = PROTECT(allocVector(INTSXP, LENGTH(x)));
  int *xp = INTEGER(x), *idxp = INTEGER(idx), *ansp = INTEGER(ans);
  for (int i=0; i<LENGTH(x); ++i)
    ansp[i] = xp[idxp[i]-1];
  UNPROTECT(1);
  return ans;
}

SEXP joinOut(int matchn, int *starts_x, int *lens_x, int *starts_y, int *lens_y, bool x_ord, int *x_o_idx) {
  SEXP ans = PROTECT(allocVector(VECSXP, 4)), ansnames;
  setAttrib(ans, R_NamesSymbol, ansnames=allocVector(STRSXP, 4));
  SET_VECTOR_ELT(ans, 0, allocVector(INTSXP, 0));
  SET_STRING_ELT(ansnames, 0, mkChar("starts_x"));
  //int *starts_x_p = INTEGER(VECTOR_ELT(ans, 0));
  SET_VECTOR_ELT(ans, 1, allocVector(INTSXP, 0));
  SET_STRING_ELT(ansnames, 1, mkChar("lens_x"));
  //int *lens_x_p = INTEGER(VECTOR_ELT(ans, 1));
  SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, matchn));
  SET_STRING_ELT(ansnames, 2, mkChar("starts"));
  int *starts_y_p = INTEGER(VECTOR_ELT(ans, 2));
  SET_VECTOR_ELT(ans, 3, allocVector(INTSXP, matchn));
  SET_STRING_ELT(ansnames, 3, mkChar("lens"));
  int *lens_y_p = INTEGER(VECTOR_ELT(ans, 3));
  if (!x_ord) {
    for (int i=0; i<matchn; ++i) {
      //starts_x_p[i] = starts_x[i];
      //lens_x_p[i] = lens_x[i];
      starts_y_p[i] = starts_y[x_o_idx[i]-1];
      lens_y_p[i] = lens_y[x_o_idx[i]-1];
    }
  } else {
    for (int i=0; i<matchn; ++i) {
      //starts_x_p[i] = starts_x[i];
      //lens_x_p[i] = lens_x[i];
      starts_y_p[i] = starts_y[i];
      lens_y_p[i] = lens_y[i];
    }
  }
  UNPROTECT(1);
  return ans;
}

SEXP fjoinR(SEXP x, SEXP y, SEXP how) {

  if (!isInteger(x) || !isInteger(y))
    error("must be integer");
  int protecti=0, nx = LENGTH(x), ny = LENGTH(y);

  double t_index = omp_get_wtime();
  SEXP x_idx = PROTECT(forder(x, R_NilValue, ScalarLogical(TRUE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE))); protecti++;
  SEXP y_idx = PROTECT(forder(y, R_NilValue, ScalarLogical(TRUE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE))); protecti++;
  SEXP x_starts = getAttrib(x_idx, sym_starts);
  SEXP y_starts = getAttrib(y_idx, sym_starts);
  t_index = omp_get_wtime() - t_index;

  bool x_ord = LENGTH(x_idx)==0, y_ord = LENGTH(y_idx)==0;
  SEXP x_o_idx = R_NilValue;
  double t_sort = omp_get_wtime();
  if (!x_ord) {
    x = PROTECT(sortInt(x, x_idx)); protecti++;
    x_o_idx = PROTECT(forder(x_idx, R_NilValue, /*retGrp=*/ScalarLogical(FALSE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE))); protecti++;
  }
  if (!y_ord) {
    y = PROTECT(sortInt(y, y_idx)); protecti++;
  }
  t_sort = omp_get_wtime() - t_sort;

  double t_alloc = omp_get_wtime();
  int ans_n = nx;
  bool *match = (bool *)R_alloc(0, sizeof(bool)); // not sure if we need this
  int matchn = 0;
  int *starts_x = (int *)R_alloc(0, sizeof(int));
  int *lens_x = (int *)R_alloc(0, sizeof(int));
  int *starts_y = (int *)R_alloc(ans_n, sizeof(int));
  int *lens_y = (int *)R_alloc(ans_n, sizeof(int));
  for (int i=0; i<ans_n; ++i) /*starts_x[i] = lens_x[i] = */starts_y[i] = lens_y[i] = NA_INTEGER;
  t_alloc = omp_get_wtime() - t_alloc;

  double t_fjoin = omp_get_wtime();
  //            *x, nx,         *x_starts,        nx_starts,
  fjoinC(INTEGER(x), nx, INTEGER(x_starts), LENGTH(x_starts),
         INTEGER(y), ny, INTEGER(y_starts), LENGTH(y_starts),
         match, &matchn,
         starts_x, lens_x, starts_y, lens_y);
  t_fjoin = omp_get_wtime() - t_fjoin;

  double t_ans = omp_get_wtime();
  SEXP ans = R_NilValue;
  if (!x_ord) {
    ans = joinOut(matchn, starts_x, lens_x, starts_y, lens_y, x_ord, INTEGER(x_o_idx));
  } else {
    ans = joinOut(matchn, starts_x, lens_x, starts_y, lens_y, x_ord, /*anything*/starts_x);
  }
  t_ans = omp_get_wtime() - t_ans;
  Rprintf("fjoinR: index %.3fs; sort %.3fs; alloc %.3fs; fjoinC %.3fs; ans %.3fs\n", t_index, t_sort, t_alloc, t_fjoin, t_ans);
  UNPROTECT(protecti);
  return ans;
}
