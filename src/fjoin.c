#include "data.table.h"

/*
 * sort-merge join
 * 
 * split whole 1:nx_starts into numbers of buckets equal to threads*2
 * each thread do binary merge x of first and last element on y, and then sort-merge to a subset of y defined by those matches
 */
static int max_i_match(int *y, int ny, int *y_o, bool y_ord, int val) { // those need to be binary search obviously
  for (int i=0; i<ny; ++i) {
    //Rprintf("i=%d: y[y_o[i]] > val: %d > %d\n", i, y[y_o[i]-1], val);
    if (y[y_o[i]-1]>val) {
      if (i==0) error("i=0 max_i_match, unhandled");
      return(i-1);
    }
  }
  error("not yet handled in max_i_match");
}
static int min_i_match(int *y, int ny, int *y_o, bool y_ord, int val) {
  for (int i=ny-1; i<ny; --i) {
    if (y[y_o[i]-1]<val) {
      if (i==ny-1) error("i=ny-1 min_i_match, unhandled");
      return(i+1);
    }
  }
  error("not yet handled in min_i_match");
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

void grpLens(int *starts, int n_starts, int n, int *lens) { // #4395
  for (int i=0; i<n_starts-1; ++i)
    lens[i] = starts[i+1]-starts[i];
  lens[n_starts-1] = n - starts[n_starts-1]+1;
  return;
}

void fjoinC(int *x, int nx, int *x_starts, int nx_starts,
           int *y, int ny, int *y_starts, int ny_starts,
           //joinhow how,
           bool *match, int *matchn,
           int *starts_x, int *lens_x,
           int *starts_y, int *lens_y) {
  bool x_ord=true, y_ord=true;
  int *x_o=0, *y_o=0;
  
  bool unq_x = nx_starts==nx, unq_y = ny_starts==ny;
  int *x_lens=0, *y_lens=0;
  double t_lens = omp_get_wtime();
  if (!unq_x || true) {
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
  //int min_i = 0, max_i = x_starts[nx_starts-1]-1;
  //Rprintf("x[i]=[%d:%d]\n", min_i, max_i);
  //int nth = getDTthreads();
  //int nBatch=nth*2;
  int nBatch=1;
  //int b1_min_i = min_i, b1_max_i = max_i;
  //int b1_min_i = min_i, b1_max_i = x_starts[nx_starts/2]-1;
  //int b2_min_i = x_starts[nx_starts/2+1]-1, b2_max_i = max_i;
  //Rprintf("x: 1[%d-%d]\n", b1_min_i, b1_max_i);
  //Rprintf("x: 2[%d-%d]\n", b2_min_i, b2_max_i);
  
  
  int *xs, *ys, nxs, nys;
  xs = x_starts + 0*sizeof(int);
  nxs = nx_starts;
  ys = y_starts + 0*sizeof(int);
  nys = ny_starts;
  
  int *th = (int *)R_alloc(nBatch, sizeof(int)); // report number of used threads
  t_batch = omp_get_wtime() - t_batch;
  
  double t_join = omp_get_wtime();
  //#pragma omp parallel for schedule(dynamic) num_threads(nth)
  for (int b=0; b<nBatch; ++b) {
    // fjoin1(/*local in*/xs, nxs, ys, nys, /*global in*/x, x_lens, y, y_lens, /*out*/start_y, lens_y);
    int i=0, j=0, is=0, js=0, x_i, y_j;
    while (is<nxs && js<nys) {
      //Rprintf("============= is=%d\n", is);
      i = xs[is]-1, j = ys[js]-1;
      x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        //Rprintf("x[%d]==y[%d]: %d\n", i, j, x_i);
        int j1 = j+1;
        for (int ii=0; ii<x_lens[is]; ++ii) {
          //Rprintf("starts_y[%d]=%d\n", i+ii, j1);
          starts_y[i+ii] = j1; lens_y[i+ii] = y_lens[js];
        }
        is++;
      } else if (x_i < y_j) is++; else if (x_i > y_j) js++;
    }
    /*while (is<nx_starts && js<ny_starts) {
      //Rprintf("============= is=%d\n", is);
      i = x_starts[is]-1, j = y_starts[js]-1;
      x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        //Rprintf("x[%d]==y[%d]: %d\n", i, j, x_i);
        int j1 = j+1;
        for (int ii=0; ii<x_lens[is]; ++ii) {
          //Rprintf("starts_y[%d]=%d\n", i+ii, j1);
          starts_y[i+ii] = j1; lens_y[i+ii] = y_lens[js];
        }
        is++;
      } else if (x_i < y_j) is++; else if (x_i > y_j) js++;
    }*/
    th[b] = omp_get_thread_num();
  }
  t_join = omp_get_wtime() - t_join;
  //for (int b=0; b<nBatch; ++b) Rprintf("th(batch[%d])=%d\n", b, th[b]);
  
  if (false) {
    error("input must be ordered!");
    if (false) {
      // find bucket ranges
      int min_x_i = x_o[0]-1, max_x_i = x_o[nx-1]-1;
      int min_y_i = y_o[0]-1, max_y_i = y_o[ny-1]-1;
      //int min_x = x[min_x_i], max_x = x[max_x_i];
      //int min_y = y[min_y_i], max_y = y[max_y_i];
      
      //bool trim_min_x = min_x < min_y;
      //bool trim_min_y = min_x > min_y;
      //bool trim_max_x = max_x > max_y;
      //bool trim_max_y = max_x < max_y;
      // todo trim buckets to join-able subsets of x and y
      
      //Rprintf("min_x=%d; max_x=%d; min_y=%d, max_y=%d\n", min_x, max_x, min_y, max_y);
      //int min_xy = imin(min_x, min_y), max_xy = imin(max_x, max_y);
      //if (max_xy - min_xy < 2) error("not handled yet");
      
      int n_buckets = 2;
      if (n_buckets != 2) error("not handled yet");
      // this needs to be done for every bucket
      if (y_ord) error("y must not be sorted");
      
      int b1_max_x_o_i = max_i_match(x, nx, x_o, x_ord, x[x_o[nx/2]-1]);
      Rprintf("b1_max_x_o_i=%d; x[x_o[b1_max_x_o_i]-1]=%d\n", b1_max_x_o_i, x[x_o[b1_max_x_o_i]-1]);
      int b1_min_x_i = min_x_i, b1_max_x_i = x_o[b1_max_x_o_i]-1; // this need to ensure that duplicate goes into same bucket!! fjoin(c(1:3,3L,3L,5L,4L), y), 3 goes into both buckets!
      int b2_min_x_i = x_o[b1_max_x_o_i+1]-1, b2_max_x_i = max_x_i;
      if (x[b1_max_x_i]==x[b2_min_x_i]) error("value of x=%d fits into multiple buckets", x[b2_min_x_i]);
      Rprintf("min(x)=%d; max(x)=%d; len(x)=%d\n", x[min_x_i], x[max_x_i], nx);
      Rprintf("b1(min(x))=%d; b1(max(x))=%d; b1(len(x))=%d\n", x[b1_min_x_i], x[b1_max_x_i], b1_max_x_o_i-0);
      Rprintf("b2(min(x))=%d; b2(max(x))=%d; b2(len(x))=%d\n", x[b2_min_x_i], x[b2_max_x_i], nx-b1_max_x_o_i+1);
      error("dev");
      int b1_max_y_o_i = max_i_match(y, ny, y_o, y_ord, /*value to match to*/x[b1_max_x_i]);
      int b1_min_y_i = min_y_i, b1_max_y_i = y_o[b1_max_y_o_i]-1;
      int b2_min_y_o_i = min_i_match(y, ny, y_o, y_ord, x[b2_min_x_i]);
      int b2_min_y_i = y_o[b2_min_y_o_i]-1, b2_max_y_i = max_y_i;
      Rprintf("b1_min_x_i=%d, b1_max_x_i=%d, b2_min_x_i=%d, b2_max_x_i=%d\n", b1_min_x_i, b1_max_x_i, b2_min_x_i, b2_max_x_i);
      Rprintf("buckets: 1(x:%d-%d, y:%d-%d); 2(x:%d-%d, y:%d-%d)\n", x[b1_min_x_i], x[b1_max_x_i], y[b1_min_y_i], y[b1_max_y_i], x[b2_min_x_i], x[b2_max_x_i], y[b2_min_y_i], y[b2_max_y_i]);
      // openmp here!!
      for (int b=0; b<n_buckets; ++b) {
        int *x_o_, nx_, *y_o_, ny_;
        if (b==0) {
          x_o_ = x_o;
          nx_ = nx/2;
          y_o_ = y_o;
          ny_ = b1_max_y_o_i;
        } else {
          x_o_ = x_o + (nx/2+1)*sizeof(int);
          nx_ = nx-nx/2;
          y_o_ = y_o + b2_min_y_o_i*sizeof(int);
          ny_ = ny-b2_min_y_o_i+1;
        }
        Rprintf("--- bucket %d content ---", b);
        Rprintf("\nx (len %d):", nx_);
        for (int i=0; i<nx_; ++i) Rprintf(" %d", x[x_o_[i]-1]);
        Rprintf("\ny (len %d):", ny_);
        for (int i=0; i<ny_; ++i) Rprintf(" %d", y[y_o_[i]-1]);
        Rprintf("\n");
        //bucketjoin(x, y, bucket_len_x, bucket_start_x, x_o, x_min, y
      }
      error("dev");
      //error("dev: !x_ord && !y_ord && (!unq_x || !unq_y)");
    }
    //Rprintf("x_i=%d; y[y_o[j]-1]= y[y_o[%d]-1]= y[%d-1]= y[%d]= %d\n", x[i], j, y_o[j], y_o[j]-1, y[y_o[j]-1]);
  }
  Rprintf("fjoinC: lens %.3fs; batch %.3fs; join %.3fs; used %d th\n", t_lens, t_batch, t_join, unqN(th, nBatch));
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
