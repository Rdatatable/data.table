#include "data.table.h"

/*
 * as of now these are optimized routines for joining
 * - single integer columns
 * - sorted (experimental) or indexed
 * 
 * TODO:
 * type of join is pushed down here so we compute only what is necessary
 * mult!="all" and "unique-key" branches escape extra computation
 */

typedef enum {
  inner, left, right, full, semi, anti, cross
} joinhow;

SEXP joinOut(int matchn, int *starts_x, int *lens_x, int *starts_y, int *lens_y) {
  SEXP ans = PROTECT(allocVector(VECSXP, 4)), ansnames;
  setAttrib(ans, R_NamesSymbol, ansnames=allocVector(STRSXP, 4));
  SET_VECTOR_ELT(ans, 0, allocVector(INTSXP, 0));
  SET_STRING_ELT(ansnames, 0, mkChar("starts_x"));
  //int *starts_x_p = INTEGER(VECTOR_ELT(ans, 0));
  SET_VECTOR_ELT(ans, 1, allocVector(INTSXP, 0));
  SET_STRING_ELT(ansnames, 1, mkChar("lens_x"));
  //int *lens_x_p = INTEGER(VECTOR_ELT(ans, 1));
  SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, matchn));
  SET_STRING_ELT(ansnames, 2, mkChar("starts_y"));
  int *starts_y_p = INTEGER(VECTOR_ELT(ans, 2));
  SET_VECTOR_ELT(ans, 3, allocVector(INTSXP, matchn));
  SET_STRING_ELT(ansnames, 3, mkChar("lens_y"));
  int *lens_y_p = INTEGER(VECTOR_ELT(ans, 3));
  for (int i=0; i<matchn; ++i) {
    //starts_x_p[i] = starts_x[i];
    //lens_x_p[i] = lens_x[i];
    starts_y_p[i] = starts_y[i];
    lens_y_p[i] = lens_y[i];
  }
  UNPROTECT(1);
  return ans;
}

/*
 * fast sorted join
 */
/*
void sjoin(int *x, int nx, int *y, int ny, joinhow how, bool *match, int *matchi, int *matchstarts, int *matchlens, int *matchn) {
  int imatch = 0;
  if (how==left) {
    for (int i=0, j=0; i<nx;) {
      if (x[i]==y[j]) {
        Rprintf("x[%d]==y[%d]\n", i, j);
        match[i] = true;
        matchi[imatch++] = i;
        matchstarts[i] = j;
        matchlens[i] = 1;
        int jj = j+1; // rather than j++, so duplicates on x side works
        while(x[i]==y[jj]) { // multiple matches
          matchlens[i]++; jj++;
        }
        i++; // we could have an outer while instead, then we dont have to recalculate matchlens when dups to dups match 2x2
        continue;
      } else if (x[i]<y[j]) {
        Rprintf("x[%d]<y[%d]\n", i, j);
        match[i] = false;
        i++;
        continue;
      } else if (x[i]>y[j]) {
        Rprintf("x[%d]>y[%d]\n", i, j);
        j++;
        continue;
      }
    }
  } else {
    error("not yet implemented");
  }
  matchn[0] = imatch;
}
SEXP sjoinR(SEXP x, SEXP y) {
  if (!isInteger(x) || !isInteger(y))
    error("must be integer");
  int nx = LENGTH(x), ny = LENGTH(y);
  //  left:  nx
  // right:  ny
  // inner:  min(nx, ny)
  //  full:  max(nx, ny)
  double t_alloc = omp_get_wtime();
  bool *match = (bool *)R_alloc(nx, sizeof(bool)); // not sure if we need this
  int *matchi = (int *)R_alloc(nx, sizeof(int));
  int *matchlens = (int *)R_alloc(nx, sizeof(int));
  int *matchstarts = (int *)R_alloc(nx, sizeof(int));
  int matchn = 0;
  t_alloc = omp_get_wtime() - t_alloc;
  double t_sjoin = omp_get_wtime();
  sjoin(INTEGER(x), nx, INTEGER(y), ny, left, match, matchi, matchstarts, matchlens, &matchn);
  t_sjoin = omp_get_wtime() - t_sjoin;
  Rprintf("alloc took %.3fs; sjoin took %.3fs\n", t_alloc, t_sjoin);
  return joinOut(matchn, matchi, matchstarts, matchlens);
}
*/

/*
 * fast indexed join
 * sort-merge join
 */
void ijoin(int *x, int nx, int *x_o, bool x_ord, int *x_starts, int nx_starts,
           int *y, int ny, int *y_o, bool y_ord, int *y_starts, int ny_starts,
           joinhow how,
           bool *match, int *matchn,
           int *starts_x, int *lens_x,
           int *starts_y, int *lens_y) {
  if (how != left)
    error("only left join implemented so far");
  if (!x_ord || !y_ord)
    error("support for already unsorted not yet implemented");
  bool unq_x = nx_starts==nx, unq_y = ny_starts==ny;
  //int imatch = 0;
  
  int *x_lens=0, *y_lens=0;
  if (!unq_x) {
    x_lens = (int *)R_alloc(nx_starts, sizeof(int)); // #4395
    for (int i=0; i<nx_starts-1; ++i) x_lens[i] = x_starts[i+1]-x_starts[i];
    x_lens[nx_starts-1] = nx-x_starts[nx_starts-1];
  }
  if (!unq_y) {
    y_lens = (int *)R_alloc(ny_starts, sizeof(int));
    for (int i=0; i<ny_starts-1; ++i) y_lens[i] = y_starts[i+1]-y_starts[i];
    y_lens[ny_starts-1] = ny-y_starts[ny_starts-1];
  }

  int i=0, j=0, is=0, js=0;
  if (how==left && unq_x && unq_y) {
    while (i<nx && j<ny) {
      int x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        starts_y[i] = j+1; lens_y[i] = 1;
        i++;
      }
      else if (x_i < y_j) i++;
      else if (x_i > y_j) j++;
    }
  } else if (how==left && unq_y) {
    while (is<nx_starts && j<ny) {
      int i = x_starts[is]-1;
      int x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        int j1 = j+1;
        for (int ii=0; ii<x_lens[is]; ++ii) {
          starts_y[i+ii] = j1; lens_y[i+ii] = 1;
        }
        is++;
      }
      else if (x_i < y_j) is++;
      else if (x_i > y_j) j++;
    }
  } else if (how==left && unq_x) {
    while (i<nx && js<ny_starts) {
      int j = y_starts[js]-1;
      int x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        starts_y[i] = j+1;
        lens_y[i] = y_lens[js];
        i++;
      }
      else if (x_i < y_j) i++;
      else if (x_i > y_j) js++;
    }
  } else if (how==left) {
    while (is<nx_starts && js<ny_starts) {
      int i = x_starts[is]-1, j = y_starts[js]-1;
      int x_i = x[i], y_j = y[j];
      if (x_i == y_j) {
        int j1 = j+1;
        for (int ii=0; ii<x_lens[is]; ++ii) {
          starts_y[i+ii] = j1; lens_y[i+ii] = y_lens[js];
        }
        is++;
      }
      else if (x_i < y_j) is++;
      else if (x_i > y_j) js++;
    }
  } else if (how==inner && unq_x && unq_y) {
    
  } else if (how==full && unq_x && unq_y) {
    
  }
  
  matchn[0] = nx;
}

SEXP ijoinR(SEXP x, SEXP y) {
  if (!isInteger(x) || !isInteger(y))
    error("must be integer");
  int nx = LENGTH(x), ny = LENGTH(y);
  double t_index = omp_get_wtime();
  SEXP x_idx = PROTECT(forder(x, R_NilValue, ScalarLogical(TRUE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE)));
  SEXP y_idx = PROTECT(forder(y, R_NilValue, ScalarLogical(TRUE), ScalarLogical(TRUE), ScalarInteger(1), ScalarLogical(FALSE)));
  SEXP x_starts = getAttrib(x_idx, sym_starts);
  SEXP y_starts = getAttrib(y_idx, sym_starts);
  t_index = omp_get_wtime() - t_index;
  double t_alloc = omp_get_wtime();
  bool *match = (bool *)R_alloc(0, sizeof(bool)); // not sure if we need this
  int matchn = 0;
  int *starts_x = (int *)R_alloc(0, sizeof(int));
  int *lens_x = (int *)R_alloc(0, sizeof(int));
  int *starts_y = (int *)R_alloc(nx, sizeof(int));
  int *lens_y = (int *)R_alloc(nx, sizeof(int));
  for (int i=0; i<nx; ++i) /*starts_x[i] = lens_x[i] = */starts_y[i] = lens_y[i] = NA_INTEGER;
  t_alloc = omp_get_wtime() - t_alloc;
  double t_ijoin = omp_get_wtime();
  //            *x, nx,           *x_o,            x_ord,         *x_starts,        nx_starts,
  ijoin(INTEGER(x), nx, INTEGER(x_idx), LENGTH(x_idx)==0, INTEGER(x_starts), LENGTH(x_starts),
        INTEGER(y), ny, INTEGER(y_idx), LENGTH(y_idx)==0, INTEGER(y_starts), LENGTH(y_starts),
        left, match, &matchn,
        starts_x, lens_x, starts_y, lens_y);
  t_ijoin = omp_get_wtime() - t_ijoin;
  Rprintf("index took %.3fs; alloc took %.3fs; ijoin took %.3fs\n", t_index, t_alloc, t_ijoin);
  UNPROTECT(2);
  return joinOut(matchn, starts_x, lens_x, starts_y, lens_y);
}
