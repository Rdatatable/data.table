#include "data.table.h"

SEXP reorder(SEXP x, SEXP order)
{
    // For internal use only by setkey().
    // 'order' must strictly be a permutation of 1:n (i.e. no repeats, zeros or NAs)
    // If only a small subset in the middle is reordered the ends are moved in: [start,end].
    // x may be a vector, or a list of same-length vectors such as data.table
    
    R_len_t nrow, ncol;
    int maxSize = 0;
    if (isNewList(x)) {
      nrow = length(VECTOR_ELT(x,0));
      ncol = length(x);
      for (int i=0; i<ncol; i++) {
        SEXP v = VECTOR_ELT(x,i);
        if (SIZEOF(v)!=4 && SIZEOF(v)!=8)
          error("Item %d of list is type '%s' which isn't yet supported", i+1, type2char(TYPEOF(v)));
        if (length(v)!=nrow)
          error("Column %d is length %d which differs from length of column 1 (%d). Invalid data.table.", i+1, length(v), nrow);
        if (SIZEOF(v) > maxSize)
          maxSize=SIZEOF(v);
      }
    } else {
      if (SIZEOF(x)!=4 && SIZEOF(x)!=8)
        error("reorder accepts vectors but this non-VECSXP is type '%s' which isn't yet supported", type2char(TYPEOF(x)));
      maxSize = SIZEOF(x);
      nrow = length(x);
      ncol = 1;
    }
    if (!isInteger(order)) error("order must be an integer vector");
    if (length(order) != nrow) error("nrow(x)[%d]!=length(order)[%d]",nrow,length(order));
    
    R_len_t start = 0;
    while (start<nrow && INTEGER(order)[start] == start+1) start++;
    if (start==nrow) return(R_NilValue);  // input is 1:n, nothing to do
    R_len_t end = nrow-1;
    while (INTEGER(order)[end] == end+1) end--;
    for (R_len_t i=start; i<=end; i++) { 
      int itmp = INTEGER(order)[i]-1;
      if (itmp<start || itmp>end) error("order is not a permutation of 1:nrow[%d]", nrow);
    }
    // Creorder is for internal use (so we should get the input right!), but the check above seems sensible, otherwise
    // would be segfault below. The for loop above should run in neglible time (sequential) and will also catch NAs.
    // It won't catch duplicates in order, but that's ok. Checking that would be going too far given this is for internal use only.
    
    // Enough working ram for one column of the largest type, for every thread.
    // Up to a limit of 1GB total. It's system dependent how to find out the truly free RAM - TODO.
    // Without a limit it could easily start swapping and not work at all.
    int nth = MIN(getDTthreads(), ncol);
    size_t oneTmpSize = (end-start+1)*(size_t)maxSize;
    size_t totalLimit = 1024*1024*(size_t)1024;  // 1GB
    nth = MIN(totalLimit/oneTmpSize, nth);
    if (nth==0) nth=1;  // if one column's worth is very big, we'll just have to try
    char *tmp[nth];  // VLA ok because small; limited to max getDTthreads() not ncol which could be > 1e6
    int ok=0; for (; ok<nth; ok++) {
      tmp[ok] = malloc(oneTmpSize);
      if (tmp[ok] == NULL) break;
    }
    if (ok==0) error("unable to allocate %d * %d bytes of working memory for reordering data.table", end-start+1, maxSize);
    nth = ok;  // as many threads for which we have a successful malloc
    // So we can still reorder a 10GB table in 16GB of RAM, as long as we have at least one column's worth of tmp
    
    #pragma omp parallel for schedule(dynamic) num_threads(nth)
    for (int i=0; i<ncol; i++) {
      const SEXP v = isNewList(x) ? VECTOR_ELT(x,i) : x;
      const int size = SIZEOF(v);
      const int me = omp_get_thread_num();
      const int *vi = INTEGER(order)+start;
      if (size==4) {
        const int *vd = (const int *)DATAPTR(v);
        int *tmpp = (int *)tmp[me];
        for (int j=start; j<=end; j++) {
          *tmpp++ = vd[*vi++ -1];  // just copies 4 bytes, including pointers on 32bit
        }
      } else {
        const double *vd = (const double *)DATAPTR(v);
        double *tmpp = (double *)tmp[me];
        for (int j=start; j<=end; j++) {
          *tmpp++ = vd[*vi++ -1];  // just copies 8 bytes, pointers too including STRSXP and VECSXP
        }
      }
      // How is this possible to not only ignore the write barrier but in parallel too?
      // Only because this reorder() function accepts and checks a unique permutation of 1:nrow. It
      // performs an in-place shuffle. This operation in the end does not change gcgen, mark or
      // named/refcnt. They all stay the same even for STRSXP and VECSXP because it's just a data shuffle.
      //
      // Theory:
      // The write to tmp is contiguous and io efficient (so less threads should not help that)
      // The read from vd is as io efficient as order is ordered (the more threads the better when close
      // to ordered but less threads may help when not very ordered).
      // TODO large data benchmark to confirm theory and auto tune.
      // io probably limits too much but at least this is our best shot (e.g. branchless) in finding out
      // on other platforms with faster bus, perhaps
      
      // copy the reordered data back into the original vector
      memcpy((char *)DATAPTR(v) + start*(size_t)size,
             tmp[me],
             (end-start+1)*(size_t)size);
      // size_t, otherwise #5305 (integer overflow in memcpy)
    }
    for (int i=0; i<nth; i++) free(tmp[i]);
    return(R_NilValue);
}

// reverse a vector - equivalent of rev(x) in base, but implemented in C and about 12x faster (on 1e8)
SEXP setrev(SEXP x) {
    R_len_t j, n, len;
    size_t size;
    char *tmp, *xt;
    if (TYPEOF(x) == VECSXP || isMatrix(x)) error("Input 'x' must be a vector");
    len = length(x);
    if (len <= 1) return(x);
    size = SIZEOF(x);
    if (!size) error("don't know how to reverse type '%s' of input 'x'.",type2char(TYPEOF(x)));
    n = (int)(len/2);
    xt = (char *)DATAPTR(x);
    if (size==4) {
        tmp = (char *)Calloc(1, int);
        if (!tmp) error("unable to allocate temporary working memory for reordering x");
        for (j=0;j<n;j++) {
            *(int *)tmp = ((int *)xt)[j];  // just copies 4 bytes (pointers on 32bit too)
            ((int *)xt)[j] = ((int *)xt)[len-1-j];
            ((int *)xt)[len-1-j] = *(int *)tmp;
        }
    } else {
        if (size!=8) error("Size of x isn't 4 or 8");
        tmp = (char *)Calloc(1, double);
        if (!tmp) error("unable to allocate temporary working memory for reordering x");
        for (j=0;j<n;j++) {
            *(double *)tmp = ((double *)xt)[j];  // just copies 8 bytes (pointers on 64bit too)
            ((double *)xt)[j] = ((double *)xt)[len-1-j];
            ((double *)xt)[len-1-j] = *(double *)tmp;
        }
    }
    Free(tmp);
    return(R_NilValue);
}


