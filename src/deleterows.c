#include "data.table.h"

static void computePrefixSum(const int *keep, int *dest, int n, int nthreads);
static void compactVectorRaw(SEXP col, const int *dest, const int *keep, int new_nrow, int old_nrow);

SEXP deleteRows(SEXP dt, SEXP rows_to_delete) {
  if (!isNewList(dt))
    error("Internal error: deleteRows received non-list dt"); // #nocov
  if (!xlength(dt)) return dt; // zero-column data.tabl
  for (R_xlen_t i = 0; i < length(dt); i++) {
    SEXP col = VECTOR_ELT(dt, i);
    if (!isVector(col)) error("Cannot delete rows by reference from non-vector column %ld", i + 1);
    if (ALTREP(col)) SET_VECTOR_ELT(dt, i, copyAsPlain(col, true));
  }

  const R_xlen_t ncol = length(dt);
  const R_xlen_t old_nrow = length(VECTOR_ELT(dt, 0));
  int nprotect = 0;

  if (old_nrow == 0) return dt;

  if (!isInteger(rows_to_delete) && !isLogical(rows_to_delete))
    internal_error(__func__, "rows_to_delete must be logical, integer, or numeric"); // #nocov

  int *keep = (int *)R_alloc(old_nrow, sizeof(int));
  const R_xlen_t n = length(rows_to_delete);
  for (R_xlen_t i = 0; i < old_nrow; i++) keep[i] = 1;
  int *idx = INTEGER(rows_to_delete);
  for (int j = 0; j < n; j++) {
    if (idx[j] == NA_INTEGER) continue;
    // should be checked from irows in [
    if (idx[j] < 1 || idx[j] > old_nrow) internal_error(__func__, "Row index %d out of range [1, %ld]", idx[j], old_nrow); //# nocov
    keep[idx[j] - 1] = 0;
  }

  int new_nrow = 0;
  for (int i = 0; i < old_nrow; i++) new_nrow += keep[i];
  if (new_nrow == old_nrow) return dt;

  int *dest = (int *)R_alloc(old_nrow, sizeof(int));
  const int nthreads = getDTthreads(old_nrow, true);
  computePrefixSum(keep, dest, old_nrow, nthreads);

  // Compact each column
  for (int j = 0; j < ncol; j++) {
    SEXP col = VECTOR_ELT(dt, j);
    if (!R_isResizable(col)) {
      // catered for ALTREP above
      SEXP newcol = R_duplicateAsResizable(col);
      SET_VECTOR_ELT(dt, j, newcol);
      col = newcol;
    }
    compactVectorRaw(col, dest, keep, new_nrow, old_nrow);
    R_resizeVector(col, new_nrow);
    SET_VECTOR_ELT(dt, j, col);
  }

  SEXP rownames = PROTECT(getAttrib(dt, R_RowNamesSymbol)); nprotect++;
  if (!isNull(rownames)) {
    // create them from scratch like in dogroups or subset to avoid R internal issues
    SEXP rn = PROTECT(allocVector(INTSXP, 2)); nprotect++;
    INTEGER(rn)[0] = NA_INTEGER;
    INTEGER(rn)[1] = -new_nrow;
    setAttrib(dt, R_RowNamesSymbol, rn);
  }

  // Clear key and indices
  setAttrib(dt, install("sorted"), R_NilValue);
  setAttrib(dt, install("index"), R_NilValue);

  // Update .internal.selfref to record the new address
  setselfref(dt);

  UNPROTECT(nprotect);
  return dt;
}

// Parallel prefix sum (exclusive scan)
// Two-pass algorithm: first count per thread, then scan, then local prefix sum
static void computePrefixSum(const int *keep, int *dest, int n, int nthreads) {
  if (nthreads == 1 || n < 10000) {
    // Sequential version
    int sum = 0;
    for (int i = 0; i < n; i++) {
      dest[i] = sum;
      sum += keep[i];
    }
    return;
  }

  // Parallel version with two passes
  int *thread_counts = (int *)R_alloc(nthreads, sizeof(int));

  // Pass 1: Count keeps per thread
  #pragma omp parallel num_threads(nthreads)
  {
    const int tid = omp_get_thread_num();
    const int chunk_size = (n + nthreads - 1) / nthreads;
    const int start = tid * chunk_size;
    const int end = (start + chunk_size > n) ? n : start + chunk_size;

    int local_count = 0;
    for (int i = start; i < end; i++) {
      local_count += keep[i];
    }
    thread_counts[tid] = local_count;
  }

  // Sequential scan of thread counts to get offsets
  int *thread_offsets = (int *)R_alloc(nthreads, sizeof(int));
  thread_offsets[0] = 0;
  for (int t = 1; t < nthreads; t++) {
    thread_offsets[t] = thread_offsets[t-1] + thread_counts[t-1];
  }

  // Pass 2: Compute local prefix sum with offset
  #pragma omp parallel num_threads(nthreads)
  {
    const int tid = omp_get_thread_num();
    const int chunk_size = (n + nthreads - 1) / nthreads;
    const int start = tid * chunk_size;
    const int end = (start + chunk_size > n) ? n : start + chunk_size;

    int local_sum = thread_offsets[tid];
    for (int i = start; i < end; i++) {
      dest[i] = local_sum;
      local_sum += keep[i];
    }
  }
}

#define COMPACT(CTYPE, ACCESSOR) {                                     \
  CTYPE *p = ACCESSOR(col);                                            \
  int i = 0;                                                           \
  while (i < old_nrow) {                                               \
    if (!keep[i]) {                                                    \
      i++;                                                             \
      continue;                                                        \
    }                                                                  \
    int run_start = i;                                                 \
    int target_idx = dest[i];                                          \
    while (i < old_nrow && keep[i]) i++;                               \
    size_t run_len = i - run_start;                                    \
    if (target_idx != run_start) {                                     \
      memmove(p + target_idx, p + run_start, run_len * sizeof(CTYPE)); \
    }                                                                  \
  }                                                                    \
}


// Type-specific stream compaction
static void compactVectorRaw(SEXP col, const int *dest, const int *keep,
                             int new_nrow, int old_nrow) {
  switch(TYPEOF(col)) {
    case INTSXP:
    case LGLSXP: {
      COMPACT(int, INTEGER);
      break;
    }
    case REALSXP: {
      COMPACT(double, REAL);
      break;
    }
    case CPLXSXP: {
      COMPACT(Rcomplex, COMPLEX);
      break;
    }
    case RAWSXP: {
      COMPACT(Rbyte, RAW);
      break;
    }
    case STRSXP: {
      for (int i = 0; i < old_nrow; i++) {
        if (keep[i]) SET_STRING_ELT(col, dest[i], STRING_ELT(col, i));
      }
      break;
    }
    case VECSXP: {
      for (int i = 0; i < old_nrow; i++) {
        if (keep[i]) SET_VECTOR_ELT(col, dest[i], VECTOR_ELT(col, i));
      }
      break;
    }
    default:
      error("Unsupported column type %s", type2char(TYPEOF(col)));
  }
}
