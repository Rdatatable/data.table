#include "data.table.h"

void benchmark_omp_flag(const int variant, int len, int halt, int th, int *cnt) {
  if (variant == 1) {
    // plain as is now
    bool skip = false;
    #pragma omp parallel for num_threads(th)
    for (int i=0; i<len; i++) {
      if (skip)
        continue;
      int tid = omp_get_thread_num(); 
      cnt[tid]++;
      if (i == halt)
        skip = true;
    }
  } else if (variant == 2) {
    // volatile
    volatile bool skip = false;
    #pragma omp parallel for num_threads(th)
    for (int i=0; i<len; i++) {
      if (skip)
        continue;
      int tid = omp_get_thread_num(); 
      cnt[tid]++;
      if (i == halt)
        skip = true;
    }
  } else if (variant == 3) {
    // volatile and shared
    volatile bool skip = false;
    #pragma omp parallel for num_threads(th) shared(skip)
    for (int i=0; i<len; i++) {
      if (skip)
        continue;
      int tid = omp_get_thread_num(); 
      cnt[tid]++;
      if (i == halt)
        skip = true;
    }
  } else if (variant == 4) {
    // atomic write
    bool skip = false;
    #pragma omp parallel for num_threads(th) shared(skip)
    for (int i=0; i<len; i++) {
      if (skip)
        continue;
      int tid = omp_get_thread_num(); 
      cnt[tid]++;
      if (i == halt) {
        #pragma omp atomic write
        skip = true;
      }
    }
  } else if (variant == 5) {
    // atomic read and atomic write
    bool skip = false;
    #pragma omp parallel for num_threads(th) shared(skip)
    for (int i=0; i<len; i++) {
      bool local_skip;
      #pragma omp atomic read
      local_skip = skip;
      if (local_skip)
          continue;
      int tid = omp_get_thread_num(); 
      cnt[tid]++;
      if (i == halt) {
        #pragma omp atomic write
        skip = true;
      }
    }
  } else if (variant == 6) {
    // reduction-based approach
    bool skip = false;
    #pragma omp parallel for num_threads(th) reduction(||:skip)
    for (int i=0; i<len; i++) {
      if (skip)
        continue;
      int tid = omp_get_thread_num(); 
      cnt[tid]++;
      if (i == halt) {
        skip = true;
      }
    }
  } else if (variant == 7) {
    // cancellation with flag
    bool skip = false;
    #pragma omp parallel for num_threads(th) shared(skip)
    for (int i=0; i<len; i++) {
      #pragma omp cancellation point for
      int tid = omp_get_thread_num(); 
      cnt[tid]++;
      if (i == halt) {
        #pragma omp atomic write
        skip = true;
        #pragma omp cancel for
      }
    }
  }
}

SEXP benchmark_omp_flagR(SEXP variant, SEXP len, SEXP halt, SEXP th) {
  SEXP ans = PROTECT(allocVector(INTSXP, INTEGER_RO(th)[0]));
  for (int i=0; i<INTEGER_RO(th)[0]; i++) INTEGER(ans)[i] = 0;
  benchmark_omp_flag(INTEGER_RO(variant)[0], INTEGER_RO(len)[0], INTEGER_RO(halt)[0]-1, INTEGER_RO(th)[0], INTEGER(ans));
  UNPROTECT(1);
  return(ans);
}
