#ifdef _OPENMP
  #include <omp.h>
#else
  // for machines with compilers void of openmp support
  #define omp_get_num_threads() 1
  #define omp_get_thread_num() 0
  #define omp_get_max_threads() 1
  #define omp_get_thread_limit() 1
  #define omp_set_nested(a)   // empty statement to remove the call
#endif

