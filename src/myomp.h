
// Compatibility define for LLVM builds of libomp.
#ifdef KMP_VERSION_BUILD
# ifndef _OPENMP
#  define _OPENMP KMP_VERSION_BUILD
# endif
#endif

#ifdef _OPENMP
  #include <omp.h>
  #if _OPENMP >= 201511
    #define monotonic_dynamic monotonic:dynamic // #4786
  #else
    #define monotonic_dynamic dynamic
  #endif
  #define MY_OPENMP              _OPENMP
  // for use in error messages (e.g. fsort.c; #4786) to save an #ifdef each time
  // initially chose OMP_VERSION but figured OpenMP might define that in future, so picked MY_ prefix
#else
  // for machines with compilers void of openmp support
  #define omp_get_num_threads()  1
  #define omp_get_thread_num()   0
  #define omp_get_max_threads()  1
  #define omp_get_thread_limit() 1
  #define omp_get_num_procs()    1
  #define omp_get_wtime()        0
  #define MY_OPENMP              0
#endif
