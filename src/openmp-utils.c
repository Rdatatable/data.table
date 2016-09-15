#include "data.table.h"
#ifdef _OPENMP
    #include <omp.h>
#endif

// Goals:
// 1) By default use all CPU for end-user convenience in most usage scenarios.
// 2) But not on CRAN - two threads max is policy
// 3) And not if user doesn't want to :
//    i) Respect env variable OMP_NUM_THREADS (which just calls (ii) on startup ...)
//    ii) Respect omp_set_num_threads() 
//    iii) Provide way to restrict data.table only independently of base R and other packages using openMP
// 4) Avoid user needing to remember to unset this control after their use of data.table

static int DTthreads = 0;   // 0 means use omp_get_max_threads() default, otherwise use this many threads
                            // never read directly, hence static. Go via getDTthreads()

SEXP setDTthreads(SEXP threads) {
    if (!isInteger(threads) || length(threads) != 1 || INTEGER(threads)[0] < 0) // catches NA too since NA is -ve
        error("Argument to setDTthreads must be a single integer >= 0. Default 0 is recommended to use all CPU.");
        
    // do not call omp_set_num_threads() here as that affects other openMP packages
    // and base R as well potentially. 
    DTthreads = INTEGER(threads)[0];

    return (R_NilValue);
}

int getDTthreads() {
#ifdef _OPENMP
    return DTthreads == 0 ? omp_get_max_threads() : DTthreads;  
#else
    return 1;
#endif
}

SEXP getDTthreads_R() {
    return ScalarInteger(getDTthreads());
}

