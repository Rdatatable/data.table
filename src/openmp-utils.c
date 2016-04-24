#include "data.table.h"
#ifdef _OPENMP
    #include <omp.h>
#endif

SEXP setThreads(SEXP threads) {
    if (!isInteger(threads) || length(threads) != 1 || INTEGER(threads)[0] <= 0)
        error("Argument 'threads' to setThreads must be a length=1 positive integer.");
#ifdef _OPENMP
    omp_set_num_threads(INTEGER(threads)[0]);
#endif
    return (R_NilValue);
}

SEXP getThreads() {
    SEXP ans = PROTECT(ScalarInteger(1));
#ifdef _OPENMP
    INTEGER(ans)[0] = omp_get_max_threads();
#else
    INTEGER(ans)[0] = 1;
#endif
    UNPROTECT(1);
    return(ans);
}
