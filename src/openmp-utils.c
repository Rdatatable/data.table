#include "data.table.h"
#ifdef _OPENMP
#include <pthread.h>
#endif

/* GOALS:
* 1) By default use all CPU for end-user convenience in most usage scenarios.
* 2) But not on CRAN - two threads max is policy
* 3) And not if user doesn't want to:
*    i) Respect env variable OMP_NUM_THREADS (which just calls (ii) on startup)
*    ii) Respect omp_set_num_threads()
*    iii) Provide way to restrict data.table only independently of base R and
*         other packages using openMP
* 4) Avoid user needing to remember to unset this control after their use of data.table
* 5) Automatically drop down to 1 thread when called from parallel package (e.g. mclapply) to
*    avoid the deadlock/hang (#1745 and #1727) and return to prior state afterwards.
*/

static int  DTthreads = 0;            // Never read directly, hence static; always go via getDTthreads().
static bool RestoreAfterFork = true;  // see #2885 in v1.12.0

int getDTthreads() {
#ifdef _OPENMP
  int ans=DTthreads;
  if (DTthreads==0) {  // 0 represents default
    if (omp_get_max_threads()/*OMP_NUM_THREADS*/ < omp_get_num_procs()) {
      // max_threads() -vs- num_procs(): https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/302866
      // allow user to choose number of threads to use by setting OMP_NUM_THREADS env variable
      // e.g. more than 50% of num_procs() without needing to call setDTthreads() at R level (useful on prod server)
      ans = omp_get_max_threads();
    } else {
      ans = omp_get_num_procs()/2;  // normal recommended default; e.g. half of 8 is 4 on laptop with 4 cores. Leaves plenty of room for other processes: #3395 & #3298
      ans = MIN(ans, omp_get_thread_limit()); // OMP_THREAD_LIMIT; CRAN sets this to 2; #3300. Often INT_MAX meaning unlimited/unset
    }
  } else {
    int limit = MIN(omp_get_num_procs(),      // data.table policy is to never allow > num_procs() since we know our algos won't benefit
                    omp_get_thread_limit());  
    ans = MIN(ans, limit);
  }
  return MAX(1,ans);
#else
  return 1;
#endif
}

SEXP getDTthreads_R(SEXP verbose) {
  // verbose checked at R level
  if (!isLogical(verbose) || LENGTH(verbose)!=1 || INTEGER(verbose)[0]==NA_LOGICAL) error("'verbose' must be TRUE or FALSE");
  if (LOGICAL(verbose)[0]) {
    Rprintf("omp_get_num_procs() = %d\n", omp_get_num_procs());       // not affected by env variables afaik (which we rely on)
    Rprintf("omp_get_max_threads() = %d\n", omp_get_max_threads());   // OMP_NUM_THREADS
    Rprintf("omp_get_thread_limit() = %d\n", omp_get_thread_limit()); // OMP_THREAD_LIMIT; often INT_MAX meaning unlimited/unset
    Rprintf("DTthreads = %d\n", DTthreads);
    Rprintf("RestoreAfterFork = %s\n", RestoreAfterFork ? "true" : "false");
    #ifndef _OPENMP
      Rprintf("This installation of data.table has not been compiled with OpenMP support.\n");
      // the omp_ functions used above are defined in myomp.h to be 1 in this case
    #endif
  }
  return ScalarInteger(getDTthreads());
}

SEXP setDTthreads(SEXP threads, SEXP restore_after_fork) {
  if (!isInteger(threads) || length(threads) != 1 || INTEGER(threads)[0] < 0) {
    // catches NA too since NA is -ve
    error("threads= must be a single integer >= 0. Default 0 is recommended to use half of the logical CPUs.");
  }
  if (!isNull(restore_after_fork) && !(isLogical(restore_after_fork) && LOGICAL(restore_after_fork)[0]>=0 /*not NA*/)) {
    error("restore_after_fork= must be TRUE or FALSE. The default NULL means leave the current setting unchanged. getDTthreads(verbose=TRUE) reports the current setting.\n");
  }
  int old = DTthreads;
  DTthreads = INTEGER(threads)[0];
  // Do not call omp_set_num_threads() here, and particularly not to omp_get_thread_limit()
  // which is likely INT_MAX (unlimited). Any calls to omp_set_num_threads() affect other
  // packages and R itself too which has some OpenMP usage. Instead we set our own DTthreads
  // static global variable, read that from getDTthreads() and ensure num_threads(getDTthreads())
  // directive is always present via the grep in CRAN_Release.cmd.

  if (!isNull(restore_after_fork)) RestoreAfterFork = LOGICAL(restore_after_fork)[0];

  return ScalarInteger(old);
}

/*
  Auto avoid deadlock when data.table is used from within parallel::mclapply
  GNU OpenMP seems ok with just setting DTthreads to 1 which limits the next parallel region
  if data.table is used within the fork'd proceess. This is tested by test 1705.

  We used to have an after_fork() callback too, to return to multi-threaded mode after parallel's
  fork completes. But now in an attempt to alleviate problems propogating (likely Intel's OpenMP only)
  we now leave data.table in single-threaded mode after parallel's fork. User can call setDTthreads(0)
  to return to multi-threaded as we do in tests on Linux.
  [UPDATE] in v1.12.0 we're trying again to RestoreAferFork (#2285) with optional-off due to success
           reported by Ken Run and Mark Klik in fst#110 and fst#112

  DO NOT call omp_set_num_threads(1) inside when_fork()!! That causes a different crash/hang on MacOS
  upon mclapply's fork even if data.table is merely loaded and neither used yet in the session nor by
  what mclapply is calling. Even when passing on CRAN's MacOS all-OK. As discovered by several MacOS
  and RStudio users here when 1.10.4-2 went to CRAN :
     https://github.com/Rdatatable/data.table/issues/2418
  v1.10.4-3 removed the call to omp_set_num_threads().
  We do not know why calling (mere) omp_set_num_threads(1) in the when_fork() would cause a problem
  on MacOS.
*/

static int pre_fork_DTthreads = 0;

void when_fork() {
  pre_fork_DTthreads = DTthreads;
  DTthreads = 1;
}

void after_fork() {
  if (RestoreAfterFork) DTthreads = pre_fork_DTthreads;
}

void avoid_openmp_hang_within_fork() {
  // Called once on loading data.table from init.c
#ifdef _OPENMP
  pthread_atfork(&when_fork, &after_fork, NULL);
#endif
}

