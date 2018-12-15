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

static int  DTthreads = 0;             // Never read directly, hence static; always go via getDTthreads().
static bool RestoreAfterFork = true;  // see #2885 in v1.12.0

int getDTthreads() {
#ifdef _OPENMP
  int ans = omp_get_max_threads();
  if (ans>1024) {
    warning("omp_get_max_threads() has returned %d. This is unreasonably large. Applying hard limit of 1024. Please check OpenMP environment variables and other packages using OpenMP to see where this very large number has come from. Try getDTthreads(verbose=TRUE).", ans);
    // to catch INT_MAX for example, which may be the case if user or another package has called omp_set_num_threads(omp_get_thread_limit())
    // 1024 is a reasonable hard limit based on a comfortable margin above the most number of CPUs in one server I have heard about
    ans=1024;
  }
  if (DTthreads>0 && DTthreads<ans) ans = DTthreads;
  if (ans<1) ans=1;
  return ans;
#else
  return 1;
#endif
}

SEXP getDTthreads_R(SEXP verbose) {
  // verbose checked at R level
  if (!isLogical(verbose) || LENGTH(verbose)!=1 || INTEGER(verbose)[0]==NA_LOGICAL) error("'verbose' must be TRUE or FALSE");
  if (LOGICAL(verbose)[0]) {
    Rprintf("omp_get_max_threads() = %d\n", omp_get_max_threads());
    Rprintf("omp_get_thread_limit() = %d\n", omp_get_thread_limit()); // can be INT_MAX meaning unlimited
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
    error("threads= must be a single integer >= 0. Default 0 is recommended to use all CPU.");
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

