#include "data.table.h"
#ifdef _OPENMP
#include <pthread.h>
#endif
#include <errno.h>     // errno
#include <ctype.h>     // isspace

static int  DTthreads = -1;   // Never read directly hence static; use getDTthreads(n, /*throttle=*/0|1). -1 so we know for sure initDTthreads() ran and set it >= 1.
static int  DTthrottle = -1;  // Thread 1 is assigned DTthrottle iterations before a 2nd thread is utilized; #4484.
static bool RestoreAfterFork = true;  // see #2885 in v1.12.0

static int getIntEnv(const char *name, int def)
{
  const char *val = getenv(name);
  if (val==NULL) return def;
  size_t nchar = strlen(val);
  if (nchar==0) return def;
  char *end;
  errno = 0;
  long int ans = strtol(val, &end, 10);  // ignores leading whitespace. If it fully consumed the string, *end=='\0' and isspace('\0')==false
  while (isspace(*end)) end++;  // ignore trailing whitespace
  if (errno || (size_t)(end-val)!=nchar || ans<1 || ans>INT_MAX) {
    warning(_("Ignoring invalid %s==\"%s\". Not an integer >= 1. Please remove any characters that are not a digit [0-9]. See ?data.table::setDTthreads."), name, val);
    return def;
  }
  return (int)ans;
}

static inline int imin(int a, int b) { return a < b ? a : b; }
static inline int imax(int a, int b) { return a > b ? a : b; }

void initDTthreads() {
  // called at package startup from init.c
  // also called by setDTthreads(threads=NULL) (default) to reread environment variables; see setDTthreads below
  // No verbosity here in this setter. Verbosity is in getDTthreads(verbose=TRUE)
  int ans = getIntEnv("R_DATATABLE_NUM_THREADS", INT_MIN);
  if (ans>=1) {
    ans = imin(ans, omp_get_num_procs());  // num_procs is a hard limit; user cannot achieve more. ifndef _OPENMP then myomp.h defines this to be 1
  } else {
    // Only when R_DATATABLE_NUM_THREADS is unset (or <=0) do we use PROCS_PERCENT; #4514
    int perc = getIntEnv("R_DATATABLE_NUM_PROCS_PERCENT", 50); // use "NUM_PROCS" to use the same name as the OpenMP function this uses
    // 50% of logical CPUs by default; half of 8 is 4 on laptop with 4 cores. Leaves plenty of room for other processes: #3395 & #3298
    if (perc<=1 || perc>100) {
      warning(_("Ignoring invalid R_DATATABLE_NUM_PROCS_PERCENT==%d. If used it must be an integer between 2 and 100. Default is 50. See ?setDTtheads."), perc);
      // not allowing 1 is to catch attempts to use 1 or 1.0 to represent 100%.
      perc = 50;
    }
    ans = imax(omp_get_num_procs()*perc/100, 1); // imax for when formula would result in 0.
  }
  ans = imin(ans, omp_get_thread_limit());  // honors OMP_THREAD_LIMIT when OpenMP started; e.g. CRAN sets this to 2. Often INT_MAX meaning unlimited/unset
  ans = imin(ans, omp_get_max_threads());   // honors OMP_NUM_THREADS when OpenMP started, plus reflects any omp_set_* calls made since
  // max_threads() -vs- num_procs(): https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/302866
  ans = imin(ans, getIntEnv("OMP_THREAD_LIMIT", INT_MAX));  // user might expect `Sys.setenv(OMP_THREAD_LIMIT=2);setDTthreads()` to work. Satisfy this
  ans = imin(ans, getIntEnv("OMP_NUM_THREADS", INT_MAX));   //   expectation by reading them again now. OpenMP just reads them on startup (quite reasonably)
  ans = imax(ans, 1);  // just in case omp_get_* returned <=0 for any reason, or the env variables above are set <=0
  DTthreads = ans;
  DTthrottle = imax(1, getIntEnv("R_DATATABLE_THROTTLE", 1024)); // 2nd thread is used only when n>1024, 3rd thread when n>2048, etc
}

int getDTthreads(const int64_t n, const bool throttle) {
  // this is the main getter used by all parallel regions; they specify num_threads(n, true|false).
  // Keep this light, simple and robust. initDTthreads() ensures 1 <= DTthreads <= omp_get_num_proc()
  // throttle introduced in 1.12.10 (see NEWS item); #4484
  // throttle==true  : a number of iterations per thread (DTthrottle) is applied before a second thread is utilized 
  // throttle==false : parallel region is already pre-chunked such as in fread; e.g. two batches intended for two threads
  if (n<1) return 1; // 0 or negative could be deliberate in calling code for edge cases where loop is not intended to run at all
  int64_t ans = throttle ? 1+(n-1)/DTthrottle :  // 1 thread for n<=1024, 2 thread for n<=2048, etc
                           n;                    // don't use 20 threads for just one or two batches
  return ans>=DTthreads ? DTthreads : (int)ans;  // apply limit in static local DTthreads saved there by initDTthreads() and setDTthreads()
}

static const char *mygetenv(const char *name, const char *unset) {
  const char *ans = getenv(name);
  return (ans==NULL || ans[0]=='\0') ? unset : ans;
}

SEXP getDTthreads_R(SEXP verbose) {
  if (!isLogical(verbose) || LENGTH(verbose)!=1 || INTEGER(verbose)[0]==NA_LOGICAL) error(_("'verbose' must be TRUE or FALSE"));
  if (LOGICAL(verbose)[0]) {
    #ifndef _OPENMP
      Rprintf(_("This installation of data.table has not been compiled with OpenMP support.\n"));
    #endif
    // this output is captured, paste0(collapse="; ")'d, and placed at the end of test.data.table() for display in the last 13 lines of CRAN check logs
    // it is also printed at the start of test.data.table() so that we can trace any Killed events on CRAN before the end is reached
    // this is printed verbatim (e.g. without using data.table to format the output) in case there is a problem even with simple data.table creation/printing
    Rprintf(_("  omp_get_num_procs()            %d\n"), omp_get_num_procs());
    Rprintf(_("  R_DATATABLE_NUM_PROCS_PERCENT  %s\n"), mygetenv("R_DATATABLE_NUM_PROCS_PERCENT", "unset (default 50)"));
    Rprintf(_("  R_DATATABLE_NUM_THREADS        %s\n"), mygetenv("R_DATATABLE_NUM_THREADS", "unset"));
    Rprintf(_("  R_DATATABLE_THROTTLE           %s\n"), mygetenv("R_DATATABLE_THROTTLE", "unset (default 1024)"));
    Rprintf(_("  omp_get_thread_limit()         %d\n"), omp_get_thread_limit());
    Rprintf(_("  omp_get_max_threads()          %d\n"), omp_get_max_threads());
    Rprintf(_("  OMP_THREAD_LIMIT               %s\n"), mygetenv("OMP_THREAD_LIMIT", "unset"));  // CRAN sets to 2
    Rprintf(_("  OMP_NUM_THREADS                %s\n"), mygetenv("OMP_NUM_THREADS", "unset"));
    Rprintf(_("  RestoreAfterFork               %s\n"), RestoreAfterFork ? "true" : "false");
    Rprintf(_("  data.table is using %d threads with throttle==%d. See ?setDTthreads.\n"), getDTthreads(INT_MAX, false), DTthrottle);
  }
  return ScalarInteger(getDTthreads(INT_MAX, false));
}

SEXP setDTthreads(SEXP threads, SEXP restore_after_fork, SEXP percent, SEXP throttle) {
  if (!isNull(restore_after_fork)) {
    if (!isLogical(restore_after_fork) || LOGICAL(restore_after_fork)[0]==NA_LOGICAL) {
      error(_("restore_after_fork= must be TRUE, FALSE, or NULL (default). getDTthreads(verbose=TRUE) reports the current setting.\n"));
    }
    RestoreAfterFork = LOGICAL(restore_after_fork)[0];  // # nocov
  }
  if (length(throttle)) {
    if (!isInteger(throttle) || LENGTH(throttle)!=1 || INTEGER(throttle)[0]<1)
      error(_("'throttle' must be a single number, non-NA, and >=1"));
    DTthrottle = INTEGER(throttle)[0];
  }
  int old = DTthreads;
  if (!length(threads) && !length(throttle)) {
    initDTthreads();
    // Rerun exactly the same function used on startup (re-reads env variables); this is now default setDTthreads() behavior from 1.12.2
    // Allows robust testing of environment variables using Sys.setenv() to experiment.
    // Default  is now (as from 1.12.2) threads=NULL which re-reads environment variables.
    // If a CPU has been unplugged (high end servers allow live hardware replacement) then omp_get_num_procs() will
    // reflect that and a call to setDTthreads(threads=NULL) will update DTthreads.
  } else if (length(threads)) {
    int n=0;
    if (length(threads)!=1 || !isInteger(threads) || (n=INTEGER(threads)[0]) < 0) {  // <0 catches NA too since NA is negative (INT_MIN)
      error(_("threads= must be either NULL or a single number >= 0. See ?setDTthreads."));
    }
    int num_procs = imax(omp_get_num_procs(), 1); // max just in case omp_get_num_procs() returns <= 0 (perhaps error, or unsupported)
    if (!isLogical(percent) || length(percent)!=1 || LOGICAL(percent)[0]==NA_LOGICAL) {
      error(_("Internal error: percent= must be TRUE or FALSE at C level"));  // # nocov
    }
    if (LOGICAL(percent)[0]) {
      if (n<2 || n>100) error(_("Internal error: threads==%d should be between 2 and 100 (percent=TRUE at C level)."), n);  // # nocov
      n = num_procs*n/100;  // if 0 it will be reset to 1 in the imax() below
    } else {
      if (n==0 || n>num_procs) n = num_procs; // setDTthreads(0) == setDTthread(percent=100); i.e. use all logical CPUs (the default in 1.12.0 and before, from 1.12.2 it's 50%)
    }
    n = imin(n, omp_get_thread_limit());  // can't think why this might be different from its value on startup, but call it just in case
    n = imin(n, getIntEnv("OMP_THREAD_LIMIT", INT_MAX));  // user might have called Sys.setenv(OMP_THREAD_LIMIT=) since startup and expect setDTthreads to respect it
    DTthreads = imax(n, 1);  // imax just in case
    // Do not call omp_set_num_threads() here. Any calls to omp_set_num_threads() affect other
    // packages and R itself too which has some OpenMP usage. Instead we set our own DTthreads
    // static variable and read that from getDTthreads(n, throttle).
    // All parallel regions should include num_threads(getDTthreads(n, true|false)) and this is ensured via
    // a grep in CRAN_Release.cmd.
  }
  return ScalarInteger(old);
}

/*
  Automatically drop down to 1 thread when called from parallel package (e.g. mclapply) to avoid
  deadlock when data.table is used from within parallel::mclapply; #1745 and #1727.
  GNU OpenMP seems ok with just setting DTthreads to 1 which limits the next parallel region
  if data.table is used within the fork'd proceess. This is tested by test 1705.

  From v1.12.0 we're trying again to RestoreAferFork (#2285) with optional-off due to success
  reported by Ken Run and Mark Klik in fst#110 and fst#112. We had tried that before but had
  experienced problems likely on Intel's OpenMP only (Mac).

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

