/*
 * Shared helpers for data.table fuzzing harnesses (libFuzzer).
 *
 * Adapted from r-devel/r-oss-fuzz/harnesses/common.h.
 *
 * Provides:
 *   fuzz_suppress_warnings() - Suppress R warnings to avoid buffer overflow
 *   fuzz_init_r()            - Full R + data.table initialization sequence
 *   fuzz_set_string()        - Stage a string input under a toplevel context
 *   fuzz_set_string_ce()     - Stage a string with an explicit encoding mark
 *   fuzz_set_raw_arg()       - Stage a raw-vector input likewise
 *   fuzz_make_wrapper()      - Parse an R closure in R_GlobalEnv for a call slot
 *   fuzz_scratch_file()      - Per-process temp file for path-only entry points
 *   fuzz_write_scratch()     - Rewrite that file with the current input
 */

#ifndef FUZZ_COMMON_H
#define FUZZ_COMMON_H

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define R_NO_REMAP 1
#include <Rembedded.h>
#include <Rinternals.h>
#include <Rinterface.h>   /* R_SignalHandlers */
#include <R_ext/Parse.h>

/*
 * Suppress R warnings globally.
 *
 * Many R/data.table functions emit warnings on malformed input (e.g.,
 * fread bumped column types, stopped early, or coercion introduced NAs).
 * In persistent fuzzing, these accumulate and can overflow R's warning
 * buffer. Setting warn = -1 suppresses all warnings.
 */
static void fuzz_suppress_warnings(void)
{
    int error = 0;
    SEXP warn_call;
    Rf_protect(warn_call = Rf_lang2(Rf_install("options"),
                                    Rf_ScalarInteger(-1)));
    SET_TAG(CDR(warn_call), Rf_install("warn"));
    R_tryEval(warn_call, R_GlobalEnv, &error);
    Rf_unprotect(1);
}

/*
 * Set R_HOME based on the fuzzer binary's location.
 *
 * OSS-Fuzz / ClusterFuzzLite places everything in $OUT. The build script
 * copies R + installed data.table to $OUT/r-install, so R_HOME is at
 * $OUT/r-install/lib/R. Derived from /proc/self/exe so it works regardless
 * of mount path.
 */
static void fuzz_set_r_home(void)
{
    /* Skip if R_HOME is already set (e.g., for local testing) */
    if (getenv("R_HOME") != NULL)
        return;

    char exe[PATH_MAX];
    ssize_t len = readlink("/proc/self/exe", exe, sizeof(exe) - 1);
    if (len <= 0)
        return;
    exe[len] = '\0';

    char *slash = strrchr(exe, '/');
    if (slash == NULL)
        return;
    *slash = '\0';

    char r_home[PATH_MAX];
    snprintf(r_home, sizeof(r_home), "%s/r-install/lib/R", exe);
    setenv("R_HOME", r_home, 1);
}

/*
 * Cap R's vector heap.
 *
 * Hostile input can ask for large allocations. R enforces R_MAX_VSIZE in
 * allocVector and signals an ordinary R error ("vector memory limit of N
 * reached"), which R_ToplevelExec / R_tryEvalSilent catches cleanly so the
 * iteration is discarded and fuzzing continues.
 */
#ifndef FUZZ_R_MAX_VSIZE
#define FUZZ_R_MAX_VSIZE "1Gb"
#endif

static void fuzz_set_vsize_limit(void)
{
    setenv("R_MAX_VSIZE", FUZZ_R_MAX_VSIZE, 0);
}

/*
 * Default data.table to single-threaded execution for deterministic
 * coverage guidance unless overridden in the environment or explicitly
 * toggled inside a specific harness slot.
 */
static void fuzz_set_dt_threads(void)
{
    setenv("R_DATATABLE_NUM_THREADS", "1", 0);
    setenv("OMP_NUM_THREADS", "1", 0);
}

/*
 * Load data.table into R_GlobalEnv at startup.
 *
 * Fails hard via abort() if data.table cannot be loaded so a broken build
 * never silently fuzzes nothing.
 */
static void fuzz_load_datatable(void)
{
    int error = 0;
    SEXP call;
    Rf_protect(call = Rf_lang2(Rf_install("library"),
                               Rf_mkString("data.table")));
    R_tryEvalSilent(call, R_GlobalEnv, &error);
    Rf_unprotect(1);
    if (error != 0) {
        fprintf(stderr, "fuzz_init_r: failed to load package 'data.table'\n");
        abort();
    }
}

/*
 * Initialize embedded R and data.table for fuzzing.
 *
 * Call once from LLVMFuzzerInitialize:
 *   1. Set R_HOME relative to the fuzzer binary
 *   2. Cap R's vector heap (must precede Rf_initEmbeddedR)
 *   3. Pin OpenMP / data.table thread count to 1 by default
 *   4. Initialize embedded R without R's signal handlers
 *   5. Suppress warnings
 *   6. Load data.table
 */
static void fuzz_init_r(void)
{
    fuzz_set_r_home();
    fuzz_set_vsize_limit();
    fuzz_set_dt_threads();

    R_SignalHandlers = 0;

    char *r_argv[] = {"R", "--vanilla", "--no-echo", "--no-restore"};
    int r_argc = sizeof(r_argv) / sizeof(r_argv[0]);
    Rf_initEmbeddedR(r_argc, r_argv);

    fuzz_suppress_warnings();
    fuzz_load_datatable();
}

/* Evaluate malformed-input-heavy targets without printing each expected
 * R error. R_tryEvalSilent installs its own top-level error context. */
static void fuzz_eval_silent(SEXP call, SEXP env)
{
    int error = 0;
    R_tryEvalSilent(call, env, &error);
}

/*
 * Stage per-iteration inputs under a toplevel context.
 *
 * Rf_mkChar and Rf_allocVector can themselves signal the vector-limit
 * error. Wrapping input staging in R_ToplevelExec catches any longjmp.
 */
typedef struct {
    SEXP vec;
    const char *str;
} fuzz_string_data_t;

static void fuzz_do_set_string(void *data)
{
    fuzz_string_data_t *sd = (fuzz_string_data_t *)data;
    SET_STRING_ELT(sd->vec, 0, Rf_mkChar(sd->str));
}

static inline Rboolean fuzz_set_string(SEXP vec, const char *str)
{
    fuzz_string_data_t sd = { vec, str };
    return R_ToplevelExec(fuzz_do_set_string, &sd);
}

/*
 * Parse and evaluate one R function literal in R_GlobalEnv, once, at
 * initialization.
 *
 * Evaluating in R_GlobalEnv (after library(data.table)) ensures that
 * data.table exports, S3 methods ([.data.table), and special symbols
 * (:=, .SD, .N, .I, .GRP, .BY) resolve normally inside the closure.
 */
static inline SEXP fuzz_make_wrapper(const char *source)
{
    ParseStatus status;
    SEXP text, parsed, wrapper;
    Rf_protect(text = Rf_mkString(source));
    Rf_protect(parsed = R_ParseVector(text, -1, &status, R_NilValue));
    if (status != PARSE_OK || XLENGTH(parsed) != 1)
        abort();

    wrapper = Rf_eval(VECTOR_ELT(parsed, 0), R_GlobalEnv);
    Rf_unprotect(2);
    return wrapper;
}

/*
 * fuzz_set_string with an explicit encoding mark.
 */
typedef struct {
    SEXP vec;
    const char *str;
    cetype_t ce;
} fuzz_ce_string_data_t;

static inline void fuzz_do_set_string_ce(void *data)
{
    fuzz_ce_string_data_t *sd = (fuzz_ce_string_data_t *)data;
    SET_STRING_ELT(sd->vec, 0, Rf_mkCharCE(sd->str, sd->ce));
}

static inline Rboolean fuzz_set_string_ce(SEXP vec, const char *str, cetype_t ce)
{
    fuzz_ce_string_data_t sd = { vec, str, ce };
    return R_ToplevelExec(fuzz_do_set_string_ce, &sd);
}

typedef struct {
    SEXP call;
    const uint8_t *data;
    size_t size;
} fuzz_raw_data_t;

static inline void fuzz_do_set_raw(void *data)
{
    fuzz_raw_data_t *rd = (fuzz_raw_data_t *)data;

    SEXP raw = Rf_allocVector(RAWSXP, (R_xlen_t)rd->size);
    SETCADR(rd->call, raw);
    memcpy(RAW(raw), rd->data, rd->size);
}

static inline Rboolean fuzz_set_raw_arg(SEXP call, const uint8_t *data, size_t size)
{
    fuzz_raw_data_t rd = { call, data, size };
    return R_ToplevelExec(fuzz_do_set_raw, &rd);
}

/*
 * Stage the input in a scratch file.
 *
 * Used by targets like fread_file that exercise the mmap/file path of
 * fread() (including embedded NUL bytes, BOMs, and header/EOF handling).
 */
static inline char *fuzz_scratch_file(const char *name)
{
    const char *tmpdir = getenv("TMPDIR");
    if (tmpdir == NULL || *tmpdir == '\0')
        tmpdir = "/tmp";

    char *path = malloc(PATH_MAX);
    if (path == NULL)
        return NULL;
    snprintf(path, PATH_MAX, "%s/fuzz-%s-XXXXXX", tmpdir, name);

    int fd = mkstemp(path);
    if (fd < 0) {
        free(path);
        return NULL;
    }
    close(fd);
    return path;
}

static inline Rboolean fuzz_write_scratch(const char *path, const uint8_t *data,
                                          size_t size)
{
    FILE *fp = fopen(path, "wb");
    if (fp == NULL)
        return FALSE;

    size_t written = size > 0 ? fwrite(data, 1, size, fp) : 0;
    int rc = fclose(fp);
    return written == size && rc == 0;
}

#endif /* FUZZ_COMMON_H */
