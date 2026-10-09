/*
 * Generic libFuzzer C driver for data.table R fuzz harnesses.
 *
 * Adapted from r-devel/r-oss-fuzz/harnesses/common.h.
 *
 * How it works:
 *   - Each fuzz target is defined as a plain R script in
 *     .clusterfuzzlite/harnesses/<target>.R that returns a list() of
 *     single-argument closures: list(function(x) { ... }, ...).
 *   - An optional attribute attr(slots, "mode") selects how the fuzzer's
 *     bytes (after the first slot-selector byte data[0]) are staged into R:
 *       * "lines" (default): split bytes on '\n' into a character vector `x`
 *       * "text":            pass bytes as a length-1 character string `x`
 *       * "file":            write raw bytes (including any embedded NULs)
 *                            to a scratch file and pass its path `f`
 *   - At startup (LLVMFuzzerInitialize), this driver initializes embedded R,
 *     loads data.table, evaluates <target>.R once, and pre-constructs the
 *     protected LANGSXP call objects.
 *   - Per input (LLVMFuzzerTestOneInput), it stages the payload under
 *     R_ToplevelExec and evaluates calls[data[0] % n_calls] via
 *     R_tryEvalSilent -- zero per-iteration R parsing overhead.
 */

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

#define FUZZ_MAX_TEXT_INPUT  (1024 * 64)
#define FUZZ_MAX_LINES_INPUT (1024 * 16)
#define FUZZ_MAX_LINES       2000

#ifndef FUZZ_R_MAX_VSIZE
#define FUZZ_R_MAX_VSIZE "1Gb"
#endif

typedef enum {
    FUZZ_MODE_LINES = 0,
    FUZZ_MODE_TEXT  = 1,
    FUZZ_MODE_FILE  = 2
} fuzz_mode_t;

static fuzz_mode_t fuzz_mode = FUZZ_MODE_LINES;
static int n_calls = 0;
static SEXP *calls = NULL;
static SEXP x_text = NULL;
static char *scratch_path = NULL;

static void fuzz_resolve_exe(char *exe_dir, size_t dir_sz,
                             char *exe_name, size_t name_sz)
{
    char exe[PATH_MAX];
    ssize_t len = readlink("/proc/self/exe", exe, sizeof(exe) - 1);
    if (len <= 0) {
        snprintf(exe_dir, dir_sz, ".");
        snprintf(exe_name, name_sz, "unknown");
        return;
    }
    exe[len] = '\0';

    char *slash = strrchr(exe, '/');
    if (slash == NULL) {
        snprintf(exe_dir, dir_sz, ".");
        snprintf(exe_name, name_sz, "%s", exe);
    } else {
        *slash = '\0';
        snprintf(exe_dir, dir_sz, "%s", exe);
        snprintf(exe_name, name_sz, "%s", slash + 1);
    }
}

static char *fuzz_scratch_file(const char *name)
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

static Rboolean fuzz_write_scratch(const char *path, const uint8_t *data,
                                   size_t size)
{
    FILE *fp = fopen(path, "wb");
    if (fp == NULL)
        return FALSE;

    size_t written = size > 0 ? fwrite(data, 1, size, fp) : 0;
    int rc = fclose(fp);
    return written == size && rc == 0;
}

static void fuzz_eval_silent(SEXP call, SEXP env)
{
    int error = 0;
    R_tryEvalSilent(call, env, &error);
}

typedef struct {
    SEXP vec;
    const char *str;
} fuzz_string_data_t;

static void fuzz_do_set_string(void *data)
{
    fuzz_string_data_t *sd = (fuzz_string_data_t *)data;
    SET_STRING_ELT(sd->vec, 0, Rf_mkChar(sd->str));
}

static Rboolean fuzz_set_string(SEXP vec, const char *str)
{
    fuzz_string_data_t sd = { vec, str };
    return R_ToplevelExec(fuzz_do_set_string, &sd);
}

typedef struct {
    const uint8_t *data;
    size_t size;
} lines_input_t;

static void fuzz_do_set_lines(void *data)
{
    lines_input_t *input = (lines_input_t *)data;
    const uint8_t *p = input->data;
    const uint8_t *end = input->data + input->size;

    int n = 1;
    for (const uint8_t *q = p; q < end && n < FUZZ_MAX_LINES; q++) {
        if (*q == '\n')
            n++;
    }

    SEXP lines;
    Rf_protect(lines = Rf_allocVector(STRSXP, n));
    for (int i = 0; i < n; i++) {
        const uint8_t *nl = memchr(p, '\n', (size_t)(end - p));
        if (nl == NULL || i == n - 1)
            nl = end;
        SET_STRING_ELT(lines, i, Rf_mkCharLen((const char *)p, (int)(nl - p)));
        p = nl < end ? nl + 1 : end;
    }

    for (int i = 0; i < n_calls; i++)
        SETCADR(calls[i], lines);
    Rf_unprotect(1);
}

static void fuzz_init_r(const char *exe_dir)
{
    if (getenv("R_HOME") == NULL) {
        char r_home[PATH_MAX];
        snprintf(r_home, sizeof(r_home), "%s/r-install/lib/R", exe_dir);
        setenv("R_HOME", r_home, 1);
    }

    setenv("R_MAX_VSIZE", FUZZ_R_MAX_VSIZE, 0);
    setenv("R_DATATABLE_NUM_THREADS", "1", 0);
    setenv("OMP_NUM_THREADS", "1", 0);

    R_SignalHandlers = 0;

    char *r_argv[] = {"R", "--vanilla", "--no-echo", "--no-restore"};
    int r_argc = sizeof(r_argv) / sizeof(r_argv[0]);
    Rf_initEmbeddedR(r_argc, r_argv);

    /* Suppress warnings globally so warning accumulation cannot overflow. */
    int error = 0;
    SEXP warn_call;
    Rf_protect(warn_call = Rf_lang2(Rf_install("options"),
                                    Rf_ScalarInteger(-1)));
    SET_TAG(CDR(warn_call), Rf_install("warn"));
    R_tryEval(warn_call, R_GlobalEnv, &error);
    Rf_unprotect(1);

    /* Load data.table into R_GlobalEnv; abort immediately if missing. */
    SEXP lib_call;
    Rf_protect(lib_call = Rf_lang2(Rf_install("library"),
                                   Rf_mkString("data.table")));
    R_tryEvalSilent(lib_call, R_GlobalEnv, &error);
    Rf_unprotect(1);
    if (error != 0) {
        fprintf(stderr, "driver.c: failed to load package 'data.table'\n");
        abort();
    }
}

static void fuzz_find_harness_r(const char *exe_dir, const char *target,
                                char *out_path, size_t out_sz)
{
    const char *env_path = getenv("FUZZ_HARNESS_R");
    if (env_path != NULL && access(env_path, R_OK) == 0) {
        snprintf(out_path, out_sz, "%s", env_path);
        return;
    }

    snprintf(out_path, out_sz, "%s/harnesses/%s.R", exe_dir, target);
    if (access(out_path, R_OK) == 0)
        return;

    snprintf(out_path, out_sz, ".clusterfuzzlite/harnesses/%s.R", target);
    if (access(out_path, R_OK) == 0)
        return;

    fprintf(stderr, "driver.c: could not find harness script %s.R\n", target);
    abort();
}

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
    (void)argc;
    (void)argv;

    char exe_dir[PATH_MAX];
    char exe_name[PATH_MAX];
    fuzz_resolve_exe(exe_dir, sizeof(exe_dir), exe_name, sizeof(exe_name));

#ifdef FUZZ_TARGET
    const char *target = FUZZ_TARGET;
#else
    const char *target = exe_name;
#endif

    fuzz_init_r(exe_dir);

    scratch_path = fuzz_scratch_file(target);
    if (scratch_path == NULL)
        abort();
    Rf_defineVar(Rf_install(".fuzz_file"), Rf_mkString(scratch_path), R_GlobalEnv);

    char r_script[PATH_MAX];
    fuzz_find_harness_r(exe_dir, target, r_script, sizeof(r_script));

    /* Evaluate source(r_script)$value to obtain the list of slot closures. */
    int error = 0;
    SEXP src_call, src_res, slots;
    Rf_protect(src_call = Rf_lang2(Rf_install("source"), Rf_mkString(r_script)));
    Rf_protect(src_res = R_tryEval(src_call, R_GlobalEnv, &error));
    if (error != 0 || TYPEOF(src_res) != VECSXP || XLENGTH(src_res) < 1) {
        fprintf(stderr, "driver.c: failed to source %s\n", r_script);
        abort();
    }
    slots = VECTOR_ELT(src_res, 0);
    if (TYPEOF(slots) != VECSXP || XLENGTH(slots) < 1) {
        fprintf(stderr, "driver.c: %s must return a non-empty list of functions\n",
                r_script);
        abort();
    }

    /* Inspect optional attr(slots, "mode"): "lines" (default), "text", "file". */
    SEXP mode_attr = Rf_getAttrib(slots, Rf_install("mode"));
    if (TYPEOF(mode_attr) == STRSXP && XLENGTH(mode_attr) == 1) {
        const char *m = CHAR(STRING_ELT(mode_attr, 0));
        if (strcmp(m, "text") == 0)
            fuzz_mode = FUZZ_MODE_TEXT;
        else if (strcmp(m, "file") == 0)
            fuzz_mode = FUZZ_MODE_FILE;
        else if (strcmp(m, "lines") == 0)
            fuzz_mode = FUZZ_MODE_LINES;
        else {
            fprintf(stderr, "driver.c: unknown mode '%s' in %s\n", m, r_script);
            abort();
        }
    }

    n_calls = (int)XLENGTH(slots);
    calls = (SEXP *)calloc((size_t)n_calls, sizeof(SEXP));
    if (calls == NULL)
        abort();

    SEXP arg_holder;
    if (fuzz_mode == FUZZ_MODE_TEXT) {
        x_text = Rf_allocVector(STRSXP, 1);
        R_PreserveObject(x_text);
        arg_holder = x_text;
    } else if (fuzz_mode == FUZZ_MODE_FILE) {
        arg_holder = Rf_mkString(scratch_path);
        R_PreserveObject(arg_holder);
    } else {
        arg_holder = Rf_allocVector(STRSXP, 0);
        R_PreserveObject(arg_holder);
    }

    for (int i = 0; i < n_calls; i++) {
        SEXP fn = VECTOR_ELT(slots, i);
        if (TYPEOF(fn) != CLOSXP) {
            fprintf(stderr, "driver.c: slot %d in %s is not a function\n",
                    i + 1, r_script);
            abort();
        }
        calls[i] = Rf_lang2(fn, arg_holder);
        R_PreserveObject(calls[i]);
    }

    Rf_unprotect(2); /* src_call, src_res */

    /* Warmup: prime dispatch and internal state on a minimal input. */
    if (fuzz_mode == FUZZ_MODE_TEXT) {
        SET_STRING_ELT(x_text, 0, Rf_mkChar("a,b,c\n1,2.5,x\n3,4.0,y\n"));
        for (int i = 0; i < n_calls; i++)
            fuzz_eval_silent(calls[i], R_GlobalEnv);
    }

    return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2)
        return 0;

    const uint8_t *payload = data + 1;
    size_t payload_size = size - 1;
    int slot = data[0] % n_calls;

    if (fuzz_mode == FUZZ_MODE_TEXT) {
        if (size > FUZZ_MAX_TEXT_INPUT || memchr(payload, '\0', payload_size))
            return 0;
        char buffer[FUZZ_MAX_TEXT_INPUT];
        memcpy(buffer, payload, payload_size);
        buffer[payload_size] = '\0';
        if (!fuzz_set_string(x_text, buffer))
            return 0;
    } else if (fuzz_mode == FUZZ_MODE_FILE) {
        if (size > FUZZ_MAX_TEXT_INPUT)
            return 0;
        /* Reject archive magic bytes so fread(file=) always takes the direct
         * C mmap path in src/fread.c rather than R archive extraction. */
        if (payload_size >= 2) {
            if ((payload[0] == 'P' && payload[1] == 'K') ||
                (payload[0] == 0x1f && payload[1] == 0x8b) ||
                (payload[0] == 'B' && payload[1] == 'Z'))
                return 0;
        }
        if (!fuzz_write_scratch(scratch_path, payload, payload_size))
            return 0;
    } else {
        if (size > FUZZ_MAX_LINES_INPUT || memchr(payload, '\0', payload_size))
            return 0;
        lines_input_t input = { payload, payload_size };
        if (!R_ToplevelExec(fuzz_do_set_lines, &input))
            return 0;
    }

    fuzz_eval_silent(calls[slot], R_GlobalEnv);
    return 0;
}
