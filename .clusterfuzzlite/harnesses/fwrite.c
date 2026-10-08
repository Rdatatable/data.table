/*
 * libFuzzer harness for data.table::fwrite() and fread()/fwrite() round-trips.
 *
 * Splits the input on newlines into a character vector `x`, constructs
 * data.tables with diverse column types (character, integer, numeric,
 * logical, factor, complex, Date/POSIXct, list columns), and exercises
 * src/fwrite.c and src/fwriteR.c:
 *   - Quoting modes (quote = "auto", TRUE, FALSE; qmethod = "double", "escape")
 *   - Numeric / scientific / decimal formatting (dec = ",", scipen)
 *   - Date/time serialization (dateTimeAs = "ISO", "squash", "epoch", "write.csv")
 *   - List-column serialization (sep2)
 *   - BOM, custom EOL, custom NA string, row.names, append
 *   - Streaming zlib gzip compression (compress = "gzip")
 *   - Multi-threaded buffer formatting (nThread = 2L)
 *   - fwrite() -> fread() round-trip parsing
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

#define FUZZ_MAX_INPUT (1024 * 16)
#define FUZZ_MAX_LINES 1000
#define N_CALLS 12

static char *scratch_path;
static SEXP calls[N_CALLS];

static const char *const sources[N_CALLS] = {
    /* 0: Mixed-type data.table with auto-quoting + fread round-trip */
    "function(x) { n <- as.numeric(x); i <- as.integer(n); l <- (i %% 2L == 0L);"
    "  dt <- data.table(s = x, n = n, i = i, l = l);"
    "  fwrite(dt, .fuzz_file); fread(file = .fuzz_file) }",
    /* 1: Force quoting and escape qmethod */
    "function(x) { dt <- data.table(a = x, b = rev(x));"
    "  fwrite(dt, .fuzz_file, quote = TRUE, qmethod = 'escape');"
    "  fwrite(dt, .fuzz_file, quote = TRUE, qmethod = 'double');"
    "  fwrite(dt, .fuzz_file, quote = FALSE) }",
    /* 2: European decimal comma, semicolon sep, custom na and eol */
    "function(x) { n <- as.numeric(x); dt <- data.table(x = x, n = n);"
    "  fwrite(dt, .fuzz_file, sep = ';', dec = ',', na = 'NULL', eol = '\\r\\n');"
    "  fread(file = .fuzz_file, sep = ';', dec = ',', na.strings = 'NULL') }",
    /* 3: Numeric scipen formatting extremes */
    "function(x) { n <- as.numeric(x); dt <- data.table(n = n, m = -n);"
    "  fwrite(dt, .fuzz_file, scipen = 0L);"
    "  fwrite(dt, .fuzz_file, scipen = 999L);"
    "  fwrite(dt, .fuzz_file, scipen = -5L) }",
    /* 4: Factors, logicalAsInt, row.names, BOM */
    "function(x) { f <- factor(x); l <- !is.na(as.numeric(x));"
    "  dt <- data.table(f = f, l = l);"
    "  fwrite(dt, .fuzz_file, logical01 = TRUE, row.names = TRUE, bom = TRUE);"
    "  fread(file = .fuzz_file) }",
    /* 5: Date and POSIXct serialization modes */
    "function(x) { n <- as.numeric(x); n[!is.finite(n)] <- 0;"
    "  d <- as.Date(n %% 50000, origin = '1970-01-01');"
    "  p <- as.POSIXct(n %% 2e9, origin = '1970-01-01', tz = 'UTC');"
    "  dt <- data.table(d = d, p = p);"
    "  fwrite(dt, .fuzz_file, dateTimeAs = 'ISO');"
    "  fwrite(dt, .fuzz_file, dateTimeAs = 'squash');"
    "  fwrite(dt, .fuzz_file, dateTimeAs = 'epoch');"
    "  fwrite(dt, .fuzz_file, dateTimeAs = 'write.csv') }",
    /* 6: List columns with sep2 */
    "function(x) { k <- seq_len(min(50L, length(x)));"
    "  lcol <- lapply(k, function(idx) x[seq_len(min(3L, idx))]);"
    "  dt <- data.table(id = k, items = lcol);"
    "  fwrite(dt, .fuzz_file, sep2 = c('', '|', '')) }",
    /* 7: Complex numbers and bit64/integer64-like raw reals */
    "function(x) { n <- as.numeric(x); z <- complex(real = n, imaginary = rev(n));"
    "  i64 <- n; class(i64) <- 'integer64';"
    "  dt <- data.table(z = z, i64 = i64);"
    "  fwrite(dt, .fuzz_file) }",
    /* 8: Streaming gzip compression in src/fwrite.c */
    "function(x) { dt <- data.table(a = x, b = as.numeric(x));"
    "  fwrite(dt, .fuzz_file, compress = 'gzip') }",
    /* 9: Append mode and col.names = FALSE */
    "function(x) { dt <- data.table(a = x);"
    "  fwrite(dt, .fuzz_file, col.names = TRUE);"
    "  fwrite(dt, .fuzz_file, append = TRUE, col.names = FALSE);"
    "  fread(file = .fuzz_file) }",
    /* 10: UTF-8 and Latin-1 marked strings */
    "function(x) { u <- x; Encoding(u) <- ifelse(validUTF8(u), 'UTF-8', 'unknown');"
    "  l <- x; Encoding(l) <- 'latin1';"
    "  dt <- data.table(u = u, l = l);"
    "  fwrite(dt, .fuzz_file); fread(file = .fuzz_file, encoding = 'UTF-8') }",
    /* 11: Multi-threaded fwrite */
    "function(x) { dt <- data.table(a = rep(x, length.out = 200L),"
    "  b = as.numeric(rep(x, length.out = 200L)));"
    "  fwrite(dt, .fuzz_file, nThread = 2L) }",
};

typedef struct {
    const uint8_t *data;
    size_t size;
} lines_input_t;

static void set_lines(void *data)
{
    lines_input_t *input = data;
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

    for (int i = 0; i < N_CALLS; i++)
        SETCADR(calls[i], lines);
    Rf_unprotect(1);
}

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
    (void)argc;
    (void)argv;
    fuzz_init_r();

    scratch_path = fuzz_scratch_file("fwrite");
    if (scratch_path == NULL)
        abort();

    Rf_defineVar(Rf_install(".fuzz_file"), Rf_mkString(scratch_path), R_GlobalEnv);

    SEXP placeholder;
    Rf_protect(placeholder = Rf_allocVector(STRSXP, 0));
    for (int i = 0; i < N_CALLS; i++) {
        SEXP wrapper;
        Rf_protect(wrapper = fuzz_make_wrapper(sources[i]));
        Rf_protect(calls[i] = Rf_lang2(wrapper, placeholder));
    }
    return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > FUZZ_MAX_INPUT || memchr(data + 1, '\0', size - 1))
        return 0;

    lines_input_t input = { data + 1, size - 1 };
    if (!R_ToplevelExec(set_lines, &input))
        return 0;

    fuzz_eval_silent(calls[data[0] % N_CALLS], R_GlobalEnv);
    return 0;
}
