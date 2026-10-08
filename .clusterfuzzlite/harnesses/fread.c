/*
 * libFuzzer harness for data.table::fread() in-memory text parser.
 *
 * Feeds fuzzed delimited text to fread(text = x, ...) across 20 call
 * configurations exercising the C parser state machine in src/fread.c
 * and src/freadR.c:
 *   - Separator / quote / header / skip auto-detection
 *   - Explicit sep (',', '\t', ' ', NULL single-column)
 *   - Quote rules ('"', '\'', '') and strip.white
 *   - Ragged rows (fill = TRUE, fill = Inf) and blank.lines.skip
 *   - Custom dec = ',', na.strings, and comment.char
 *   - Type inference and out-of-sample type bumping (logical01, logicalYN,
 *     keepLeadingZeros, integer64, hex, ISO8601/scientific doubles)
 *   - Explicit colClasses, select, drop, check.names, key, index
 *   - UTF-8 and Latin-1 string encodings
 *   - Multi-threaded chunked jump-point boundary parsing (nThread = 2L)
 *
 * The first input byte selects the configuration; the remaining bytes
 * form the NUL-terminated text string passed to fread(text = x, ...).
 */

#include <stdint.h>
#include <string.h>

#include "common.h"

#define FUZZ_MAX_INPUT (1024 * 64)
#define N_CALLS 20

static const char *const sources[N_CALLS] = {
    /* 0: Full auto-detection */
    "function(x) fread(text = x)",
    /* 1: Explicit comma separator and header */
    "function(x) fread(text = x, sep = \",\", header = TRUE)",
    /* 2: Tab-separated, unquoted */
    "function(x) fread(text = x, sep = \"\\t\", quote = \"\")",
    /* 3: Whitespace-separated with strip.white */
    "function(x) fread(text = x, sep = \" \", strip.white = TRUE)",
    /* 4: Single-column line mode (sep = NULL) */
    "function(x) fread(text = x, sep = NULL, header = FALSE)",
    /* 5: Single-quote delimiter, preserve whitespace */
    "function(x) fread(text = x, quote = \"'\", strip.white = FALSE)",
    /* 6: Ragged rows with fill = TRUE and blank.lines.skip = TRUE */
    "function(x) fread(text = x, fill = TRUE, blank.lines.skip = TRUE)",
    /* 7: Unbounded ragged column growth (fill = Inf) */
    "function(x) fread(text = x, fill = Inf, header = FALSE)",
    /* 8: European decimal comma and semicolon separator */
    "function(x) fread(text = x, dec = \",\", sep = \";\")",
    /* 9: Multiple NA string tokens */
    "function(x) fread(text = x, na.strings = c(\"NA\", \"\", \"null\", \"NULL\", \"-\", \"N/A\"))",
    /* 10: Explicit line skip and row cap */
    "function(x) fread(text = x, skip = 1L, nrows = 5L)",
    /* 11: Substring search skip */
    "function(x) fread(text = x, skip = \"a\")",
    /* 12: Header-only sample (nrows = 0L) */
    "function(x) fread(text = x, nrows = 0L)",
    /* 13: Logical 0/1, Y/N, and leading-zero preservation */
    "function(x) fread(text = x, logical01 = TRUE, logicalYN = TRUE, keepLeadingZeros = TRUE)",
    /* 14: Force all columns to character */
    "function(x) fread(text = x, colClasses = \"character\")",
    /* 15: Explicit column class mapping + integer64 modes */
    "function(x) { fread(text = x, colClasses = list(integer = 1L), fill = TRUE);"
    "  fread(text = x, integer64 = \"character\"); fread(text = x, integer64 = \"double\") }",
    /* 16: Column selection by position and name */
    "function(x) { fread(text = x, select = c(1L, 2L), fill = TRUE);"
    "  fread(text = x, header = FALSE, select = \"V1\") }",
    /* 17: Column drop + check.names + key/index */
    "function(x) fread(text = x, header = FALSE, drop = 2L, check.names = TRUE, key = \"V1\", index = \"V1\")",
    /* 18: Explicit UTF-8 and Latin-1 markings + comment.char */
    "function(x) { fread(text = x, encoding = \"UTF-8\", comment.char = \"#\");"
    "  fread(text = x, encoding = \"Latin-1\", stringsAsFactors = TRUE) }",
    /* 19: Multi-threaded chunked parsing across jump points */
    "function(x) fread(text = x, nThread = 2L, fill = TRUE)",
};

static SEXP x_text;
static SEXP calls[N_CALLS];

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
    (void)argc;
    (void)argv;
    fuzz_init_r();

    Rf_protect(x_text = Rf_allocVector(STRSXP, 1));

    for (int i = 0; i < N_CALLS; i++) {
        SEXP wrapper;
        Rf_protect(wrapper = fuzz_make_wrapper(sources[i]));
        Rf_protect(calls[i] = Rf_lang2(wrapper, x_text));
    }

    /* Warmup: prime fread's dispatch and internal state. */
    SET_STRING_ELT(x_text, 0, Rf_mkChar("a,b,c\n1,2.5,x\n3,4.0,y\n"));
    for (int i = 0; i < N_CALLS; i++)
        fuzz_eval_silent(calls[i], R_GlobalEnv);

    return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > FUZZ_MAX_INPUT || memchr(data + 1, '\0', size - 1))
        return 0;

    char buffer[FUZZ_MAX_INPUT];
    memcpy(buffer, data + 1, size - 1);
    buffer[size - 1] = '\0';
    if (!fuzz_set_string(x_text, buffer))
        return 0;

    fuzz_eval_silent(calls[data[0] % N_CALLS], R_GlobalEnv);
    return 0;
}
