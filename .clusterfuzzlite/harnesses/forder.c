/*
 * libFuzzer harness for data.table's radix ordering, sorting, ranking,
 * grouping, uniqueness, and string-hashing primitives:
 *   - src/forder.c   (setorder, setorderv, setkey, setindex, by= grouping)
 *   - src/fsort.c    (fsort parallel double radix sort)
 *   - src/frank.c    (frank, frankv across all ties.method options)
 *   - src/uniqlist.c (unique, duplicated, anyDuplicated, uniqueN, rleid, rowid)
 *   - src/chmatch.c  (chmatch, %chin%, chorder, chgroup)
 *
 * Splits the input on newlines into a character vector `x`, from which
 * each slot constructs character, numeric, integer, integer64, logical,
 * factor, and UTF-8 / Latin-1 marked columns.
 */

#include <stdint.h>
#include <string.h>

#include "common.h"

#define FUZZ_MAX_INPUT (1024 * 16)
#define FUZZ_MAX_LINES 2000
#define N_CALLS 16

static SEXP calls[N_CALLS];

static const char *const sources[N_CALLS] = {
    /* 0: Character radix order (ascending, descending, na.last variants) */
    "function(x) { dt <- data.table(a = x); setorder(dt, a, na.last = TRUE);"
    "  setorder(dt, -a, na.last = FALSE); dt[order(a, na.last = NA)] }",
    /* 1: Numeric radix order (including -0.0, NaN, Inf, -Inf, denormals) */
    "function(x) { n <- as.numeric(x); dt <- data.table(n = n);"
    "  setorder(dt, n, na.last = TRUE); setorder(dt, -n, na.last = FALSE);"
    "  dt[order(n, na.last = NA)] }",
    /* 2: Integer and logical radix order (counting sort vs radix range branches) */
    "function(x) { i <- as.integer(as.numeric(x)); l <- (i %% 3L == 0L);"
    "  dt <- data.table(i = i, l = l); setorder(dt, i, -l, na.last = TRUE);"
    "  setorder(dt, -i, l, na.last = FALSE) }",
    /* 3: 64-bit integer (bit64::integer64 representation on REALSXP) radix order */
    "function(x) { n <- as.numeric(x); class(n) <- 'integer64';"
    "  dt <- data.table(i64 = n); setorder(dt, i64, na.last = TRUE);"
    "  setorder(dt, -i64, na.last = FALSE) }",
    /* 4: Multi-column mixed-type radix order + setkey / setindex */
    "function(x) { n <- as.numeric(x); i <- as.integer(n);"
    "  dt <- data.table(s = x, n = n, i = rev(i));"
    "  setkey(dt, s, n); setindex(dt, n, i);"
    "  setorderv(dt, c('s', 'n', 'i'), order = c(1L, -1L, 1L)) }",
    /* 5: fsort on numeric vectors (src/fsort.c) */
    "function(x) { n <- as.numeric(x); n <- n[!is.na(n) & n >= 0];"
    "  if (length(n) > 0L) fsort(n);"
    "  fsort(as.numeric(x), decreasing = FALSE, na.last = TRUE, internal = FALSE) }",
    /* 6: frank / frankv across all ties.method modes */
    "function(x) { n <- as.numeric(x); dt <- data.table(x = x, n = n);"
    "  frank(dt, ties.method = 'average', na.last = 'keep');"
    "  frank(dt, ties.method = 'first', na.last = TRUE);"
    "  frank(dt, ties.method = 'last', na.last = FALSE);"
    "  frank(dt, ties.method = 'dense'); frank(dt, ties.method = 'min');"
    "  frank(dt, ties.method = 'max'); frankv(n, ties.method = 'random') }",
    /* 7: unique, duplicated, anyDuplicated, uniqueN on data.table */
    "function(x) { n <- as.numeric(x); dt <- data.table(a = x, b = rev(n));"
    "  unique(dt); unique(dt, fromLast = TRUE); unique(dt, by = 'a');"
    "  duplicated(dt); anyDuplicated(dt); uniqueN(dt); uniqueN(dt, by = 'a') }",
    /* 8: rleid and rowid run-length / group-counter primitives */
    "function(x) { n <- as.numeric(x);"
    "  rleid(x); rleid(x, n); rowid(x); rowid(x, n, prefix = 'id') }",
    /* 9: chmatch and %chin% string hash table */
    "function(x) { chmatch(x, rev(x)); chmatch(rev(x), x, nomatch = 0L);"
    "  x %chin% rev(x); data.table:::chorder(x); data.table:::chgroup(x) }",
    /* 10: UTF-8 and Latin-1 mixed-encoding radix order and chmatch */
    "function(x) { u <- x; Encoding(u) <- ifelse(validUTF8(u), 'UTF-8', 'unknown');"
    "  l <- rev(x); Encoding(l) <- 'latin1';"
    "  m <- c(u, l); dt <- data.table(m = m);"
    "  setorder(dt, m); unique(dt); chmatch(u, l); u %chin% l }",
    /* 11: Factor ordering, grouping, and uniqueness */
    "function(x) { f <- factor(x); dt <- data.table(f = f, v = seq_along(x));"
    "  setorder(dt, f); unique(dt, by = 'f'); dt[, .(cnt = .N), by = f] }",
    /* 12: Grouped aggregations (forder retgrp = TRUE + dogroups / GForce) */
    "function(x) { n <- as.numeric(x); i <- as.integer(n);"
    "  dt <- data.table(g = x, n = n, i = i);"
    "  dt[, .(s = sum(n, na.rm = TRUE), m = mean(n, na.rm = TRUE),"
    "         mn = min(i, na.rm = TRUE), mx = max(i, na.rm = TRUE),"
    "         cnt = .N, grp = .GRP), by = g] }",
    /* 13: keyby= and ad-hoc by= expressions */
    "function(x) { n <- as.numeric(x); dt <- data.table(g = x, n = n);"
    "  dt[, .(med = median(n, na.rm = TRUE), sd = sd(n, na.rm = TRUE),"
    "         first = first(n), last = last(n)), keyby = .(g, neg = n < 0)] }",
    /* 14: set() and := in-place column assignment + shallow/copy */
    "function(x) { dt <- data.table(a = x); dt[, b := as.numeric(a)];"
    "  if (nrow(dt) > 0L) set(dt, i = 1L, j = 'a', value = 'z');"
    "  dt[, b := NULL]; copy(dt) }",
    /* 15: Multi-threaded radix sort (setDTthreads(2L)) */
    "function(x) { setDTthreads(2L); on.exit(setDTthreads(1L));"
    "  dt <- data.table(a = rep(x, length.out = 250L),"
    "                   b = as.numeric(rep(x, length.out = 250L)));"
    "  setorder(dt, a, -b) }",
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
