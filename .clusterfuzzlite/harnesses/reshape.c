/*
 * libFuzzer harness for data.table's reshaping, binding, transposing,
 * splitting, and cross-join C primitives:
 *   - src/fmelt.c     (melt.data.table, single/list measure.vars, na.rm, factors)
 *   - src/fcast.c     (dcast.data.table, fun.aggregate, drop, fill, multi-value)
 *   - src/rbindlist.c (rbindlist type coercion, use.names, fill, idcol)
 *   - src/transpose.c (transpose, tstrsplit)
 *   - src/cj.c        (CJ cross join)
 */

#include <stdint.h>
#include <string.h>

#include "common.h"

#define FUZZ_MAX_INPUT (1024 * 16)
#define FUZZ_MAX_LINES 500
#define N_CALLS 12

static SEXP calls[N_CALLS];

static const char *const sources[N_CALLS] = {
    /* 0: melt across homogeneous and heterogeneous column types */
    "function(x) { n <- as.numeric(x); i <- as.integer(n);"
    "  dt <- data.table(id = x, v1 = n, v2 = rev(n), v3 = i, v4 = rev(x));"
    "  melt(dt, id.vars = 'id', measure.vars = c('v1', 'v2'), na.rm = TRUE);"
    "  melt(dt, id.vars = 'id', measure.vars = c('v1', 'v3', 'v4'),"
    "       variable.factor = FALSE, value.factor = TRUE) }",
    /* 1: melt with list measure.vars (multiple value columns) */
    "function(x) { n <- as.numeric(x); i <- as.integer(n);"
    "  dt <- data.table(id = x, a1 = n, a2 = rev(n), b1 = i, b2 = rev(i));"
    "  melt(dt, id.vars = 'id', measure.vars = list(c('a1', 'a2'), c('b1', 'b2')),"
    "       na.rm = TRUE); melt(dt, id.vars = 'id', measure.vars = patterns('^a', '^b')) }",
    /* 2: dcast basic and with fill / drop = TRUE */
    "function(x) { k <- head(x, 60L); n <- as.numeric(k);"
    "  dt <- data.table(r = k, c = rev(k), v = n);"
    "  dcast(dt, r ~ c, value.var = 'v', fun.aggregate = length);"
    "  dcast(dt, r ~ c, value.var = 'v', fun.aggregate = sum, fill = 0) }",
    /* 3: dcast with drop = FALSE and multiple value.var */
    "function(x) { k <- head(x, 25L); n <- as.numeric(k); i <- as.integer(n);"
    "  dt <- data.table(r = factor(k), c = factor(rev(k)), v1 = n, v2 = i);"
    "  dcast(dt, r ~ c, value.var = c('v1', 'v2'), fun.aggregate = mean, drop = FALSE);"
    "  dcast(dt, r ~ c, value.var = 'v1', fun.aggregate = min, drop = c(TRUE, FALSE)) }",
    /* 4: melt -> dcast round-trip */
    "function(x) { k <- head(x, 40L); n <- as.numeric(k);"
    "  dt <- data.table(id = seq_along(k), grp = k, a = n, b = rev(n));"
    "  m <- melt(dt, id.vars = c('id', 'grp'));"
    "  dcast(m, id + grp ~ variable, value.var = 'value') }",
    /* 5: rbindlist with type promotions, fill = TRUE, use.names = TRUE */
    "function(x) { n <- as.numeric(x); i <- as.integer(n); f <- factor(x);"
    "  d1 <- data.table(a = x, b = n, c = i);"
    "  d2 <- data.table(b = rev(x), a = f, d = !is.na(n));"
    "  d3 <- data.table(a = i, c = n);"
    "  rbindlist(list(d1, d2, d3), use.names = TRUE, fill = TRUE, idcol = 'src');"
    "  rbindlist(list(d1, d2), use.names = FALSE, fill = TRUE) }",
    /* 6: rbindlist with Date, POSIXct, integer64, and list columns */
    "function(x) { n <- as.numeric(x); n[!is.finite(n)] <- 0;"
    "  i64 <- n; class(i64) <- 'integer64';"
    "  d1 <- data.table(a = as.Date(n %% 10000, origin = '1970-01-01'), b = i64, l = as.list(x));"
    "  d2 <- data.table(a = as.Date(rev(n) %% 10000, origin = '1970-01-01'), b = n, l = as.list(n));"
    "  rbindlist(list(d1, d2), use.names = TRUE, fill = TRUE) }",
    /* 7: transpose on lists and data.tables */
    "function(x) { n <- as.numeric(x); i <- as.integer(n);"
    "  dt <- data.table(rn = x, a = n, b = rev(n), c = i);"
    "  transpose(dt, keep.names = 'col', make.names = 'rn');"
    "  l <- list(x, head(x, 3L), tail(x, 5L));"
    "  transpose(l, fill = NA_character_, ignore.empty = TRUE);"
    "  transpose(l, fill = '', ignore.empty = FALSE) }",
    /* 8: tstrsplit with type.convert and fixed/regex splits */
    "function(x) { tstrsplit(x, ',', fixed = TRUE, fill = '<NA>');"
    "  tstrsplit(x, ',', fixed = TRUE, type.convert = TRUE);"
    "  tstrsplit(x, '[[:space:],;|]+', keep = 1L) }",
    /* 9: CJ cross-join with sorted and unique variants */
    "function(x) { k <- head(x, 20L); n <- as.numeric(k);"
    "  CJ(a = k, b = n, sorted = TRUE, unique = FALSE);"
    "  CJ(a = k, b = n, sorted = FALSE, unique = TRUE) }",
    /* 10: split.data.table across by= and flatten */
    "function(x) { k <- head(x, 60L); n <- as.numeric(k);"
    "  dt <- data.table(g1 = k, g2 = rev(k), v = n);"
    "  s <- split(dt, by = 'g1', keep.by = TRUE, drop = TRUE);"
    "  rbindlist(s, idcol = 'grp') }",
    /* 11: tables, setalloccol, truelength, address primitives */
    "function(x) { dt <- as.data.table(list(a = x, b = as.numeric(x)));"
    "  alloc.col(dt, 64L); setDT(as.data.frame(dt));"
    "  truelength(dt); address(dt) }",
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
