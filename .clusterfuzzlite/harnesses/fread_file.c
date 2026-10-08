/*
 * libFuzzer harness for data.table::fread() file / mmap path.
 *
 * Unlike fread(text = ...), which passes an R CHARSXP and therefore cannot
 * contain embedded NUL bytes, fread(file = ...) mmaps the file directly in
 * src/fread.c. This reaches:
 *   - Embedded NUL byte ('\0') handling and skipping inside fields/headers
 *   - UTF-8 / UTF-16 BOM stripping at the start of the mapped region
 *   - Page-aligned mmap EOF termination checks (fsize % 4096 == 0)
 *   - Multi-threaded mmap jump-point scanning
 *
 * Archive magic bytes (PKZIP, gzip, bzip2) are rejected up front so every
 * iteration exercises src/fread.c's C mmap parser directly rather than
 * shelling out to R's archive/decompression wrappers.
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

#define FUZZ_MAX_INPUT (1024 * 64)
#define N_CALLS 8

static const char *const sources[N_CALLS] = {
    /* 0: Default mmap auto-detection */
    "function(f) fread(file = f)",
    /* 1: Comma-separated with fill = TRUE */
    "function(f) fread(file = f, sep = \",\", fill = TRUE)",
    /* 2: Unquoted tab-separated */
    "function(f) fread(file = f, sep = \"\\t\", quote = \"\")",
    /* 3: Single-column line mode */
    "function(f) fread(file = f, sep = NULL, header = FALSE)",
    /* 4: Strip white FALSE + single quote */
    "function(f) fread(file = f, quote = \"'\", strip.white = FALSE, fill = TRUE)",
    /* 5: Header-only and skip modes */
    "function(f) { fread(file = f, nrows = 0L); fread(file = f, skip = 1L, fill = TRUE) }",
    /* 6: Comment char and blank.lines.skip */
    "function(f) fread(file = f, comment.char = \"#\", blank.lines.skip = TRUE)",
    /* 7: Multi-threaded mmap chunking */
    "function(f) fread(file = f, nThread = 2L, fill = TRUE)",
};

static char *scratch_path;
static SEXP calls[N_CALLS];

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
    (void)argc;
    (void)argv;
    fuzz_init_r();

    scratch_path = fuzz_scratch_file("fread");
    if (scratch_path == NULL)
        abort();

    SEXP path_sexp;
    Rf_protect(path_sexp = Rf_mkString(scratch_path));
    for (int i = 0; i < N_CALLS; i++) {
        SEXP wrapper;
        Rf_protect(wrapper = fuzz_make_wrapper(sources[i]));
        Rf_protect(calls[i] = Rf_lang2(wrapper, path_sexp));
    }

    return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > FUZZ_MAX_INPUT)
        return 0;

    const uint8_t *payload = data + 1;
    size_t payload_size = size - 1;

    /* Skip zip/gzip/bzip2 magic headers so fread() always takes the direct
     * C mmap path in src/fread.c rather than R-level archive extraction. */
    if (payload_size >= 2) {
        if ((payload[0] == 'P' && payload[1] == 'K') ||
            (payload[0] == 0x1f && payload[1] == 0x8b) ||
            (payload[0] == 'B' && payload[1] == 'Z'))
            return 0;
    }

    if (!fuzz_write_scratch(scratch_path, payload, payload_size))
        return 0;

    fuzz_eval_silent(calls[data[0] % N_CALLS], R_GlobalEnv);
    return 0;
}
