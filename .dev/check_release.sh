#!/usr/bin/env bash
# Static code checks for data.table CRAN release.
#
# bash .dev/check_release.sh
# 
# Prints nothing on success; reports all failures then exits non-zero.
# Run from anywhere inside the data.table repo.
#
# Prerequisites:
#   macOS: brew install grep   (provides ggrep, GNU grep)
#   Linux: grep already GNU
#
# Checks NOT automated here (need manual review before release):
#   - T/F bare symbols in tests.Rraw  (many intentional false positives)
#   - tryCatch in tests.Rraw          (used legitimately in test infrastructure)
#   - PROTECT leak seals (install.*alloc etc.) -- existing code has known-ok matches
#   - setAttrib / install( / mkChar raw scans  -- require human judgment
#   - pragma omp file list            -- informational only (update ?openmp docs)
#   - Loop counter scope in froll.c   -- lines 560/767 not yet tagged
#   - z_const                         -- one expected comment-only match
#   - checkbashisms ./configure       -- Linux-only tool (devscripts package)

set -uo pipefail

# ── Locate repo root ──────────────────────────────────────────────────────────
REPO=$(git -C "$(dirname "${BASH_SOURCE[0]}")" rev-parse --show-toplevel 2>/dev/null) || {
    echo "ERROR: not inside a git repository" >&2; exit 1
}
grep -q "^Package: data.table$" "$REPO/DESCRIPTION" 2>/dev/null || {
    echo "ERROR: git root is not the data.table package ($REPO/DESCRIPTION)" >&2; exit 1
}

SRC="$REPO/src"
R_DIR="$REPO/R"
MAN="$REPO/man"
TESTS="$REPO/inst/tests"

# ── Detect GNU grep ───────────────────────────────────────────────────────────
if command -v ggrep &>/dev/null && ggrep --version 2>/dev/null | grep -q "GNU grep"; then
    G=ggrep
elif grep --version 2>/dev/null | grep -q "GNU grep"; then
    G=grep
else
    echo "ERROR: GNU grep required. On macOS: brew install grep" >&2
    exit 1
fi

# ── Check harness ─────────────────────────────────────────────────────────────
NFAIL=0

# chk DESC CMD — run CMD in a subshell; report failure if it produces any output.
chk() {
    local desc="$1" cmd="$2"
    local out
    out=$(bash -c "$cmd" 2>&1) || true
    if [[ -n "$out" ]]; then
        printf '\nFAIL: %s\n%s\n' "$desc" "$(printf '%s\n' "$out" | sed 's/^/  /')"
        NFAIL=$((NFAIL + 1))
    fi
}

# ── Encoding ──────────────────────────────────────────────────────────────────
# po/ = translation files (legitimate non-ASCII), vignettes/ = localised Rmd.
# DESCRIPTION may have accented characters in international contributor names —
# R CMD check --as-cran validates it separately.
chk "no non-ASCII bytes in R/C/test/man source" \
    "$G -RI --exclude-dir=.git --exclude-dir=po --exclude-dir=vignettes \
        --exclude='*.md' --exclude='*.yml' --exclude='*~' --exclude='*.csv' \
        --exclude='DESCRIPTION' --exclude='NAMESPACE' \
        -P '[\x80-\xFF]' '$REPO'"

# ── R source ──────────────────────────────────────────────────────────────────
chk "no .Call with quoted first arg" \
    "$G '[.]Call(\"' '$R_DIR'/*.R"

chk "no ifelse() — use fifelse()" \
    "$G -Enr '\bifelse' '$R_DIR'"

# test.data.table.R legitimately uses substring() in its test filtering logic
chk "no substring() — use substr() (#4447)" \
    "$G -Fnr 'substring' '$R_DIR' --exclude=test.data.table.R"

chk "no tabs in R source" \
    "$G -Pln '\t' '$R_DIR'/*.R"

# ── Rd manual pages ───────────────────────────────────────────────────────────
chk "no unescaped % in .Rd files (silently truncates text)" \
    "$G -n '[^\\\\]%' '$MAN'/*.Rd"

# ── Test files ────────────────────────────────────────────────────────────────
# Commented-out system.time lines (prefixed with #) are filtered out
chk "no system.time in non-benchmark tests" \
    "$G -Fn 'system.time' '$TESTS'/*.Rraw \
        | $G -Fv 'benchmark.Rraw' \
        | $G -Fv 'this system.time usage ok' \
        | $G -Pv ':[[:space:]]*#'"

# ── OpenMP ────────────────────────────────────────────────────────────────────
# openmp-utils.c, myomp.h, fread.c/h have intentional direct uses
chk "no direct omp_get_max_threads (use getDTthreads)" \
    "$G -rn 'omp_get_max_threads' '$SRC' --include='*.c' --include='*.h' \
        | $G -Fv 'openmp-utils' | $G -Fv 'myomp' | $G -Fv 'fread'"

# Every parallel region must have an explicit num_threads() clause (may use the
# 'nth' variable set by getDTthreads() rather than calling it inline).
chk "all 'pragma omp parallel' have a num_threads() clause" \
    "$G -i 'pragma.*omp parallel' '$SRC'/*.c | $G -v num_threads"

chk "no 'const int nth' (Solaris compat, #4638)" \
    "$G -i 'const.*int.*nth' '$SRC'/*.c"

# ── C source — style ──────────────────────────────────────────────────────────
chk "no tabs in C source" \
    "$G -Pln '\t' '$SRC'/*.c"

chk "no Rprintf in init.c (use REprintf)" \
    "$G 'Rprintf' '$SRC/init.c'"

# Filter comment lines (init.c:318 documents the forbidden pattern in a comment)
chk "no &REAL type-punning (excl. comment lines)" \
    "$G -n '&REAL' '$SRC'/*.c | $G -Pv ':\d+:\s*//'"

chk "no [UN]PROTECT_PTR (#3232)" \
    "$G 'PROTECT_PTR' '$SRC'/*.c"

# ── C source — R API compliance ───────────────────────────────────────────────
chk "use R_Realloc, not bare Realloc" \
    "$G -n '\bRealloc\b' '$SRC'/*.c | $G -v '\bR_Realloc\b'"

chk "use R_Calloc, not bare Calloc" \
    "$G -n '\bCalloc\b' '$SRC'/*.c | $G -v '\bR_Calloc\b'"

chk "use R_Free, not bare Free" \
    "$G -n '\bFree\b' '$SRC'/*.c | $G -v '\bR_Free\b' \
        | $G -Pv ':\d+:\s*//' | $G -Pv ':\d+:\s*\*'"

chk "use STRING_PTR_RO, not STRING_PTR (#6312)" \
    "$G -n '\bSTRING_PTR\b' '$SRC'/*.c | $G -v '\bSTRING_PTR_RO\b'"

chk "no defined(R_VERSION) guards (obsolete)" \
    "$G -rn '\bdefined(R_VERSION)\b' '$SRC'/*.c"

# ── C source — PROTECT hygiene ────────────────────────────────────────────────
# ScalarLogical returns a global constant from R >= 3.1.0; wrapping it in
# PROTECT() is unnecessary and a sign of stale code.
chk "ScalarLogical must NOT be directly PROTECTed (R >= 3.1 returns a constant)" \
    "$G -n 'PROTECT(ScalarLogical' '$SRC'/*.c"

# ScalarInteger/ScalarString allocate; they must be PROTECTed, returned, or
# passed to setAttrib (which protects them).
chk "ScalarInteger must be PROTECTed, returned, or passed to setAttrib" \
    "cd '$SRC' && $G 'ScalarInteger' *.c | $G -v PROTECT | $G -v setAttrib | $G -v return"

chk "ScalarString must be PROTECTed, returned, or passed to setAttrib" \
    "cd '$SRC' && $G 'ScalarString' *.c | $G -v PROTECT | $G -v setAttrib | $G -v return"

# allocVector/coerceVector/asCharacter return unprotected SEXPs; any line that
# uses them without an immediate protection mechanism is flagged.
# Comment filters (':\s*//' and ':\s*\*') exclude '//' and C block comment lines.
# Additional allocVector filters exclude trailing-comment mentions, string-literal
# mentions, and the SEXP-assigned-then-returned idiom (verified safe in utils.c).
chk "allocVector without PROTECT / SET_VECTOR_ELT / setAttrib / return" \
    "cd '$SRC' && $G 'allocVector' *.c \
        | $G -v PROTECT | $G -v SET_VECTOR_ELT | $G -v setAttrib | $G -v return \
        | $G -Pv ':\s*//' | $G -Pv ':\s*\*' \
        | $G -Pv '//.*allocVector' | $G -Pv '\"[^\"]*allocVector' \
        | $G -Pv 'SEXP \w+ = allocVector'"

chk "coerceVector without PROTECT / SET_VECTOR_ELT / setAttrib / return" \
    "cd '$SRC' && $G 'coerceVector' *.c \
        | $G -v PROTECT | $G -v SET_VECTOR_ELT | $G -v setAttrib | $G -v return \
        | $G -Pv ':\s*//' | $G -Pv ':\s*\*'"

chk "asCharacter without PROTECT / SET_VECTOR_ELT / setAttrib / return" \
    "cd '$SRC' && $G 'asCharacter' *.c \
        | $G -v PROTECT | $G -v SET_VECTOR_ELT | $G -v setAttrib | $G -v return \
        | $G -Pv ':\s*//' | $G -Pv ':\s*\*'"

# getAttrib calls should use pre-installed sym_* symbols, not mk* functions
chk "no getAttrib with mk* (use pre-installed sym_* symbols)" \
    "cd '$SRC' && $G 'getAttrib.*mk' *.c"

# ── Summary ───────────────────────────────────────────────────────────────────
if [[ $NFAIL -gt 0 ]]; then
    printf '\n%d check(s) failed.\n' "$NFAIL" >&2
    exit 1
fi
