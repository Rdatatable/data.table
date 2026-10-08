# Fuzzing `data.table` (ClusterFuzzLite / OSS-Fuzz)

This directory contains coverage-guided fuzzing harnesses, dictionaries, sanitizer options, and build scripts for `data.table`, modeled directly on [`r-devel/r-oss-fuzz`](https://github.com/r-devel/r-oss-fuzz).

All files in `.clusterfuzzlite/` and `.github/workflows/` are excluded from the CRAN source tarball via `.Rbuildignore`.

## Fuzz Targets

Each `.clusterfuzzlite/harnesses/<name>.c` compiles into a libFuzzer target `<name>` with automatic wiring for `.clusterfuzzlite/dictionaries/<name>.dict` and `.clusterfuzzlite/options/<name>.options`:

| Target | Primary C Files Covered | Description |
| :--- | :--- | :--- |
| `fread` | `src/fread.c`, `src/freadR.c` | In-memory `fread(text = x, ...)` across 20 parser configurations (`sep`, `quote`, `header`, `fill`, `dec`, `na.strings`, `skip`, `nrows`, `logical01`, `logicalYN`, `keepLeadingZeros`, `colClasses`, `select`, `drop`, `encoding`, `nThread = 2L`). |
| `fread_file` | `src/fread.c`, `src/freadR.c` | File-backed / `mmap` `fread(file = f, ...)` reaching embedded `NUL` (`\0`) byte handling, UTF-8/UTF-16 BOM stripping, and page-boundary EOF conditions. |
| `fwrite` | `src/fwrite.c`, `src/fwriteR.c` | `fwrite()` across column types (`character`, `integer`, `numeric`, `logical`, `factor`, `Date`, `POSIXct`, `complex`, `integer64`, list columns), quoting/escaping modes, streaming `gzip` compression, and `fwrite() -> fread()` round-trips. |
| `forder` | `src/forder.c`, `src/fsort.c`, `src/frank.c`, `src/uniqlist.c`, `src/chmatch.c` | Radix ordering (`setorder`, `setkey`, `setindex`), `fsort`, `frank`, `unique`/`duplicated`/`uniqueN`, `rleid`/`rowid`, `chmatch`/`%chin%`, UTF-8/Latin-1 mixed encodings, and grouped aggregations (`by=` / `keyby=`). |
| `bmerge` | `src/bmerge.c`, `src/ijoin.c` | Binary search equi-joins, rolling joins (`roll = TRUE`, `-Inf`, `"nearest"`, bounded numeric, `rollends`), non-equi joins, `by = .EACHI`, update joins (`:=`), `foverlaps`, and set operations (`fintersect`, `funion`, `fsetdiff`, `fsetequal`). |
| `reshape` | `src/fmelt.c`, `src/fcast.c`, `src/rbindlist.c`, `src/transpose.c`, `src/cj.c` | `melt`, `dcast`, `rbindlist` (with heterogeneous column promotions, `use.names`, `fill`), `transpose`, `tstrsplit`, and `CJ`. |
| `froll` | `src/froll.c`, `src/frollR.c`, `src/frolladaptive.c`, `src/nafill.c`, `src/coalesce.c`, `src/fifelse.c`, `src/between.c`, `src/shift.c` | Rolling window statistics (`frollmean`, `frollsum`, `frollmax`, `frollmin`, `frollprod`, `frollmedian`, `frollvar`, `frollsd`, `frolladapt` with `algo = "fast"` and `"exact"`, fixed and adaptive windows), `nafill`/`setnafill`, `fcoalesce`, `fifelse`/`fcase`, `between`/`inrange`, and `shift`. |

## Architecture

1. **Prebuilt Instrumented R Base Image**:
   - Compiling R trunk with AddressSanitizer (`-fsanitize=address`), libFuzzer coverage instrumentation (`-fsanitize=fuzzer-no-link`), and `--enable-strict-barrier` takes ~45 minutes.
   - `.clusterfuzzlite/Dockerfile` pulls `ghcr.io/r-devel/r-oss-fuzz/r-base:address` (or a repository-local image built by `.clusterfuzzlite/docker/base/Dockerfile` via `.github/workflows/base-image.yml`), which provides R pre-installed at `/opt/r-prebuilt`.
2. **Per-Run `data.table` & Harness Build (`.clusterfuzzlite/ossfuzz.sh`)**:
   - Copies `/opt/r-prebuilt` to `$OUT/r-install` and installs `data.table` from source with `$CFLAGS` (`$SANITIZER_FLAGS` + `$COVERAGE_FLAGS`), so both `libR.so` and `data.table.so` provide full coverage feedback and sanitizer instrumentation.
   - Stages seed corpora from `inst/tests/*.csv` and `inst/tests/*.txt` alongside structured binary seeds.
   - Compiles and links each harness in `.clusterfuzzlite/harnesses/*.c` against `$LIB_FUZZING_ENGINE` and `libR.so` with `DT_RPATH` (`$ORIGIN/r-install/lib/R/lib`).
3. **In-Process Execution (`common.h`)**:
   - Initializes embedded R once in `LLVMFuzzerInitialize` (`R_MAX_VSIZE=1Gb`, `R_SignalHandlers=0`, `R_DATATABLE_NUM_THREADS=1`, `options(warn = -1)`, `library(data.table)`).
   - Evaluates each fuzz input under `R_ToplevelExec` / `R_tryEvalSilent` so expected R-level errors on malformed input are caught cleanly without terminating the fuzzer.
4. **GitHub Actions Workflows (`.github/workflows/`)**:
   - `cflite-pr.yml`: Fuzzes pull requests touching `src/**`, `R/**`, or `.clusterfuzzlite/**` (`mode: code-change`, 600s) and uploads SARIF results.
   - `cflite-batch.yml`: Runs daily batch fuzzing (`mode: batch`, 3600s) to grow the persistent corpus and upload a baseline build.
   - `cflite-prune.yml`: Runs weekly corpus minimization (`mode: prune`, 2400s).
   - `base-image.yml`: Optional workflow to build and publish a custom `r-base:address` container image from R SVN trunk.
