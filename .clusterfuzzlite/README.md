# Fuzzing `data.table` (ClusterFuzzLite / OSS-Fuzz)

This directory contains coverage-guided fuzzing harnesses, dictionaries, sanitizer options, and build scripts for `data.table`, modeled on [`r-devel/r-oss-fuzz`](https://github.com/r-devel/r-oss-fuzz).

## How Harnesses Work (R-First Design)

Instead of writing C code or embedding R expressions inside C string literals for every fuzz target:
- **Single C Driver ([`.clusterfuzzlite/harnesses/driver.c`](driver.c))**: Compiled once into `driver.o` and linked into every target binary `$OUT/<name>`. At `LLVMFuzzerInitialize` time, it inspects the binary's name (`<name>`), initializes embedded R + `data.table`, evaluates `.clusterfuzzlite/harnesses/<name>.R` once, and pre-builds protected `LANGSXP` call objects for each closure in the returned `list()`.
- **Plain `.R` Target Scripts (`.clusterfuzzlite/harnesses/<name>.R`)**: Each target is a regular R file returning a `list()` of `function(x) { ... }` closures. An optional `mode` attribute (`structure(mode = "text" | "file" | "lines", list(...))`) controls how `LLVMFuzzerTestOneInput` stages bytes `data[1..size-1]` (after the slot-selector byte `data[0]`):
  - `"lines"` (default): splits bytes on `\n` into a character vector `x` (and provides `.fuzz_file` in `R_GlobalEnv`).
  - `"text"`: stages bytes as a length-1 character string `x`.
  - `"file"`: writes raw bytes (including embedded `NUL`s or BOMs) to a scratch file and passes its path `f`.

To add a new fuzz case to an existing target, append a `function(x) { ... }` closure to its `.R` file. To add a new target, drop a new `<name>.R` file into `.clusterfuzzlite/harnesses/`.

## Fuzz Targets

| Target Script | Mode | Primary C Files Covered | Description |
| :--- | :--- | :--- | :--- |
| [`fread.R`](harnesses/fread.R) | `"text"` | `src/fread.c`, `src/freadR.c` | In-memory `fread(text = x, ...)` across 20 parser configurations (`sep`, `quote`, `header`, `fill`, `dec`, `na.strings`, `skip`, `nrows`, `logical01`, `logicalYN`, `keepLeadingZeros`, `colClasses`, `select`, `drop`, `encoding`, `nThread = 2L`). |
| [`fread_file.R`](harnesses/fread_file.R) | `"file"` | `src/fread.c`, `src/freadR.c` | File-backed / `mmap` `fread(file = f, ...)` reaching embedded `NUL` (`\0`) byte handling, UTF-8/UTF-16 BOM stripping, and page-boundary EOF conditions. |
| [`fwrite.R`](harnesses/fwrite.R) | `"lines"` | `src/fwrite.c`, `src/fwriteR.c` | `fwrite()` across column types (`character`, `integer`, `numeric`, `logical`, `factor`, `Date`, `POSIXct`, `complex`, `integer64`, list columns), quoting/escaping modes, streaming `gzip` compression, and `fwrite() -> fread()` round-trips. |
| [`forder.R`](harnesses/forder.R) | `"lines"` | `src/forder.c`, `src/fsort.c`, `src/frank.c`, `src/uniqlist.c`, `src/chmatch.c` | Radix ordering (`setorder`, `setkey`, `setindex`), `fsort`, `frank`, `unique`/`duplicated`/`uniqueN`, `rleid`/`rowid`, `chmatch`/`%chin%`, UTF-8/Latin-1 mixed encodings, and grouped aggregations (`by=` / `keyby=`). |
| [`bmerge.R`](harnesses/bmerge.R) | `"lines"` | `src/bmerge.c`, `src/ijoin.c`, `src/mergelist.c` | Binary search equi-joins, rolling joins (`roll = TRUE`, `-Inf`, `"nearest"`, bounded numeric, `rollends`), non-equi joins, `by = .EACHI`, update joins (`:=`), `foverlaps`, `mergelist`/`setmergelist`, `cbindlist`/`setcbindlist`, and set operations (`fintersect`, `funion`, `fsetdiff`, `fsetequal`). |
| [`reshape.R`](harnesses/reshape.R) | `"lines"` | `src/fmelt.c`, `src/fcast.c`, `src/rbindlist.c`, `src/transpose.c`, `src/cj.c` | `melt`, `dcast`, `rbindlist` (with heterogeneous column promotions, `use.names`, `fill`), `transpose`, `tstrsplit`, and `CJ`. |
| [`froll.R`](harnesses/froll.R) | `"lines"` | `src/froll.c`, `src/frollR.c`, `src/frolladaptive.c`, `src/nafill.c`, `src/coalesce.c`, `src/fifelse.c`, `src/between.c`, `src/shift.c`, `src/idatetime.c` | Rolling window statistics (`frollmean`, `frollsum`, `frollmax`, `frollmin`, `frollprod`, `frollmedian`, `frollvar`, `frollsd`, `frolladapt` with `algo = "fast"` and `"exact"`, fixed and adaptive windows), `nafill`/`setnafill`, `fcoalesce`, `fifelse`/`fcase`, `between`/`inrange`, `shift`, and `IDate`/`ITime` extractors (`year`, `month`, `mday`, `yday`, `wday`, `quarter`, `week`, `isoweek`, `yearmon`, `yearqtr`, `round.IDate`). |
| [`assign.R`](harnesses/assign.R) | `"lines"` | `src/assign.c`, `src/deleterows.c` | By-reference assignment (`:=`, `let`, `set`), sub-assignment type promotion (`memrecycle`), factor level expansion (`Csetlevels`), grouped `:=`, row deletion by reference (`.ROW := NULL` / `CdeleteRows`), `setnames`, `setcolorder`, `setDT`/`setDF`, `setalloccol`, and `[<-`/`$<-`/`[[<-`. |

## Dictionaries & Options

- [`dictionaries/fread.dict`](dictionaries/fread.dict) & [`dictionaries/fread_file.dict`](dictionaries/fread_file.dict): Delimiters, quotes, escapes, BOMs, embedded `NUL`s, and 32/64-bit integer overflow boundary literals for `fread`.
- [`dictionaries/lines.dict`](dictionaries/lines.dict): Shared fallback dictionary for all `"lines"`-mode targets (IEEE-754 special values, integer limits, UTF-8/Latin-1 sequences).
- [`options/default.options`](options/default.options): Default ASan configuration (`detect_leaks = 0` so `R_MAX_VSIZE` `longjmp`s past `malloc` cleanup do not trigger false-positive exit-time LeakSanitizer aborts).
