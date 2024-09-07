# data.table continuous integration and deployment

On each Pull Request opened in GitHub we run GitHub Actions test jobs to provide prompt feedback about the status of PR. Our main CI pipeline runs on GitLab CI nightly. GitLab repository automatically mirrors our GitHub repository and runs pipeline on `master` branch every night. It tests more environments and different configurations. It publish variety of artifacts.

## Environments

### [GitLab CI](./../.gitlab-ci.yml)

Test jobs:
- `test-lin-rel` - `r-release` on Linux, most comprehensive test environment, force all suggests, `-O3 -flto=auto -fno-common -Wunused-result`, test for no compilation warnings.
- `test-lin-rel-vanilla` - `r-release` on Linux, no suggested deps, no zlib, no OpenMP, flags `-g -O0 -fno-openmp`, skip manual and vignettes.
- `test-lin-rel-cran` - `--as-cran` on Linux, strict test for final status of `R CMD check`.
- `test-lin-dev-gcc-strict-cran` - `--as-cran` on Linux, `r-devel` built with `-enable-strict-barrier --disable-long-double`, test for compilation warnings, test for new NOTEs/WARNINGs from `R CMD check`.
- `test-lin-dev-clang-cran` - same as `gcc-strict` job but R built with `clang` and  no `--enable-strict-barrier --disable-long-double` flags.
- `test-lin-310-cran` - R 3.1.0 on Linux, stated R dependency version.
- `test-win-rel` - `r-release` on Windows.
- `test-win-dev` - `r-devel` on Windows.
- `test-win-old` - `r-oldrel` on Windows.
- `test-mac-rel` - macOS build not yet available, see [#3326](https://github.com/Rdatatable/data.table/issues/3326) for status

Tests jobs are allowed to fail, summary and logs of test jobs are later published at _CRAN-like checks_ page, see artifacts below.

Artifacts:
- [homepage](https://rdatatable.gitlab.io/data.table) - made with [pkgdown](https://github.com/r-lib/pkgdown)
- [html manual](https://rdatatable.gitlab.io/data.table/library/data.table/html/00Index.html)
- [pdf manual](https://rdatatable.gitlab.io/data.table/web/packages/data.table/data.table.pdf)
- [html vignettes](https://rdatatable.gitlab.io/data.table/library/data.table/doc/index.html)
- R packages repository for `data.table` and all _Suggests_ dependencies, url: `https://rdatatable.gitlab.io/data.table`
  - sources
  - Windows binaries for `r-release`, `r-devel` and `r-oldrel`
- [CRAN-like homepage](https://rdatatable.gitlab.io/data.table/web/packages/data.table/index.html)
- [CRAN-like checks results](https://rdatatable.gitlab.io/data.table/web/checks/check_results_data.table.html)

### [GitHub Actions](./../.github/workflows)

TODO document

## CI tools

### [`ci.R`](./ci.R)

Base R implemented helper script, [originally proposed to base R](https://svn.r-project.org/R/branches/tools4pkgs/src/library/tools/R/packages.R), that ease the process of extracting dependency information from description files, and to mirror packages and their recursive dependencies from CRAN to local CRAN-like directory. It is used in [GitLab CI pipeline](./../.gitlab-ci.yml).

### [`publish.R`](./publish.R)

Base R implemented helper script to orchestrate generation of most artifacts and to arrange them nicely. It is being used only in [_integration_ stage in GitLab CI pipeline](./../.gitlab-ci.yml).
