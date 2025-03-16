# data.table continuous integration and deployment

On each Pull Request opened in GitHub we run GitHub Actions test jobs to provide prompt feedback about the status of PR. Our more thorough main CI pipeline runs nightly on GitLab CI. GitLab repository automatically mirrors our GitHub repository and runs pipeline on `master` branch every night. It tests more environments and different configurations. It publishes a variety of artifacts such as our [homepage](https://rdatatable.gitlab.io/data.table/) and [CRAN-like website for dev version](https://rdatatable.gitlab.io/data.table/web/packages/data.table/index.html), including windows binaries for the dev version.

## Environments

### [GitLab CI](./../.gitlab-ci.yml)

Test jobs:
- `test-lin-rel` - `r-release` on Linux, most comprehensive test environment, force all suggests, `-O3 -flto=auto -fno-common -Wunused-result`, test for no compilation warnings.
- `test-lin-rel-vanilla` - `r-release` on Linux, no suggested deps, no zlib, no OpenMP, flags `-g -O0 -fno-openmp`, skip manual and vignettes.
- `test-lin-rel-cran` - `--as-cran` on Linux, strict test for final status of `R CMD check`.
- `test-lin-dev-gcc-strict-cran` - `--as-cran` on Linux, `r-devel` built with `-enable-strict-barrier --disable-long-double`, test for compilation warnings, test for new NOTEs/WARNINGs from `R CMD check`.
- `test-lin-dev-clang-cran` - same as `gcc-strict` job but R built with `clang` and  no `--enable-strict-barrier --disable-long-double` flags.
- `test-lin-ancient-cran` - Stated R dependency version (currently 3.3.0) on Linux.
- `test-lin-dev-san` - `r-devel` on Linux built with `clang -fsanitize=address,undefined` (including LeakSanitizer), test for sanitizer output in tests and examples.
- `test-win-rel` - `r-release` on Windows.
- `test-win-dev` - `r-devel` on Windows.
- `test-win-old` - `r-oldrel` on Windows.
- `test-mac-rel` - `r-release` on macOS.
- `test-mac-old` - `r-oldrel` on macOS.

The CI steps for the tests are [required](https://github.com/Rdatatable/data.table/blob/55eb0f160b169398d51f138131c14a66c86e5dc9/.ci/publish.R#L162-L168) to be named according to the pattern `test-(lin|win|mac)-<R version>[-<suffix>]*`, where `<R version>` is `rel`, `dev`, `old`, `ancient`, or three digits comprising an R version (e.g. `362` corresponding to R-3.6.2).

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

## GitLab Open Source Program

We are currently part of the [GitLab for Open Source Program](https://about.gitlab.com/solutions/open-source/). This gives us 50,000 compute minutes per month for our GitLab CI. Our license needs to be renewed yearly (around July) and is currently managed by @ben-schwen.

## Updating CI pipeline

Basic CI checks are also run on every push to the GitLab repository. This can **and should** be used for PRs changing the CI pipeline before merging them to master.

```shell
# fetch changes from remote (GitHub) and push them to GitLab 
git fetch git@github.com:Rdatatable/data.table.git new_branch:new_branch
git push
# after updating on GitHub, pull changes from remote and push to GitLab
git pull git@github.com:Rdatatable/data.table.git new_branch
git push
```

Make sure to include a link to the pipeline results in your PR. 
