# A list of performance tests.
#
# See documentation in https://github.com/Rdatatable/data.table/wiki/Performance-testing for best practices.
#
# Each entry in this list corresponds to a performance test and contains a sublist with three mandatory arguments:
# - N: A numeric sequence of data sizes to vary.
# - setup: An expression evaluated for every data size before measuring time/memory.
# - expr: An expression that will be evaluated for benchmarking performance across different git commit versions.
#         This must call a function from data.table using a syntax with double or triple colon prefix.
#         The package name before the colons will be replaced by a new package name that uses the commit SHA hash.
#         (For instance, data.table:::[.data.table will become data.table.some_40_digit_SHA1_hash:::[.data.table)
#
# Optional parameters that may be useful to configure tests:
# - times: Number of times each expression is evaluated (default is 10).
# - seconds.limit: The maximum median timing (in seconds) of an expression. No timings for larger N are computed past that threshold. Default of 0.01 seconds should be sufficient for most tests.
# - Historical references (short version name = commit SHA1).
#   For historical regressions, use version names 'Before', 'Regression', and 'Fixed'
#   When there was no regression, use 'Slow' and 'Fast' 
# @note Please check https://github.com/tdhock/atime/blob/main/vignettes/data.table.Rmd for more information.
# nolint start: undesirable_operator_linter. ':::' needed+appropriate here.
test.list <- atime::atime_test_list(
  # A function to customize R package metadata and source files to facilitate version-specific installation and testing.
  #
  # This is specifically tailored for handling data.table which requires specific changes in non-standard files (such as the object file name in Makevars and version checking code in onLoad.R)
  # to support testing across different versions (base and HEAD for PRs, commits associated with historical regressions, etc.) of the package.
  # It appends a SHA1 hash to the package name (PKG.SHA), ensuring each version can be installed and tested separately.
  #
  # @param old.Package Current name of the package.
  # @param new.Package New name of the package, including a SHA hash.
  # @param sha SHA1 hash used for differentiating versions.
  # @param new.pkg.path Path to the package files.
  #
  # @details
  # The function modifies:
  # - DESCRIPTION, updating the package name.
  # - Makevars, customizing the shared object file name and adjusting the build settings.
  # - R/onLoad.R, adapting custom version checking for package loading operations.
  # - NAMESPACE, changing namespace settings for dynamic linking.
  #
  # @examples
  # pkg.edit.fun("data.table", "data.table.some_SHA1_hash", "some_SHA1_hash", "/path/to/data.table")
  #
  # @return None (performs in-place file modifications)
  # @note This setup is typically unnecessary for most packages but essential for data.table due to its unique configuration.
  pkg.edit.fun = function(old.Package, new.Package, sha, new.pkg.path) {
    pkg_find_replace <- function(glob, FIND, REPLACE) {
      atime::glob_find_replace(file.path(new.pkg.path, glob), FIND, REPLACE)
    }
    Package_regex <- gsub(".", "_?", old.Package, fixed = TRUE)
    Package_ <- gsub(".", "_", old.Package, fixed = TRUE)
    new.Package_ <- paste0(Package_, "_", sha)
    pkg_find_replace(
      "DESCRIPTION",
      paste0("Package:\\s+", old.Package),
      paste("Package:", new.Package))
    pkg_find_replace(
      file.path("src", "Makevars.*in"),
      Package_regex,
      new.Package_)
    pkg_find_replace(
      file.path("R", "onLoad.R"),
      Package_regex,
      new.Package_)
    pkg_find_replace(
      file.path("R", "onLoad.R"),
      sprintf('packageVersion\\("%s"\\)', old.Package),
      sprintf('packageVersion\\("%s"\\)', new.Package))
    pkg_find_replace(
      file.path("src", "init.c"),
      paste0("R_init_", Package_regex),
      paste0("R_init_", gsub("[.]", "_", new.Package_)))
    pkg_find_replace(
      "NAMESPACE",
      sprintf('useDynLib\\("?%s"?', Package_regex),
      paste0('useDynLib(', new.Package_))
  },

  # Performance regression discussed in: https://github.com/Rdatatable/data.table/issues/4311
  # Fixed in: https://github.com/Rdatatable/data.table/pull/4440
  "shallow regression fixed in #4440" = atime::atime_test(
    N = 10^seq(3,8),
    setup = {
      set.seed(1L)
      dt <- data.table(a = sample.int(N))
      setindexv(dt, "a")
    },
    expr = data.table:::shallow(dt),
    # Before = "", This needs to be updated later as there are two issues here: A) The source of regression (or the particular commit that led to it) is not clear; B) Older versions of data.table are having problems when being installed in this manner (This includes commits from before March 20 2020, when the issue that discovered or first mentioned the regression was created)
    Regression = "b1b1832b0d2d4032b46477d9fe6efb15006664f4", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/0f0e7127b880df8459b0ed064dc841acd22f5b73) in the PR (https://github.com/Rdatatable/data.table/pull/4440/commits) that fixes the regression
    Fixed = "9d3b9202fddb980345025a4f6ac451ed26a423be"), # Merge commit in the PR that fixed the regression (https://github.com/Rdatatable/data.table/pull/4440)

  # Test based on: https://github.com/Rdatatable/data.table/issues/5424
  # Performance regression introduced from a commit in: https://github.com/Rdatatable/data.table/pull/4491
  # Fixed in: https://github.com/Rdatatable/data.table/pull/5463
  "memrecycle regression fixed in #5463" = atime::atime_test(
    N = 10^seq(3, 8),
    setup = {
      n <- N/100
      set.seed(2L)
      dt <- data.table(
        g = sample(seq_len(n), N, TRUE),
        x = runif(N),
        key = "g")
      dt_mod <- copy(dt)
    },
    expr = data.table:::`[.data.table`(dt_mod, , N := .N, by = g),
    Before = "be2f72e6f5c90622fe72e1c315ca05769a9dc854", # Parent of the regression causing commit (https://github.com/Rdatatable/data.table/commit/e793f53466d99f86e70fc2611b708ae8c601a451) in the PR that introduced the issue (https://github.com/Rdatatable/data.table/pull/4491/commits)
    Regression = "e793f53466d99f86e70fc2611b708ae8c601a451", # Commit responsible for regression in the PR that introduced the issue (https://github.com/Rdatatable/data.table/pull/4491/commits)
    Fixed = "58409197426ced4714af842650b0cc3b9e2cb842"), # Last commit in the PR that fixed the regression (https://github.com/Rdatatable/data.table/pull/5463/commits)

  # Issue reported in: https://github.com/Rdatatable/data.table/issues/5426
  # To be fixed in: https://github.com/Rdatatable/data.table/pull/5427
  "setDT improved in #5427" = atime::atime_test(
    N = 10^seq(1, 7),
    setup = {
      L <- replicate(N, 1, simplify = FALSE)
      setDT(L)
    },
    expr = {
      data.table:::setattr(L, "class", NULL)
      data.table:::setDT(L)
    },
    Slow = "c4a2085e35689a108d67dacb2f8261e4964d7e12", # Parent of the first commit in the PR that fixes the issue (https://github.com/Rdatatable/data.table/commit/7cc4da4c1c8e568f655ab5167922dcdb75953801)
    Fast = "1872f473b20fdcddc5c1b35d79fe9229cd9a1d15"), # Last commit in the PR that fixes the issue (https://github.com/Rdatatable/data.table/pull/5427/commits)

  # Issue reported in: https://github.com/Rdatatable/data.table/issues/4200
  # To be fixed in: https://github.com/Rdatatable/data.table/pull/4558
  "DT[by] fixed in #4558" = atime::atime_test(
    N = 10^seq(1, 20),
    setup = {
      d <- data.table(
        id3 = sample(c(seq.int(N*0.9), sample( N*0.9, N*0.1, TRUE))),
        v1 = sample(5L, N, TRUE),
        v2 = sample(5L, N, TRUE)
      )
    },
    expr = {
      expr=data.table:::`[.data.table`(d, , max(v1) - min(v2), by = id3)
    },
    Before = "7a9eaf62ede487625200981018d8692be8c6f134", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/515de90a6068911a148e54343a3503043b8bb87c) in the PR (https://github.com/Rdatatable/data.table/pull/4164/commits) that introduced the regression
    Regression = "c152ced0e5799acee1589910c69c1a2c6586b95d", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/15f0598b9828d3af2eb8ddc9b38e0356f42afe4f) in the PR (https://github.com/Rdatatable/data.table/pull/4558/commits) that fixes the regression
    Fixed = "f750448a2efcd258b3aba57136ee6a95ce56b302"), # Second commit of the PR (https://github.com/Rdatatable/data.table/pull/4558/commits) that fixes the regression

  # Issue with sorting again when already sorted: https://github.com/Rdatatable/data.table/issues/4498
  # Fixed in: https://github.com/Rdatatable/data.table/pull/4501
  "DT[,.SD] improved in #4501" = atime::atime_test(
    N = 10^seq(1, 10, by=0.5),
    setup = {
      set.seed(1)
      L = as.data.table(as.character(rnorm(N, 1, 0.5)))
      setkey(L, V1)
    },
    ## New DT can safely retain key.
    expr = {
      data.table:::`[.data.table`(L, , .SD)
    },
    Fast = "353dc7a6b66563b61e44b2fa0d7b73a0f97ca461", # Close-to-last merge commit in the PR (https://github.com/Rdatatable/data.table/pull/4501/commits) that fixes the issue 
    Slow = "3ca83738d70d5597d9e168077f3768e32569c790", # Circa 2024 master parent of close-to-last merge commit (https://github.com/Rdatatable/data.table/commit/353dc7a6b66563b61e44b2fa0d7b73a0f97ca461) in the PR (https://github.com/Rdatatable/data.table/pull/4501/commits) that fixes the issue 
    Slower = "cacdc92df71b777369a217b6c902c687cf35a70d"), # Circa 2020 parent of the first commit (https://github.com/Rdatatable/data.table/commit/74636333d7da965a11dad04c322c752a409db098) in the PR (https://github.com/Rdatatable/data.table/pull/4501/commits) that fixes the issue 

  # Issue reported in: https://github.com/Rdatatable/data.table/issues/6286
  # Fixed in: https://github.com/Rdatatable/data.table/pull/6296
  "DT[by, verbose = TRUE] improved in #6296" = atime::atime_test(
    N = 10^seq(1, 9),
    setup = {
      dt = data.table(a = 1:N)
      dt_mod <- copy(dt)
    },
    expr = data.table:::`[.data.table`(dt_mod, , 1, by = a, verbose = TRUE),
    Slow = "a01f00f7438daf4612280d6886e6929fa8c8f76e", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/fc0c1e76408c34a8482f16f7421d262c7f1bde32) in the PR (https://github.com/Rdatatable/data.table/pull/6296/commits) that fixes the issue
    Fast = "f248bbe6d1204dfc8def62328788eaadcc8e17a1"), # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/6296) that fixes the issue

  NULL)
# nolint end: undesirable_operator_linter.
