pval.thresh <- 0.001 # to reduce false positives.

# Test case adapted from https://github.com/Rdatatable/data.table/issues/6105#issue-2268691745 which is where the issue was reported.
# https://github.com/Rdatatable/data.table/pull/6107 fixed performance across 3 ways to specify a column as Date, and we test each individually.
extra.args.6107 <- c(
  "colClasses=list(Date='date')",
  "colClasses='Date'",
  "select=list(Date='date')")
extra.test.list <- list()
for (extra.arg in extra.args.6107){
  this.test <- atime::atime_test(
    FasterIO = "b70d0267ae89d3fffe8f4a5a6041dcb131709e97", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/6925) that reduced time usage
    Slow = "e9087ce9860bac77c51467b19e92cf4b72ca78c7", # Parent of the merge commit (https://github.com/Rdatatable/data.table/commit/a77e8c22e44e904835d7b34b047df2eff069d1f2) of the PR (https://github.com/Rdatatable/data.table/pull/6107) that fixes the issue
    Fast = "a77e8c22e44e904835d7b34b047df2eff069d1f2", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/6107) that fixes the issue
    setup = {
      set.seed(1)
      DT = data.table(date=.Date(sample(20000, N, replace=TRUE)))
      tmp_csv = tempfile()
      fwrite(DT, tmp_csv)
    })
  this.test$expr = str2lang(sprintf("data.table::fread(tmp_csv, %s)", extra.arg))
  extra.test.list[[sprintf("fread(%s) improved in #6107", extra.arg)]] <- this.test
}

# Test case adapted from https://github.com/Rdatatable/data.table/pull/4386#issue-602528139 which is where the performance was improved.
for(retGrp_chr in c("T","F"))extra.test.list[[sprintf(
  "forderv(retGrp=%s) improved in #4386", retGrp_chr
)]] <- list(
  ## From ?bench::mark, "Each expression will always run at least twice,
  ## once to measure the memory allocation and store results
  ## and one or more times to measure timing."
  ## So for atime(times=10) that means 11 times total.
  ## First time for memory allocation measurement,
  ## (also sets the index of dt in this example),
  ## then 10 more times for time measurement.
  ## Timings should be constant if the cached index is used (Fast),
  ## and (log-)linear if the index is re-computed (Slow).
  Slow = "b1b1832b0d2d4032b46477d9fe6efb15006664f4", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/b0efcf59442a7d086c6df17fa6a45c81b082322e) in the PR (https://github.com/Rdatatable/data.table/pull/4386/commits) where the performance was improved.
  Fast = "1a84514f6d20ff1f9cc614ea9b92ccdee5541506", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/4386/commits) where the performance was improved.
  setup = quote({
    dt <- data.table(group = rep(1:2, l=N))
  }),
  expr = substitute({
    old.opt <- options(datatable.forder.auto.index = TRUE) # required for test, un-documented, comments in forder.c say it is for debugging only.
    data.table:::forderv(dt, "group", retGrp = RETGRP)
    options(old.opt) # so the option does not affect other tests.
  }, list(RETGRP=eval(str2lang(retGrp_chr))))
)

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
  # Common N and pkg.edit.fun are defined here, and inherited in all test cases below which do not re-define them.
  N = as.integer(10^seq(1, 7, by=0.5)),
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
    # require C<23 for empty prototype declarations to work, #7689
    descfile = file.path(new.pkg.path, "DESCRIPTION")
    desc = as.data.frame(read.dcf(descfile))
    desc$SystemRequirements = paste(
      c(desc$SystemRequirements, "USE_C99"),
      collapse = "; ")
    write.dcf(desc, descfile)
    # allow compilation on new R versions where 'Calloc' is not defined
    pkg_find_replace(
      file.path("src", "*.c"),
      "\\b(Calloc|Free|Realloc)\\b",
      "R_\\1")
    pkg_find_replace(
      "NAMESPACE",
      sprintf('useDynLib\\("?%s"?', Package_regex),
      paste0('useDynLib(', new.Package_))
  },

  # Constant overhead improvement https://github.com/Rdatatable/data.table/pull/6925
  # Test case adapted from https://github.com/Rdatatable/data.table/pull/7022#discussion_r2107900643
  "fread disk overhead improved in #6925" = atime::atime_test(
    N = 2^seq(0, 20), # smaller N because we are doing multiple fread calls.
    setup = {
      fwrite(iris[1], iris.csv <- tempfile())
    },
    Fast = "b70d0267ae89d3fffe8f4a5a6041dcb131709e97", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/6925) that reduced time usage
    Slow = "e25ea80b793165094cea87d946d2bab5628f70a6", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/60a01fa65191c44d7997de1843e9a1dfe5be9f72)
    expr = replicate(N, data.table::fread(iris.csv))
  ),

  # Performance regression discussed in https://github.com/Rdatatable/data.table/issues/4311
  # Test case adapted from https://github.com/Rdatatable/data.table/pull/4440#issuecomment-632842980 which is the fix PR.
  "shallow regression fixed in #4440" = atime::atime_test(
    setup = {
      set.seed(1L)
      dt <- data.table(a = sample.int(N))
      setindexv(dt, "a")
    },
    # Before = "", This needs to be updated later as there are two issues here: A) The source of regression (or the particular commit that led to it) is not clear; B) Older versions of data.table are having problems when being installed in this manner (This includes commits from before March 20 2020, when the issue that discovered or first mentioned the regression was created)
    Regression = "b1b1832b0d2d4032b46477d9fe6efb15006664f4", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/0f0e7127b880df8459b0ed064dc841acd22f5b73) in the PR (https://github.com/Rdatatable/data.table/pull/4440/commits) that fixes the regression
    Fixed = "9d3b9202fddb980345025a4f6ac451ed26a423be", # Merge commit in the PR that fixed the regression (https://github.com/Rdatatable/data.table/pull/4440)
    expr = data.table:::shallow(dt)),

  # Test based on https://github.com/Rdatatable/data.table/issues/5424
  # Performance regression introduced from a commit in https://github.com/Rdatatable/data.table/pull/4491
  # Test case adapted from https://github.com/Rdatatable/data.table/pull/5463#issue-1373642456 which is the fix PR.
  "memrecycle regression fixed in #5463" = atime::atime_test(
    setup = {
      bigN <- N*100
      set.seed(2L)
      dt <- data.table(
        g = sample(seq_len(N), bigN, TRUE),
        x = runif(bigN),
        key = "g")
      dt_mod <- copy(dt)
    },
    Before = "d47a83fb2e25582e508f191f87a31ca81b736b57", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/196f420b50181b92036538776956ddf2c5b7a5a1) in the PR (https://github.com/Rdatatable/data.table/pull/4491/commits) that introduced the issue
    Regression = "85adf09e3463838d547977ae9bc75e3b37f9cbaf", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/4491) that introduced the issue
    Fixed = "19b7866112614db53eb3e909c097407d91cd6738", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/5463) that fixed the regression
    expr = data.table:::`[.data.table`(dt_mod, , N := .N, by = g)),

  # Issue reported in https://github.com/Rdatatable/data.table/issues/5426
  # Test case adapted from https://github.com/Rdatatable/data.table/pull/5427#issue-1323678063 which is the fix PR.
  "setDT improved in #5427" = atime::atime_test(
    setup = {
      L <- replicate(N, 1, simplify = FALSE)
      setDT(L)
    },
    Slow = "c4a2085e35689a108d67dacb2f8261e4964d7e12", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/7cc4da4c1c8e568f655ab5167922dcdb75953801) in the PR (https://github.com/Rdatatable/data.table/pull/5427/commits) that fixes the issue
    Fast = "2487c61656335764980e478c323f7e6ce4e6d4ca", # Merge commit in the PR (https://github.com/Rdatatable/data.table/pull/5427) that fixes the issue
    expr = {
      data.table:::setattr(L, "class", NULL)
      data.table:::setDT(L)
    }),

  # Test case adapted from https://github.com/Rdatatable/data.table/issues/4200#issuecomment-645980224 which is where the issue was reported.
  # Fixed in https://github.com/Rdatatable/data.table/pull/4558
  "DT[by] fixed in #4558" = atime::atime_test(
    setup = {
      N9 <- as.integer(N * 0.9)
      d <- data.table(
        id = sample(c(seq.int(N9), sample(N9, N-N9, TRUE))),
        v1 = sample(5L, N, TRUE),
        v2 = sample(5L, N, TRUE)
      )
    },
    PR7401="0216983c51e03e3f61d5e6f08f4ba0c42cceb22c", # Merge commit (https://github.com/Rdatatable/data.table/commit/0216983c51e03e3f61d5e6f08f4ba0c42cceb22c) of a PR (https://github.com/Rdatatable/data.table/pull/7401) which increased speed and memory usage of this test (https://github.com/Rdatatable/data.table/issues/7687)
    Before = "7a9eaf62ede487625200981018d8692be8c6f134", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/515de90a6068911a148e54343a3503043b8bb87c) in the PR (https://github.com/Rdatatable/data.table/pull/4164/commits) that introduced the regression
    Regression = "c152ced0e5799acee1589910c69c1a2c6586b95d", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/15f0598b9828d3af2eb8ddc9b38e0356f42afe4f) in the PR (https://github.com/Rdatatable/data.table/pull/4558/commits) that fixes the regression
    Fixed = "ba32f3cba38ec270587e395f6e6c26a80be36be6", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/4558) that fixes the regression
    expr = data.table:::`[.data.table`(d, , max(v1) - min(v2), by = id)),

  # Issue with sorting again when already sorted, as reported in https://github.com/Rdatatable/data.table/issues/4498
  # Test case adapted from https://github.com/Rdatatable/data.table/pull/4501#issue-625311918 which is the fix PR.
  "DT[,.SD] improved in #4501" = atime::atime_test(
    setup = {
      set.seed(1)
      L = as.data.table(as.character(rnorm(N, 1, 0.5)))
      setkey(L, V1)
    },
    Fast = "680b5e8e6d3f16a09dfb2f86ac7b2ce5ce70c3f1", # Merge commit in the PR (https://github.com/Rdatatable/data.table/pull/4501/commits) that fixes the issue 
    Slow = "3ca83738d70d5597d9e168077f3768e32569c790", # Circa 2024 master parent of close-to-last merge commit (https://github.com/Rdatatable/data.table/commit/353dc7a6b66563b61e44b2fa0d7b73a0f97ca461) in the PR (https://github.com/Rdatatable/data.table/pull/4501/commits) that fixes the issue 
    Slower = "cacdc92df71b777369a217b6c902c687cf35a70d", # Circa 2020 parent of the first commit (https://github.com/Rdatatable/data.table/commit/74636333d7da965a11dad04c322c752a409db098) in the PR (https://github.com/Rdatatable/data.table/pull/4501/commits) that fixes the issue
    ## New DT can safely retain key.
    expr = data.table:::`[.data.table`(L, , .SD)),

  # Test case adapted from https://github.com/Rdatatable/data.table/issues/6286#issue-2412141289 which is where the issue was reported.
  # Fixed in https://github.com/Rdatatable/data.table/pull/6296
  "DT[by,verbose=TRUE] improved in #6296" = atime::atime_test(
    setup = {
      dt = data.table(a = 1:N)
      dt_mod <- copy(dt)
    },
    Slow = "a01f00f7438daf4612280d6886e6929fa8c8f76e", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/fc0c1e76408c34a8482f16f7421d262c7f1bde32) in the PR (https://github.com/Rdatatable/data.table/pull/6296/commits) that fixes the issue
    Fast = "f248bbe6d1204dfc8def62328788eaadcc8e17a1", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/6296) that fixes the issue
    expr = data.table:::`[.data.table`(dt_mod, , 1, by = a, verbose = TRUE)),

  # Test case adapted from https://github.com/Rdatatable/data.table/issues/5492#issue-1416598382 which is where the issue was reported,
  # and from https://github.com/Rdatatable/data.table/pull/5493#issue-1416656788 which is the fix PR.
  "transform improved in #5493" = atime::atime_test(
    setup = {
      df <- data.frame(x = runif(N))
      dt <- as.data.table(df)
    },
    Slow = "0895fa247afcf6b38044bd5f56c0d209691ddb31", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/93ce3ce1373bf733ebd2036e2883d2ffe377ab58) in the PR (https://github.com/Rdatatable/data.table/pull/5493/commits) that fixes the issue
    Fast = "2d1a0575f87cc50e90f64825c30d7a6cb6b05dd7", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/5493) that fixes the issue
    expr = data.table:::transform.data.table(dt, y = round(x))),

  # Test case created directly using the atime code below (not adapted from any other benchmark), based on the issue/fix PR https://github.com/Rdatatable/data.table/pull/5054#issue-930603663 "melt should be more efficient when there are missing input columns."
  "melt improved in #5054" = atime::atime_test(
    setup = {
      DT <- as.data.table(as.list(1:N))
      measure.vars <- lapply(1:N, function(i) {
        x = rep(NA, N)
        x[i] = i
        x
      })  
    },
    Slow = "fd24a3105953f7785ea7414678ed8e04524e6955", # Parent of the merge commit (https://github.com/Rdatatable/data.table/commit/ed72e398df76a0fcfd134a4ad92356690e4210ea) of the PR (https://github.com/Rdatatable/data.table/pull/5054) that fixes the issue
    Fast = "ed72e398df76a0fcfd134a4ad92356690e4210ea", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/5054) that fixes the issue
    expr = data.table:::melt(DT, measure.vars = measure.vars)),

  # Test case created from @tdhock's comment https://github.com/Rdatatable/data.table/pull/6393#issuecomment-2327396833, in turn adapted from @philippechataignon's comment https://github.com/Rdatatable/data.table/pull/6393#issuecomment-2326714012
  "fwrite refactored in #6393" = atime::atime_test(
    setup = {
      set.seed(1)
      NC = 10L
      L <- data.table(i=1:N)
      L[, paste0("V", 1:NC) := replicate(NC, rnorm(N), simplify=FALSE)]
      out.csv <- tempfile()
    },
    Before = "f339aa64c426a9cd7cf2fcb13d91fc4ed353cd31", # Parent of the first commit https://github.com/Rdatatable/data.table/commit/fcc10d73a20837d0f1ad3278ee9168473afa5ff1 in the PR https://github.com/Rdatatable/data.table/pull/6393/commits with major change to fwrite with gzip.
    PR = "e0abdfcd79ba30efcf679e33cbb8eba897a46f9c", # merge commit of PR6393
    expr = data.table::fwrite(L, out.csv, compress="gzip")),

  # Test case created directly using the atime code below (not adapted from any other benchmark), based on the PR, Removes unnecessary data.table call from as.data.table.array https://github.com/Rdatatable/data.table/pull/7019
  "as.data.table.array improved in #7019" = atime::atime_test(
    setup = {
      dims = c(N, 1, 1)
      arr = array(seq_len(prod(dims)), dim=dims)
    },
    Slow = "73d79edf8ff8c55163e90631072192301056e336",  # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/8397dc3c993b61a07a81c786ca68c22bc589befc) in the PR that improves efficiency
    Fast = "2715663fcf0344c3f7c73241d391d8de347bdb9d",  # Merge commit of the PR that improves efficiency
    expr = data.table:::as.data.table.array(arr, na.rm=FALSE)),

  # https://github.com/Rdatatable/data.table/pull/7144 added the speedup code and this performance test.
  "isoweek improved in #7144" = atime::atime_test(
    setup = {
      set.seed(349)
      x = sample(Sys.Date() - 0:5000, N, replace=TRUE)
    },
    Slow = "038e7f8c2bed60f38c3faa2cc2c4e339c3570b94", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/c0b32a60466bed0e63420ec105bc75c34590865e) in the PR
    Fast = "ed2df986da6d3a4ff35bec1b0f75db2b767e3eb2", # Merge commit of the PR that uses a much faster implementation
    expr = data.table::isoweek(x)),

  # Regression introduced by https://github.com/Rdatatable/data.table/pull/6890 and discussed in https://github.com/Rdatatable/data.table/issues/7404 (grouped by factor).
  "DT[by] max regression fixed in #7480" = atime::atime_test(
    setup = {
      dt = data.table(
        id = as.factor(rep(seq_len(N), each = 100L)),
        V1 = 1L
      )
    },
    Before = "2cb03162a21328cc5f68a8c3b0e554f5edfcb5b9", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/4cc77c617435b46a0faac35c56e7fb7b81c629fc) in the regression PR (https://github.com/Rdatatable/data.table/pull/6890/commits)
    Regression = "6f49bf1935a3009e85ea1e6f9752ff68ffa47d9b", # Merge commit of the regression PR https://github.com/Rdatatable/data.table/pull/6890
    Fixed = "b6ad1a4bc2e44d47f3e86c296c924a809a26bf58", # Merge commit of the fix PR (https://github.com/Rdatatable/data.table/pull/7480)
    seconds.limit = 1,
    expr = data.table:::`[.data.table`(dt, , base::max(V1, na.rm = TRUE), by = id)),

  tests=extra.test.list)
# nolint end: undesirable_operator_linter.
