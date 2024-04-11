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
    }

test.list <- list(
  # Performance regression discussed in: https://github.com/Rdatatable/data.table/issues/4311 
  # Fixed in: https://github.com/Rdatatable/data.table/pull/4440
  "Test regression fixed in #4440" = list(
    pkg.edit.fun = pkg.edit.fun,
    N = 10^seq(3,8),
    setup = quote({
      set.seed(1L)
      dt <- data.table(a = sample(N, N))
      setindex(dt, a)
    }),
    expr = quote(data.table:::shallow(dt)),
    "Before" = "9d3b9202fddb980345025a4f6ac451ed26a423be", # This should be changed later. Currently, the source of regression (or the particular commit that led to it) is not clear. In addition, older versions of data.table are having problems when being installed in this manner. (This includes commits from before Mar 20, 2020 or when the issue that discovered or first mentioned the regression was created)
    "Regression" = "752012f577f8e268bb6d0084ca39a09fa7fbc1c4", # A commit that is affected by the regression: https://github.com/Rdatatable/data.table/commit/752012f577f8e268bb6d0084ca39a09fa7fbc1c4
    "Fixed" = "9d3b9202fddb980345025a4f6ac451ed26a423be"), # The merge commit in #4440, the PR that fixed the regression: https://github.com/Rdatatable/data.table/commit/9d3b9202fddb980345025a4f6ac451ed26a423be

  # Test based on: https://github.com/Rdatatable/data.table/issues/5424
  # Performance regression introduced from a commit in: https://github.com/Rdatatable/data.table/pull/4491
  # Fixed in: https://github.com/Rdatatable/data.table/pull/5463    
  "Test regression fixed in #5463" = list(
    pkg.edit.fun = pkg.edit.fun,
    N = 10^seq(3, 8),
    expr = quote(data.table:::`[.data.table`(dt_mod, , N := .N, by = g)),
    setup = quote({
      n <- N/100
      set.seed(1L)
      dt <- data.table(
        g = sample(seq_len(n), N, TRUE),
        x = runif(N),
        key = "g")
      dt_mod <- copy(dt)
    }),
    "Before" = "be2f72e6f5c90622fe72e1c315ca05769a9dc854", # The commit in PR #4491 that comes before the regression introducting commit: https://github.com/Rdatatable/data.table/pull/4491/commits/be2f72e6f5c90622fe72e1c315ca05769a9dc854
    "Regression" = "e793f53466d99f86e70fc2611b708ae8c601a451", # The commit in #4491 that introduced the regression: https://github.com/Rdatatable/data.table/pull/4491/commits/e793f53466d99f86e70fc2611b708ae8c601a451
    "Fixed" = "58409197426ced4714af842650b0cc3b9e2cb842") # Last commit in #5463, the PR that fixed the regression: https://github.com/Rdatatable/data.table/pull/5463/commits/58409197426ced4714af842650b0cc3b9e2cb842     
)