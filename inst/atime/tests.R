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
    Before = "9d3b9202fddb980345025a4f6ac451ed26a423be", # This needs to be changed later. Currently assigned to the merge commit in the PR that fixed the regression (https://github.com/Rdatatable/data.table/pull/4440) as the source of regression (or the particular commit that led to it) is not clear. In addition, older versions of data.table are having problems when being installed in this manner. (This includes commits from before Mar 20, 2020 or when the issue that discovered or first mentioned the regression was created)
    Regression = "b1b1832b0d2d4032b46477d9fe6efb15006664f4", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/0f0e7127b880df8459b0ed064dc841acd22f5b73) in the PR (https://github.com/Rdatatable/data.table/pull/4440/commits) that fixes the regression
    Fixed = "9d3b9202fddb980345025a4f6ac451ed26a423be"), # Merge commit in the PR that fixed the regression (https://github.com/Rdatatable/data.table/pull/4440)

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
    Before = "19b7866112614db53eb3e909c097407d91cd6738", # Parent of the regression commit (https://github.com/Rdatatable/data.table/commit/0895fa247afcf6b38044bd5f56c0d209691ddb31), which is the parent of the first commit in the PR that causes the issue (https://github.com/Rdatatable/data.table/pull/5493/commits) 
    Regression = "0895fa247afcf6b38044bd5f56c0d209691ddb31", # The regression commit is the parent of the first commit in the PR that fixed the issue (https://github.com/Rdatatable/data.table/pull/5493/commits)
    Fixed = "1e03fe7b890e63da9651d997ea52548c90b3ae32") # Last commit in the PR that fixed the regression (https://github.com/Rdatatable/data.table/pull/5493/commits)    
)
