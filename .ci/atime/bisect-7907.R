pkg.path <- "~/R/data.table"
Test <- "fread N=cols regression"
library(data.table)
if(FALSE){
  remotes::install_github("tdhock/atime@bisect")
  atime::atime_versions_remove("data.table")
  tinfo <- atime::atime_pkg_test_info(pkg.path)
  tcall <- tinfo$test.call[[Test]]
  tres <- eval(tcall)
  plot(tres)
}
library(data.table)
(bisect.dt <- rowwiseDT(
      old=,     new=, label=, expected=, stage=,
  "Fast", "Slow", "PR7406", "?", "discovery",
  "PR7406", "Slow", "PR7370", "?", "discovery",
  "Fast", "PR7383", "PR4164", "?", "discovery",
  "PR7375", "Slow", "PR7370", "PR7370", "final",
  "Fast", "Parent7370", "PR7375", "PR7375", "final"))
for(bisect.i in 1:nrow(bisect.dt)){
  print(bisect.row <- bisect.dt[bisect.i])
  bisect.row[, atime::bisect(pkg.path, Test, old, new)]
}
