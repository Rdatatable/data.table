## This script is for running the git bisect analysis in PR7907, which
## revealed two slowdowns in fread().
pkg.path <- "~/R/data.table"
Test <- "fread N=cols regression"
library(data.table)
if(FALSE){
  ## atime functions for git bisect were added in this branch:
  remotes::install_github("tdhock/atime@bisect")
  ## if you want to start the bisect over from scratch, first remove
  ## all historical versions of data.table installed by atime:
  atime::atime_versions_remove("data.table")
}
## Commits other than Fast and Slow have been commented in tests.R so
## to re-run the bisect, you would first have to uncomment them in
## tests.R. These are the commits visualized on
## https://tdhock.github.io/2026-10-15-bisect-performance-pr7907
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
