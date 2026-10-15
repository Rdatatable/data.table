pkg.path <- "~/R/data.table"
Test <- "fread N=cols regression"
library(data.table)
if(FALSE){
  remotes::install_github("tdhock/atime@bisect")
  atime::atime_versions_remove("data.table")
}
library(data.table)
(bisect.dt <- rowwiseDT(
      old=,     new=, label=, expected=, stage=,
  "1.14.8", "1.18.6", "PR7406", "?", "discovery",
  "PR7406", "1.18.6", "PR7370", "?", "discovery",
  "1.14.8", "PR7383", "PR4164", "?", "discovery",
  "PR7375", "1.18.6", "PR7370", "PR7370", "final",
  "1.14.8", "Parent7370", "PR7375", "PR7375", "final"))
for(bisect.i in 1:nrow(bisect.dt)){
  print(bisect.row <- bisect.dt[bisect.i])
  bisect.row[, atime::bisect(pkg.path, Test, old, new)]
}
