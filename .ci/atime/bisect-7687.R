pkg.path <- "~/R/data.table"
Test <- "DT[by] fixed in #4558"
library(data.table)
if(FALSE){
  atime::atime_versions_remove("data.table")
}
## These are the bisect results visualized on
## https://tdhock.github.io/2026-10-13-bisect-performance-pr7912
(bisect.dt <- rowwiseDT(
  old=, new=, label=,
  "PR4558", "Parent5463", "PR4491",
  "PR4164", "Parent7401", "PR5463",
  "Before4164", "Parent4558", "PR4164",#returns PR4164, consistent with https://github.com/Rdatatable/data.table/issues/4200#issuecomment-646111420
  "PR4558", "Fast", "PR7401",#returns PR7401, consistent with https://github.com/Rdatatable/data.table/issues/7687#issuecomment-4162931822
  "PR4164", "Fast", "PR7401",#two speed increases between these two commits, bisect finds the larger one: PR7401.
  "PR4491", "Parent7401", "PR5463",#one speed increase
  "PR4164", "Parent4491", "PR4558"))#one speed increase
for(bisect.i in 1:nrow(bisect.dt)){
  print(bisect.row <- bisect.dt[bisect.i])
  bisect.row[, atime::bisect(pkg.path, Test, old, new)]
}

