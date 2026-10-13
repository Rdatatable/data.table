if(FALSE){
  remotes::install_github("tdhock/atime@bisect")
}
pkg.path <- "~/R/data.table"
Test <- "DT[by] fixed in #4558"
library(data.table)
(bisect.dt <- rowwiseDT(
  old=, new=, label=,
  "Before", "Parent4558", "PR4164",#returns PR4164, consistent with https://github.com/Rdatatable/data.table/issues/4200#issuecomment-646111420
  "PR4558", "Fast", "PR7401",#returns PR7401, consistent with https://github.com/Rdatatable/data.table/issues/7687#issuecomment-4162931822
  "PR4164", "Fast", "PR7401",#two speed increases between these two commits, bisect finds the larger one: PR7401.
  "PR4164", "Parent7401", "PR5463",#should find one small speed increase in PR4558? Finds PR5463??
  ## "PR7401", "Fast", "",#no speed changes, returns PR7650, but atime plot clearly shows no real change.
  ## "PR5463", "Parent7401", "TODO",#slight speed decrease.
  "PR4164", "Parent5463", "PR4491",#expected speed increase, found PR4491.
  "PR4164", "Parent4491", "PR4558"))#expected speed increase, found PR4558. DONE.
for(bisect.i in 1:nrow(bisect.dt)){
  print(bisect.row <- bisect.dt[bisect.i])
  bisect.row[, atime::bisect(pkg.path, Test, old, new, reps=1)]
}

