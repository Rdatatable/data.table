if(FALSE){
  remotes::install_github("tdhock/atime@bisect")
}
pkg.path <- "~/R/data.table"
Test <- "DT[by] fixed in #4558"
atime::bisect(pkg.path, Test, "Before", "Parent4558")#returns PR4164, consistent with https://github.com/Rdatatable/data.table/issues/4200#issuecomment-646111420
atime::bisect(pkg.path, Test, "PR4558", "Fast")#returns PR7401, consistent with https://github.com/Rdatatable/data.table/issues/7687#issuecomment-4162931822
atime::bisect(pkg.path, Test, "PR4164", "Fast")#two speed increases between these two commits, bisect finds the larger one: PR7401.
atime::bisect(pkg.path, Test, "PR4164", "Parent7401")#should find one small speed increase in PR4558? Finds PR5463??
atime::bisect(pkg.path, Test, "PR4164", "Parent5463")#expected speed increase, found PR4491.
atime::bisect(pkg.path, Test, "PR4164", "Parent4491")#expected speed increase, found PR4558. DONE.
atime::bisect(pkg.path, Test, "PR7401", "Fast")#no speed changes, returns PR7650, but atime plot clearly shows no real change.

