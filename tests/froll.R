require(data.table)
minor_version = as.integer(strsplit(packageDescription("data.table")$Version, "\\.")[[1]][[3]])
if (minor_version == 99L || Sys.getenv("RunAllDataTableTests") == "yes") test.data.table(script="froll.Rraw")
