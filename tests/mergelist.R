Sys.setenv(OMP_THREAD_LIMIT = Sys.getenv("OMP_THREAD_LIMIT", "2"))
require(data.table)
test.data.table(script="mergelist.Rraw")
