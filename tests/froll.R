Sys.setenv(OMP_THREAD_LIMIT = Sys.getenv("OMP_THREAD_LIMIT", "2"))
require(data.table)
test.data.table(script="froll.Rraw")
test.data.table(script="frollBatch.Rraw", optional=TRUE)
