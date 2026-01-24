# Valgrind developers say that performing work after fork() without
# exec() is problematic for Valgrind. frollapply() uses
# parallel::mcparallel(), which causes Valgrind to run
# Rstd_ReadConsole() incorrectly.
Sys.setenv(OMP_THREAD_LIMIT = Sys.getenv("OMP_THREAD_LIMIT",
  if (grepl("valgrind", Sys.getenv("LD_PRELOAD"))) "1" else "2"
))
require(data.table)
test.data.table(script="froll.Rraw")
test.data.table(script="frollBatch.Rraw", optional=TRUE)
