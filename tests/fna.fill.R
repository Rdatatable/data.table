#require(methods)
#if (exists("test.data.table", .GlobalEnv, inherits=FALSE)) {
#  if (!identical(suppressWarnings(packageDescription("data.table")), NA)) {
#    remove.packages("data.table")
#    stop("This is dev mode but data.table was installed. Uninstalled it. Please q() this R session and try cc() again. The installed namespace causes problems in dev mode for the S4 tests.\n")
#  }
#  if ((tt<-compiler::enableJIT(-1))>0)
#    cat("This is dev mode and JIT is enabled (level ", tt, ") so there will be a brief pause around the first test.\n", sep="")
#} else {
#  require(data.table)
#  test = data.table:::test
#  INT = data.table:::INT
#}

## replace that chunk with the above when #3340 merged
library(data.table)
INT = data.table:::INT
.test.fna.fill.failed = numeric()
test = function(num, ...) {
  ans = data.table:::test(num, ...)
  if (!ans) .test.fna.fill.failed<<-c(.test.fna.fill.failed, num)
  invisible(ans)
}
oldDTthreads = setDTthreads(1) # mimics CRAN check, tested also on 4 cores

x = 1:10
x[c(1:2, 5:6, 9:10)] = NA
test(9999.01, fna.fill(x, "locf"), INT(NA,NA,3,4,4,4,7,8,8,8))
test(9999.02, fna.fill(x, "nocb"), INT(3,3,3,4,7,7,7,8,NA,NA))
test(9999.03, fna.fill(x, fill=0L), INT(0,0,3,4,0,0,7,8,0,0))
test(9999.04, fna.fill(x, fill=5), INT(5,5,3,4,5,5,7,8,5,5))
test(9999.05, fna.fill(x, fill=NA_integer_), x)
test(9999.06, fna.fill(x, fill=NA), x)
test(9999.07, fna.fill(x, fill=NA_real_), x)
test(9999.08, fna.fill(x, fill=Inf), x)
test(9999.09, fna.fill(x, fill=NaN), x)
y = x/2
test(9999.11, fna.fill(y, "locf"), c(NA,NA,3,4,4,4,7,8,8,8)/2)
test(9999.12, fna.fill(y, "nocb"), c(3,3,3,4,7,7,7,8,NA,NA)/2)
test(9999.13, fna.fill(y, fill=0L), c(0,0,3,4,0,0,7,8,0,0)/2)
test(9999.14, fna.fill(y, fill=5/2), c(5,5,3,4,5,5,7,8,5,5)/2)
test(9999.15, fna.fill(y, fill=NA_integer_), y)
test(9999.16, fna.fill(y, fill=NA), y)
test(9999.17, fna.fill(y, fill=NA_real_), y)
test(9999.18, fna.fill(y, fill=Inf), c(Inf,Inf,3,4,Inf,Inf,7,8,Inf,Inf)/2)
test(9999.19, fna.fill(y, fill=NaN), c(NaN,NaN,3,4,NaN,NaN,7,8,NaN,NaN)/2)
z = y
z[5L] = NaN
z[2L] = Inf
z[9L] = -Inf
test(9999.21, fna.fill(z, "locf"), c(NA,Inf,3,4,NaN,NaN,7,8,-Inf,-Inf)/2)
test(9999.22, fna.fill(z, "nocb"), c(Inf,Inf,3,4,NaN,7,7,8,-Inf,NA)/2)
dt = data.table(x, y, z)
test(9999.31, fna.fill(dt, "locf"), unname(lapply(dt, fna.fill, "locf")))
test(9999.32, fna.fill(dt, "nocb"), unname(lapply(dt, fna.fill, "nocb")))
test(9999.33, fna.fill(dt, fill=0), unname(lapply(dt, fna.fill, fill=0)))
l = list(x, y[1:8], z[1:6])
test(9999.41, fna.fill(l, "locf"), lapply(l, fna.fill, "locf"))
test(9999.42, fna.fill(l, "nocb"), lapply(l, fna.fill, "nocb"))
test(9999.43, fna.fill(l, fill=0), lapply(l, fna.fill, fill=0))

setDTthreads(oldDTthreads)
if (length(.test.fna.fill.failed)) stop(sprintf("fna.fill unit tests failed: %s.", paste(.test.fna.fill.failed, collapse=", "))) else cat("fna.fill unit tests successfully passed\n")
