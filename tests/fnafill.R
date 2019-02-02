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
.test.fnafill.failed = numeric()
test = function(num, ...) {
  ans = data.table:::test(num, ...)
  if (!ans) .test.fnafill.failed<<-c(.test.fnafill.failed, num)
  invisible(ans)
}
oldDTthreads = setDTthreads(1) # mimics CRAN check, tested also on 4 cores

x = 1:10
x[c(1:2, 5:6, 9:10)] = NA
test(9999.01, fnafill(x, "locf"), INT(NA,NA,3,4,4,4,7,8,8,8))
test(9999.02, fnafill(x, "nocb"), INT(3,3,3,4,7,7,7,8,NA,NA))
test(9999.03, fnafill(x, fill=0L), INT(0,0,3,4,0,0,7,8,0,0))
test(9999.04, fnafill(x, fill=5), INT(5,5,3,4,5,5,7,8,5,5))
test(9999.05, fnafill(x, fill=NA_integer_), x)
test(9999.06, fnafill(x, fill=NA), x)
test(9999.07, fnafill(x, fill=NA_real_), x)
test(9999.08, fnafill(x, fill=Inf), x)
test(9999.09, fnafill(x, fill=NaN), x)
y = x/2
test(9999.11, fnafill(y, "locf"), c(NA,NA,3,4,4,4,7,8,8,8)/2)
test(9999.12, fnafill(y, "nocb"), c(3,3,3,4,7,7,7,8,NA,NA)/2)
test(9999.13, fnafill(y, fill=0L), c(0,0,3,4,0,0,7,8,0,0)/2)
test(9999.14, fnafill(y, fill=5/2), c(5,5,3,4,5,5,7,8,5,5)/2)
test(9999.15, fnafill(y, fill=NA_integer_), y)
test(9999.16, fnafill(y, fill=NA), y)
test(9999.17, fnafill(y, fill=NA_real_), y)
test(9999.18, fnafill(y, fill=Inf), c(Inf,Inf,3,4,Inf,Inf,7,8,Inf,Inf)/2)
test(9999.19, fnafill(y, fill=NaN), c(NaN,NaN,3,4,NaN,NaN,7,8,NaN,NaN)/2)
z = y
z[5L] = NaN
z[2L] = Inf
z[9L] = -Inf
test(9999.21, fnafill(z, "locf"), c(NA,Inf,3,4,NaN,NaN,7,8,-Inf,-Inf)/2)
test(9999.22, fnafill(z, "nocb"), c(Inf,Inf,3,4,NaN,7,7,8,-Inf,NA)/2)
dt = data.table(x, y, z)
test(9999.31, fnafill(dt, "locf"), unname(lapply(dt, fnafill, "locf")))
test(9999.32, fnafill(dt, "nocb"), unname(lapply(dt, fnafill, "nocb")))
test(9999.33, fnafill(dt, fill=0), unname(lapply(dt, fnafill, fill=0)))
l = list(x, y[1:8], z[1:6])
test(9999.41, fnafill(l, "locf"), lapply(l, fnafill, "locf"))
test(9999.42, fnafill(l, "nocb"), lapply(l, fnafill, "nocb"))
test(9999.43, fnafill(l, fill=0), lapply(l, fnafill, fill=0))

setDTthreads(oldDTthreads)
if (length(.test.fnafill.failed)) stop(sprintf("fnafill unit tests failed: %s.", paste(.test.fnafill.failed, collapse=", "))) else cat("fnafill unit tests successfully passed\n")
