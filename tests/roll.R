## this file will be migrated to inst/tests/tests.Rraw when branch will be ready to merge
if (!interactive()) {
  library(data.table)
  test = data.table:::test
}
oldDTthreads = setDTthreads(1)

## rolling features

#### multiple columns at once
d = as.data.table(list(1:6/2, 3:8/4))
ans = frollmean(d, 3)
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)),
  c(rep(NA_real_,2), seq(1,1.75,0.25))
)
test(9999.1, ans, expected)

#### multiple windows at once
ans = frollmean(d[, .(V1)], c(3, 4))
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)),
  c(rep(NA_real_,3), seq(1.25,2.25,0.5))
)
test(9999.2, ans, expected)

#### multiple columns and multiple windows at once
ans = frollmean(d, c(3, 4))
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)), c(rep(NA_real_,3), seq(1.25,2.25,0.5)),
  c(rep(NA_real_,2), seq(1,1.75,0.25)), c(rep(NA_real_,3), seq(1.125,1.625,0.25))
)
test(9999.3, ans, expected)

#### atomic vectors input and single window returns atomic vectors
ans = frollmean(d[["V1"]], 3)
expected = c(rep(NA_real_,2), seq(1,2.5,0.5))
test(9999.4, ans, expected)

#### various length list vectors
l = list(1:6/2, 3:10/4)
ans = frollmean(l, c(3, 4))
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)), c(rep(NA_real_,3), seq(1.25,2.25,0.5)),
  c(rep(NA_real_,2), seq(1,2.25,0.25)), c(rep(NA_real_,3), seq(1.125,2.125,0.25))
)
test(9999.5, ans, expected)

#### align: right/center/left
ans = frollmean(d, 3, align="right") # default
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)),
  c(rep(NA_real_,2), seq(1,1.75,0.25))
)
test(9999.6, ans, expected)
#ans = frollmean(d, 3, align="center") # x even, n odd
#expected = list(
#  c(NA_real_, seq(1,2.5,0.5), NA_real_),
#  c(NA_real_, seq(1,1.75,0.25), NA_real_)
#)
#test(9999.7, ans, expected)
#ans = frollmean(d, 4, align="center") # x even, n even
#expected = list(
#  c(NA_real_, seq(1.25,2.25,0.5), rep(NA_real_,2)),
#  c(NA_real_, seq(1.125,1.625,0.25), rep(NA_real_,2))
#)
#test(9999.8, ans, expected)
#de = rbind(d, data.table(3.5, 2.25))
#ans = frollmean(de, 3, align="center") # x odd, n odd
#expected = list(
#  c(NA_real_, seq(1,3,0.5), NA_real_),
#  c(NA_real_, seq(1,2,0.25), NA_real_)
#)
#test(9999.9, ans, expected)
#ans = frollmean(de, 4, align="center") # x odd, n even
#expected = list(
#  c(NA_real_, seq(1.25,2.75,0.5), rep(NA_real_,2)),
#  c(NA_real_, seq(1.125,1.875,0.25), rep(NA_real_,2))
#)
#test(9999.10, ans, expected)
#ans = frollmean(d, 3, align="left")
#expected = list(
#  c(seq(1,2.5,0.5), rep(NA_real_,2)),
#  c(seq(1,1.75,0.25), rep(NA_real_,2))
#)
#test(9999.11, ans, expected)

#### handling NAs
d = as.data.table(list(1:6/2, 3:8/4))
d[c(2L, 5L), V1:=NA][4:6, V2:=NA]
ans = frollmean(d, 2:3)
expected = list(c(NA, NA, NA, 1.75, NA, NA), rep(NA_real_, 6), c(NA, 0.875, 1.125, NA, NA, NA), c(NA, NA, 1, NA, NA, NA))
test(9999.99, ans, expected)
ans = frollmean(d, 2:3, na.rm=TRUE)
expected = list(c(NA, 0.5, 1.5, 1.75, 2, 3), c(NA, NA, 1, 1.75, 1.75, 2.5), c(NA, 0.875, 1.125, 1.25, NaN, NaN), c(NA, NA, 1, 1.125, 1.25, NaN))
test(9999.99, ans, expected)

#### fill constant
test(9999.99, frollmean(1:5, 4, fill=0), c(0, 0, 0, 2.5, 3.5))
test(9999.99, frollmean(1:5, 4, fill=-5), c(-5, -5, -5, 2.5, 3.5))
test(9999.99, frollmean(1:5, 4, fill=100), c(100, 100, 100, 2.5, 3.5))
test(9999.99, frollmean(1:5, 4, fill=Inf), c(Inf, Inf, Inf, 2.5, 3.5))
test(9999.99, frollmean(1:5, 4, fill=NaN), c(NaN, NaN, NaN, 2.5, 3.5))

#### adaptive window

## edge cases

#### length(x)==0
test(9999.99, frollmean(numeric(0), 2), numeric(0))
test(9999.99, frollmean(list(1:3, numeric()), 2), list(c(NA_real_, 1.5, 2.5), numeric(0)))

#### length(n)==0
test(9999.99, frollmean(1:3, integer()), error="n must be non 0 length")
test(9999.99, frollmean(list(1:3, 2:4), integer()), error="n must be non 0 length")

#### n==0
test(9999.99, frollmean(1:3, c(2,0)), error="n must be positive integer values")
test(9999.99, frollmean(list(1:3, 2:4), 0), error="n must be positive integer values")

#### n<0
test(9999.99, frollmean(1:3, -2), error="n must be positive integer values")

#### n[[1L]]>0 && n[[2L]]<0
test(9999.99, frollmean(1:3, c(2, -2)), error="n must be positive integer values")

#### n[[1L]]==n[[2L]]
test(9999.99, frollmean(1:3, c(2, 2)), list(c(NA_real_, 1.5, 2.5), c(NA_real_, 1.5, 2.5)))
test(9999.99, frollmean(list(1:3, 4:6), c(2, 2)), list(c(NA_real_, 1.5, 2.5), c(NA_real_, 1.5, 2.5), c(NA_real_, 4.5, 5.5), c(NA_real_, 4.5, 5.5)))

#### n>length(x)
test(9999.99, frollmean(list(1:3, 4:6), 4), list(c(NA_real_, NA_real_, NA_real_), c(NA_real_, NA_real_, NA_real_)))

#### n==length(x)
test(9999.99, frollmean(list(1:3, 4:6), 3), list(c(NA_real_, NA_real_, 2), c(NA_real_, NA_real_, 5)))

#### n<length(x[[1L]]) && n>length(x[[2L]])
test(9999.99, frollmean(list(1:5, 1:2), 3), list(c(NA_real_, NA_real_, 2, 3, 4), c(NA_real_, NA_real_)))

#### length(x)==1 && n==1
test(9999.99, frollmean(5, 1), 5)
test(9999.99, frollmean(list(1, 10, 5), 1), list(1, 10, 5))

#### length(x)==1 && n==2
test(9999.99, frollmean(5, 2), NA_real_)
test(9999.99, frollmean(list(1, 10, 5), 2), list(NA_real_, NA_real_, NA_real_))

#### n==Inf
test(9999.99, frollmean(1:5, Inf), error="n must be non-negative integer values", warning="NAs introduced by coercion to integer range")

#### n==c(5, Inf)
test(9999.99, frollmean(1:5, c(5, Inf)), error="n must be non-negative integer values", warning="NAs introduced by coercion to integer range")

#### is.complex(n)
#frollmean(1:5, 3i)

#### is.character(n)
#frollmean(1:5, "a")

#### is.factor(n)
#frollmean(1:5, as.factor("a"))

#### !adaptive && is.list(n)
#frollmean(11:15, list(1:5), adaptive=FALSE)

#### adaptive && is.integer(n)
#frollmean(11:15, 1:5, adaptive=TRUE)

#### adaptive && is.integer(n) && length(n)!=length(x)
#frollmean(11:15, 1:5, adaptive=TRUE)

#### adaptive && is.list(n) && length(n[[1L]])!=length(x)
#frollmean(11:15, list(1:4), adaptive=TRUE)

## validation

#### against zoo
if (requireNamespace("zoo", quietly=TRUE)) {
  set.seed(5)

  #### align
  x = rnorm(1e3) # x even, n even
  test(9999.51, frollmean(x, 50), zoo::rollmean(x, 50, fill=NA, align="right"))
  #test(9999.52, frollmean(x, 50, align="center"), zoo::rollmean(x, 50, fill=NA))
  #test(9999.53, frollmean(x, 50, align="left"), zoo::rollmean(x, 50, fill=NA, align="left"))
  x = rnorm(1e3+1) # x odd, n even
  test(9999.54, frollmean(x, 50), zoo::rollmean(x, 50, fill=NA, align="right"))
  #test(9999.55, frollmean(x, 50, align="center"), zoo::rollmean(x, 50, fill=NA))
  #test(9999.56, frollmean(x, 50, align="left"), zoo::rollmean(x, 50, fill=NA, align="left"))
  x = rnorm(1e3) # x even, n odd
  test(9999.57, frollmean(x, 51), zoo::rollmean(x, 51, fill=NA, align="right"))
  #test(9999.58, frollmean(x, 51, align="center"), zoo::rollmean(x, 51, fill=NA))
  #test(9999.59, frollmean(x, 51, align="left"), zoo::rollmean(x, 51, fill=NA, align="left"))
  x = rnorm(1e3+1) # x odd, n odd
  test(9999.60, frollmean(x, 51), zoo::rollmean(x, 51, fill=NA, align="right"))
  #test(9999.61, frollmean(x, 51, align="center"), zoo::rollmean(x, 51, fill=NA))
  #test(9999.62, frollmean(x, 51, align="left"), zoo::rollmean(x, 51, fill=NA, align="left"))

  #### na.rm / fill
  x = c(1L, NA, 3L, 4L, 5L)
  test(9999.99, frollmean(x, 2, fill=0), zoo::rollapply(x, 2, mean, fill=0, align="right", na.rm=FALSE))
  test(9999.99, frollmean(x, 2, fill=0, na.rm=TRUE), zoo::rollapply(x, 2, mean, fill=0, align="right", na.rm=TRUE))
  test(9999.99, frollmean(x, 2, fill=NA), zoo::rollapply(x, 2, mean, fill=NA, align="right"))
  test(9999.99, frollmean(x, 2, fill=NA, na.rm=TRUE), zoo::rollapply(x, 2, mean, fill=NA, align="right", na.rm=TRUE))
  
  #### na.rm FALSE
  d = as.data.table(list(1:6/2, 3:8/4))
  d[c(2L, 5L), V1:=NA][4:6, V2:=NA]
  ans = frollmean(d, 2:3)
  expected = list(
    zoo::rollapply(d[[1L]], 2L, mean, fill=NA, align="right"),
    zoo::rollapply(d[[1L]], 3L, mean, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 2L, mean, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 3L, mean, fill=NA, align="right")
  )
  test(9999.99, ans, expected)
  #### na.rm TRUE
  ans = frollmean(d, 2:3, na.rm=TRUE)
  expected = list(
    zoo::rollapply(d[[1L]], 2L, mean, na.rm=TRUE, fill=NA, align="right"),
    zoo::rollapply(d[[1L]], 3L, mean, na.rm=TRUE, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 2L, mean, na.rm=TRUE, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 3L, mean, na.rm=TRUE, fill=NA, align="right")
  )
  test(9999.99, ans, expected)

}

#### adaptive window against https://stackoverflow.com/a/21368246/2490497

if (dev_and_benchmark_area<-FALSE) {
  ## commented to not raise warning on cran check
  #pkgs = c("microbenchmark","TTR","caTools","RollingWindow","data.table")
  #if (all(sapply(pkgs, requireNamespace, quietly=TRUE))) {
  #  set.seed(100)
  #  nx = 1e2
  #  n = 1e1
  #  x = rnorm(nx)
  #  microbenchmark::microbenchmark(
  #    times=10, check=function(x) all(sapply(x[-1L], function(xx) isTRUE(all.equal(x[[1L]], xx)))),
  #    TTR = TTR::runMean(x, n),
  #    caTools = caTools::runmean(x, n, alg="fast", endrule="NA", align="right"),
  #    RollingWindow = RollingWindow::RollingMean(x, n)[,1L],
  #    data.table = data.table::frollmean(x, n)
  #    #, RcppRoll = RcppRoll::roll_mean(x, n, na.rm=FALSE, fill=NA, align="right")
  #    #, zoo = zoo::rollmean(x, n, fill=NA, align="right")
  #  )
  #}

  ## openmp
  #library(data.table)
  x=rnorm(1e8)
  setDTthreads(1)
  system.time(ans1<-frollmean(x, 1:10*100))
  setDTthreads(10)
  system.time(ans2<-frollmean(x, 1:10*100))
  all.equal(ans1, ans2)
  rm(ans1, ans2)
  invisible(gc())

  ## hasNA TRUE / FALSE
  #library(data.table)
  set.seed(108)
  nx = 1e9
  n = 1e4
  nas = 1e5
  x1 = rnorm(nx)
  x2 = rnorm(nx)
  x1n = copy(x1)
  x1n[sample(nx, nas)] = NA
  x2n = copy(x2)
  x2n[sample(nx, nas)] = NA
    
  system.time(frollmean(x1, n, hasNA=TRUE))
  system.time(frollmean(x2, n))
  system.time(frollmean(x1n, n, hasNA=TRUE))
  system.time(frollmean(x2n, n))

  ## adaptive TRUE FALSE
  #https://stackoverflow.com/questions/21368245/adaptive-moving-average-top-performance-in-r/21368246#21368246
  f = function(x, n, na.rm=FALSE) {
    nx = length(x)
    stopifnot(nx==length(n))
    ans = vector("double", nx)[NA]
    for (i in seq_along(x))
      if (i >= n[i]) ans[i] = mean(x[(i-n[i]+1):i], na.rm=na.rm)
    ans
  }
  wmapply = function(x, width, FUN = NULL, ...){
    FUN <- match.fun(FUN)
    SEQ1 <- 1:length(x)
    SEQ1[SEQ1 <  width] <- NA_integer_
    SEQ2 <- lapply(SEQ1, function(i) if(!is.na(i)) (i - (width[i]-1)):i)
    OUT <- lapply(SEQ2, function(i) if(!is.null(i)) FUN(x[i], ...) else NA_real_)
    return(base:::simplify2array(OUT, higher = TRUE))
  }
  #cc(F)
  #set.seed(108)
  #x = rnorm(1e2)#sample(c(1:10,4:8,2:4)*3/2, 200000, TRUE)
  #n = sample(3:10, length(x), TRUE)
  #ans1 <- f(x, n)
  #ans2 <- wmapply(x, n, mean)
  
  ## ## exact TRUE / FALSE
  ## set.seed(108)
  ## x = rnorm(1e6, 1000, 50)
  ## n = 1e5
  ## ans1 <- zoo::rollmean(x, n, fill=NA, align="right")
  ## ans2 <- zoo::rollapply(x, n, mean, fill=NA, align="right")
  ## system.time(ans3 <- frollmean(x, n, exact=FALSE))
  ## system.time(ans4 <- frollmean(x, n, exact=TRUE))
  ## format(sum(ans1-ans2, na.rm=TRUE), scientific=FALSE)
  ## format(sum(ans1-ans3, na.rm=TRUE), scientific=FALSE)
  ## format(sum(ans1-ans4, na.rm=TRUE), scientific=FALSE)
  ## format(sum(ans3-ans4, na.rm=TRUE), scientific=FALSE)
  
  ## check.roundoff = function(x, n) {
  ##   l = list(
  ##     zoo = zoo::rollmean(x, n, fill=NA, align="right"),
  ##     ttr = TTR::runMean(x, n),
  ##     catools_e = caTools::runmean(x, n, alg="exact", endrule="NA", align="right"),
  ##     catools = caTools::runmean(x, n, alg="fast", endrule="NA", align="right"),
  ##     rcpproll = RcppRoll::roll_mean(x, n, na.rm=FALSE, fill=NA, align="right"),
  ##     rollingw = RollingWindow::RollingMean(x, n)[,1L],
  ##     dt_e = frollmean(x, n, exact=TRUE),
  ##     dt = frollmean(x, n, exact=FALSE)
  ##   )
  ##   f = function(x, y) format(sum(abs(x - y), na.rm=TRUE), scientific=FALSE)
  ##   m = matrix(nrow=length(l), ncol=length(l), dimnames=list(names(l), names(l)))
  ##   for (i in 1:length(l))
  ##     for (j in 1:length(l))
  ##       m[i, j] = f(l[[1L]], l[[2L]])
  ##   res = cbind(
  ##     all.equal = sapply(l[-1L], function(x) isTRUE(all.equal(x, l[[1L]]))),
  ##     identical = sapply(l[-1L], function(x) identical(x, l[[1L]]))
  ##   )
  ##   rownames(res) = names(l[-1L])
  ##   list(res, m)
  ## }
  ## cc(F)
  ## n=k=5
  ## x = rep(100/3,30)
  ## d=1e10
  ## x[5] = d;     
  ## x[13] = d; 
  ## x[14] = d*d; 
  ## x[15] = d*d*d; 
  ## x[16] = d*d*d*d; 
  ## x[17] = d*d*d*d*d; 
  ## a = caTools::runmean(x, k, alg="fast")
  ## c = caTools::runmean(x, k, alg="exact")
  ## ans1 <- zoo::rollmean(x, n, fill=NA, align="right")
  ## ans2 <- zoo::rollapply(x, n, mean, fill=NA, align="right")
  ## system.time(ans3 <- frollmean(x, n, exact=FALSE))
  ## system.time(ans4 <- frollmean(x, n, exact=TRUE))
  ## format(sum(ans1-a, na.rm=TRUE), scientific=FALSE)
  ## format(sum(ans1-c, na.rm=TRUE), scientific=FALSE)
  ## format(sum(ans3-a, na.rm=TRUE), scientific=FALSE)
  ## format(sum(ans4-c, na.rm=TRUE), scientific=FALSE)
  ## ans3
  ## a
  ## c
  ## format(sum(ans1-ans4, na.rm=TRUE), scientific=FALSE)
  ## format(sum(c-ans4, na.rm=TRUE), scientific=FALSE)
  
  ## x = 1:100; n = 10;
  ## check.roundoff(x, n)
  
}

setDTthreads(oldDTthreads)
