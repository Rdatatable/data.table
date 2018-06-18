## this file will be migrated to inst/tests/tests.Rraw when branch will be ready to merge
if (!interactive()) {
  library(data.table)
  test = data.table:::test
  froll = data.table:::froll
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

#### in x integer converted to double
di = data.table(real=1:10/2, int=1:10)
ans = frollmean(di, 3)
expected = list(
  c(rep(NA_real_,2), seq(1,4.5,0.5)),
  c(rep(NA_real_,2), seq(2,9,1))
)
test(9999.99, ans, expected)

#### in n double converted to integer
x = 1:3/2
n = 2
test(9999.99, frollmean(x, n), c(NA, 0.75, 1.25))
n = list(c(2L,1L,2L), c(2,1,2))
# TODO: test(9999.99, frollmean(x, n), c(NA, 0.75, 1.25))

#### error on unsupported type
dx = data.table(real=1:10/2, char=letters[1:10])
test(9999.99, frollmean(dx, 3), error="x must be list, data.frame or data.table of numeric types")
dx = data.table(real=1:10/2, fact=factor(letters[1:10]))
test(9999.99, frollmean(dx, 3), error="x must be list, data.frame or data.table of numeric types")
dx = data.table(real=1:10/2, logi=logical(10))
test(9999.99, frollmean(dx, 3), error="x must be list, data.frame or data.table of numeric types")
dx = data.table(real=1:10/2, list=rep(list(NA), 10))
test(9999.99, frollmean(dx, 3), error="x must be list, data.frame or data.table of numeric types")
x = letters[1:10]
test(9999.99, frollmean(x, 3), error="x must be of type numeric")
x = 1:10/2
test(9999.99, frollmean(x, "a"), error="n must be integer")
test(9999.99, frollmean(x, factor("a")), error="n must be integer")
test(9999.99, frollmean(x, TRUE), error="n must be integer")
# TODO test(9999.99, frollmean(x, list(NA)), error="n must be integer")

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
ans = frollmean(d, 3, align="center") # x even, n odd
expected = list(
  c(NA_real_, seq(1,2.5,0.5), NA_real_),
  c(NA_real_, seq(1,1.75,0.25), NA_real_)
)
test(9999.7, ans, expected)
ans = frollmean(d, 4, align="center") # x even, n even
expected = list(
  c(NA_real_, seq(1.25,2.25,0.5), rep(NA_real_,2)),
  c(NA_real_, seq(1.125,1.625,0.25), rep(NA_real_,2))
)
test(9999.8, ans, expected)
de = rbind(d, data.table(3.5, 2.25))
ans = frollmean(de, 3, align="center") # x odd, n odd
expected = list(
  c(NA_real_, seq(1,3,0.5), NA_real_),
  c(NA_real_, seq(1,2,0.25), NA_real_)
)
test(9999.9, ans, expected)
ans = frollmean(de, 4, align="center") # x odd, n even
expected = list(
  c(NA_real_, seq(1.25,2.75,0.5), rep(NA_real_,2)),
  c(NA_real_, seq(1.125,1.875,0.25), rep(NA_real_,2))
)
test(9999.10, ans, expected)
ans = frollmean(d, 3, align="left")
expected = list(
  c(seq(1,2.5,0.5), rep(NA_real_,2)),
  c(seq(1,1.75,0.25), rep(NA_real_,2))
)
test(9999.11, ans, expected)

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
x = rnorm(1e3)
n = rep(20L, 1e3) # pseudo adaptive
test(9999.99, frollmean(x, n[1L]), frollmean(x, n, adaptive = TRUE)) # n auto wrapped in list
test(9999.99, frollmean(x, n[1L]), frollmean(x, list(n))) # n list so adaptive to TRUE
ama = function(x, n, na.rm=FALSE, fill=NA) {
  # adaptive moving average in R
  stopifnot((nx<-length(x))==length(n))
  ans = rep(NA_real_, nx)
  for (i in seq_along(x)) {
    if (i >= n[i]) ans[i] = mean(x[(i-n[i]+1):i], na.rm=na.rm)
    else ans[i] = as.double(fill)
  }
  ans
}
fastama = function(x, n, na.rm, fill=NA) {
  if (!missing(na.rm)) stop("fast adaptive moving average implemented in R does not handle NAs, input having NAs will result in incorrect answer so not even try to compare to it")
  # fast implementation of adaptive moving average in R, in case of NAs incorrect answer
  stopifnot((nx<-length(x))==length(n))
  cs = cumsum(x)
  ans = rep(NA_real_, nx)
  for (i in seq_along(cs)) {
    if (i == n[i]) ans[i] = cs[i]/n[i]
    else if (i > n[i]) ans[i] = (cs[i]-cs[i-n[i]])/n[i]
    else ans[i] = as.double(fill)
  }
  ans
}

x = c(1:4,2:5,4:6,5L)
n = c(2L, 2L, 2L, 5L, 4L, 5L, 1L, 1L, 2L, 3L, 6L, 3L)
ans1 = ama(x, n)
ans2 = frollmean(x, list(n))
test(9999.99, ans1, ans2)

x = data.table(x=x, y=x/2) # multiple columns and multiple windows
ln = list(n, n+1L)
ans1 = list(ama(x[[1L]], ln[[1L]]), ama(x[[1L]], ln[[2L]]), ama(x[[2L]], ln[[1L]]), ama(x[[2L]], ln[[2L]]))
ans2 = frollmean(x, ln, adaptive=TRUE)
test(9999.99, ans1, ans2)

#### adaptive fill
x = c(1:4,2:5,4:6,5L)
n = c(2L, 2L, 2L, 5L, 4L, 5L, 1L, 1L, 2L, 3L, 6L, 3L)
ans1 = ama(x, n, fill=150)
ans2 = frollmean(x, list(n), exact=FALSE, fill=150)
ans3 = frollmean(x, list(n), exact=TRUE, fill=150)
test(9999.99, ans1, ans2)
test(9999.99, ans1, ans3)

#### adaptive na.rm
x = c(1:4,NA,2:5,NA,4:6,NA,5L)
n = c(2L, 2L, 2L, 5L, 3L, 4L, 5L, 1L, 2L, 1L, 2L, 4L, 3L, 6L, 3L)
ans1 = ama(x, n)
ans2 = frollmean(x, list(n), exact=FALSE)
ans3 = frollmean(x, list(n), exact=TRUE)
test(9999.99, ans1, ans2)
test(9999.99, ans1, ans3)
ans1 = ama(x, n, na.rm=TRUE)
ans2 = frollmean(x, list(n), exact=FALSE, na.rm=TRUE)
ans3 = frollmean(x, list(n), exact=TRUE, na.rm=TRUE)
test(9999.99, ans1, ans2)
test(9999.99, ans1, ans3)
#### TODO test 5e9 vector where 3e9 are NAs to confirm uint_fast64_t running NA counter
if (FALSE) {
  #x = sample(c(rnorm(2e9), rep(NA_real_, 3e9)))
  #n = sample(c(1e3, 1e4, 1e5), length(x))
  #ans = frollmean(x, list(n), adaptive=TRUE)
}

#### adaptive limitations
test(9999.99, frollmean(1:2, list(1:2), partial=FALSE), c(1, 1.5))
test(9999.99, frollmean(1:2, list(1:2), partial=TRUE), error="using adaptive TRUE and partial TRUE is not implemented")
test(9999.99, frollmean(1:2, list(1:2), align="right"), c(1, 1.5))
test(9999.99, frollmean(1:2, list(1:2), align="center"), error="using adaptive TRUE and align argument different than 'right' is not implemented")
test(9999.99, frollmean(1:2, list(1:2), align="left"), error="using adaptive TRUE and align argument different than 'right' is not implemented")
test(9999.99, frollmean(list(1:2, 1:3), list(1:2)), error="Adaptive rolling function can only process 'x' having equal length of elements, like data.table or data.frame. If you want to call rolling function on list having variable length of elements call it for each field separately.")

#### adaptive exact
x = c(1:3, 1e9L, 2:5, 5e9, 4:6)
n = c(2L, 2L, 2L, 5L, 4L, 5L, 1L, 1L, 2L, 3L, 6L, 3L)
ans1 = ama(x, n)
ans2 = frollmean(x, list(n), exact=FALSE)
ans3 = frollmean(x, list(n), exact=TRUE)
ans4 = fastama(x, n)
cbind(ans1, ans2, ans3, ans4)
format(ans1-ans2, scientific=F)
format(ans1-ans3, scientific=F)
format(ans1-ans4, scientific=F)

x = sample(c(rnorm(1e3, 1e2), rnorm(1e1, 1e9)))
n = sample(1:20, length(x), TRUE)
ans1 = ama(x, n)
ans2 = frollmean(x, list(n), exact=FALSE)
ans3 = frollmean(x, list(n), exact=TRUE)
ans4 = fastama(x, n)
anserr = list(
  froll_exact_f = ans1-ans2,
  froll_exact_t = ans1-ans3,
  fastama = ans1-ans4
)
format(sapply(lapply(anserr, abs), sum, na.rm=TRUE), scientific=FALSE)
# as of now exact=TRUE has bigger roundoff TODO

x = sample(c(rnorm(1e6, 1e2), rnorm(1e1, 1e9)))
n = sample(100:2000, length(x), TRUE)
system.time(ans1 <- ama(x, n))
system.time(ans2 <- frollmean(x, list(n), exact=FALSE))
system.time(ans3 <- frollmean(x, list(n), exact=TRUE))
system.time(ans4 <- fastama(x, n))
anserr = list(
  froll_exact_f = ans1-ans2,
  froll_exact_t = ans1-ans3,
  fastama = ans1-ans4
)
format(sapply(lapply(anserr, abs), sum, na.rm=TRUE), scientific=FALSE)

x = sample(rnorm(1e5, 1e7, 5e6))
n = sample(10:100, length(x), TRUE)
ans1 = ama(x, n)
ans2 = frollmean(x, list(n), exact=FALSE)
ans3 = frollmean(x, list(n), exact=TRUE)
ans4 = fastama(x, n)
anserr = list(
  froll_exact_f = ans1-ans2,
  froll_exact_t = ans1-ans3,
  fastama = ans1-ans4
)
format(sapply(lapply(anserr, abs), sum, na.rm=TRUE), scientific=FALSE)


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
test(9999.99, frollmean(5, 1, align="left"), 5)
test(9999.99, frollmean(list(1, 10, 5), 1, align="left"), list(1, 10, 5))
test(9999.99, frollmean(5, 1, align="center"), 5)
test(9999.99, frollmean(list(1, 10, 5), 1, align="center"), list(1, 10, 5))
test(9999.99, frollsum(5, 1), 5)
test(9999.99, frollsum(list(1, 10, 5), 1), list(1, 10, 5))
test(9999.99, frollsum(5, 1, align="left"), 5)
test(9999.99, frollsum(list(1, 10, 5), 1, align="left"), list(1, 10, 5))
test(9999.99, frollsum(5, 1, align="center"), 5)
test(9999.99, frollsum(list(1, 10, 5), 1, align="center"), list(1, 10, 5))

#### length(x)==1 && n==2
test(9999.99, frollmean(5, 2), NA_real_)
test(9999.99, frollmean(list(1, 10, 5), 2), list(NA_real_, NA_real_, NA_real_))
test(9999.99, frollmean(5, 2, align="left"), NA_real_)
test(9999.99, frollmean(list(1, 10, 5), 2, align="left"), list(NA_real_, NA_real_, NA_real_))
test(9999.99, frollmean(5, 2, align="center"), NA_real_)
test(9999.99, frollmean(list(1, 10, 5), 2, align="center"), list(NA_real_, NA_real_, NA_real_))
test(9999.99, frollsum(5, 2), NA_real_)
test(9999.99, frollsum(list(1, 10, 5), 2), list(NA_real_, NA_real_, NA_real_))
test(9999.99, frollsum(5, 2, align="left"), NA_real_)
test(9999.99, frollsum(list(1, 10, 5), 2, align="left"), list(NA_real_, NA_real_, NA_real_))
test(9999.99, frollsum(5, 2, align="center"), NA_real_)
test(9999.99, frollsum(list(1, 10, 5), 2, align="center"), list(NA_real_, NA_real_, NA_real_))

#### n==Inf
test(9999.99, frollmean(1:5, Inf), error="n must be positive integer values", warning="NAs introduced by coercion to integer range")

#### n==c(5, Inf)
test(9999.99, frollmean(1:5, c(5, Inf)), error="n must be positive integer values", warning="NAs introduced by coercion to integer range")

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
  set.seed(108)

  zoo_compare = function(x, n) {
    num.step = 0.001
    #### fun, align, na.rm, partial
    drollapply = function(...) as.double(zoo::rollapply(...)) # rollapply is not consistent in data type of answer, force to double
    for (fun in c("mean","sum")) {
      for (align in c("right","center","left")) {
        for (na.rm in c(FALSE, TRUE)) {
          for (partial in c(FALSE)) { # TODO partial TRUE
            num <<- num + num.step
            test(num,
                 froll(fun, x, n, align=align, na.rm=na.rm, partial=partial),
                 drollapply(x, n, FUN=fun, fill=NA, align=align, na.rm=na.rm, partial=partial))
          }
        }
      }
    }
  }
  num = 9999.0
  ## no NA
  x = rnorm(1e3); n = 50 # x even, n even
  zoo_compare(x, n)
  x = rnorm(1e3+1); n = 50 # x odd, n even
  zoo_compare(x, n)
  x = rnorm(1e3); n = 51 # x even, n odd
  zoo_compare(x, n)
  x = rnorm(1e3+1); n = 51 # x odd, n odd
  zoo_compare(x, n)
  ## leading and trailing NAs
  x = c(rep(NA, 60), rnorm(1e3), rep(NA, 60)); n = 50
  zoo_compare(x, n)
  x = c(rep(NA, 60), rnorm(1e3+1), rep(NA, 60)); n = 50
  zoo_compare(x, n)
  x = c(rep(NA, 60), rnorm(1e3), rep(NA, 60)); n = 51
  zoo_compare(x, n)
  x = c(rep(NA, 60), rnorm(1e3+1), rep(NA, 60)); n = 51
  zoo_compare(x, n)
  ## random NA
  makeNA = function(x, ratio=0.1) {id=sample(length(x), as.integer(length(x) * ratio)); x[id]=NA; x}
  x = makeNA(rnorm(1e3)); n = 50
  zoo_compare(x, n)
  x = makeNA(rnorm(1e3+1)); n = 50
  zoo_compare(x, n)
  x = makeNA(rnorm(1e3)); n = 51
  zoo_compare(x, n)
  x = makeNA(rnorm(1e3+1)); n = 51
  zoo_compare(x, n)
  rm(num)
  
  ## test vs partial / fill combinations in zoo
  #drollapply = function(...) as.double(zoo::rollapply(...))
  #drollapply(c(NA, 1:5, NA), 2, "sum", align="right", fill=0, partial=F, na.rm=F)
  #drollapply(c(NA, 1:5, NA), 2, "sum", align="right", fill=0, partial=T, na.rm=F)
  #drollapply(c(NA, 1:5, NA), 2, "sum", align="right", fill=0, partial=F, na.rm=T)
  #drollapply(c(NA, 1:5, NA), 2, "sum", align="right", fill=0, partial=T, na.rm=T)
  
  #### na.rm / fill
  x = as.double(c(1L, NA, 3L, 4L, 5L))
  test(9999.99, frollmean(x, 2, fill=0), zoo::rollapply(x, 2, mean, fill=0, align="right", na.rm=FALSE))
  test(9999.99, frollsum(x, 2, fill=0), zoo::rollapply(x, 2, sum, fill=0, align="right", na.rm=FALSE))
  test(9999.99, frollmean(x, 2, fill=0, na.rm=TRUE), zoo::rollapply(x, 2, mean, fill=0, align="right", na.rm=TRUE))
  test(9999.99, frollsum(x, 2, fill=0, na.rm=TRUE), zoo::rollapply(x, 2, sum, fill=0, align="right", na.rm=TRUE))
  test(9999.99, frollmean(x, 2, fill=NA), zoo::rollapply(x, 2, mean, fill=NA, align="right"))
  test(9999.99, frollsum(x, 2, fill=NA), zoo::rollapply(x, 2, sum, fill=NA, align="right"))
  test(9999.99, frollmean(x, 2, fill=NA, na.rm=TRUE), zoo::rollapply(x, 2, mean, fill=NA, align="right", na.rm=TRUE))
  test(9999.99, frollsum(x, 2, fill=NA, na.rm=TRUE), zoo::rollapply(x, 2, sum, fill=NA, align="right", na.rm=TRUE))
  
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

  ans = frollsum(d, 2:3)
  expected = list(
    zoo::rollapply(d[[1L]], 2L, sum, fill=NA, align="right"),
    zoo::rollapply(d[[1L]], 3L, sum, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 2L, sum, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 3L, sum, fill=NA, align="right")
  )
  test(9999.99, ans, expected)

  ans = frollmean(d, 2:3, align="center")
  expected = list(
    zoo::rollapply(d[[1L]], 2L, mean, fill=NA),
    zoo::rollapply(d[[1L]], 3L, mean, fill=NA),
    zoo::rollapply(d[[2L]], 2L, mean, fill=NA),
    zoo::rollapply(d[[2L]], 3L, mean, fill=NA)
  )
  test(9999.99, ans, expected)
  
  ans = frollsum(d, 2:3, align="center")
  expected = list(
    zoo::rollapply(d[[1L]], 2L, sum, fill=NA),
    zoo::rollapply(d[[1L]], 3L, sum, fill=NA),
    zoo::rollapply(d[[2L]], 2L, sum, fill=NA),
    zoo::rollapply(d[[2L]], 3L, sum, fill=NA)
  )
  test(9999.99, ans, expected)
  ans = frollmean(d, 2:3, align="left")
  expected = list(
    zoo::rollapply(d[[1L]], 2L, mean, fill=NA, align="left"),
    zoo::rollapply(d[[1L]], 3L, mean, fill=NA, align="left"),
    zoo::rollapply(d[[2L]], 2L, mean, fill=NA, align="left"),
    zoo::rollapply(d[[2L]], 3L, mean, fill=NA, align="left")
  )
  test(9999.99, ans, expected)
  ans = frollsum(d, 2:3, align="left")
  expected = list(
    zoo::rollapply(d[[1L]], 2L, sum, fill=NA, align="left"),
    zoo::rollapply(d[[1L]], 3L, sum, fill=NA, align="left"),
    zoo::rollapply(d[[2L]], 2L, sum, fill=NA, align="left"),
    zoo::rollapply(d[[2L]], 3L, sum, fill=NA, align="left")
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
  ans = frollsum(d, 2:3, na.rm=TRUE)
  expected = list(
    zoo::rollapply(d[[1L]], 2L, sum, na.rm=TRUE, fill=NA, align="right"),
    zoo::rollapply(d[[1L]], 3L, sum, na.rm=TRUE, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 2L, sum, na.rm=TRUE, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 3L, sum, na.rm=TRUE, fill=NA, align="right")
  )
  test(9999.99, ans, expected)

}

#### adaptive window against https://stackoverflow.com/a/21368246/2490497

if (dev_and_benchmark_area<-FALSE) {

  if (.Platform$OS.type=="unix") {
    memgb = as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))/(1024^2)
    if (memgb > 20) {
      x = rnorm(2.5e9, 100, 20)
      ans = sapply(10^(1:6), function(n) {message("doing ", n); system.time(frollmean(x=x, n=n))})
    }
  }
  
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
  
  ## # exact TRUE / FALSE
  ## rsummean = function(x, n) {ans=rep(NA_real_, nx<-length(x)); for(i in n:nx) ans[i]=sum(x[(i-n+1):i])/n; ans}
  ## rmean = function(x, n) {ans=rep(NA_real_, nx<-length(x)); for(i in n:nx) ans[i]=mean(x[(i-n+1):i]); ans}
  ## dtmean = function(x, n) sapply(n:length(x), function(i) as.data.table(list(x[(i-n+1):i]))[, mean(V1)]) # no api to data.table fastmean, so `[` required
  ## nx = 200
  ## x = rnorm(nx, sd=30) + abs(seq(nx)-nx/4)
  ## n = 196
  ## l = list(
  ##   caTools_R = caTools::runmean(x, n, alg="R", endrule="NA", align="right"),
  ##   caTools_fast = caTools::runmean(x, n, alg="fast", endrule="NA", align="right"),
  ##   caTools_C = caTools::runmean(x, n, alg="C", endrule="NA", align="right"),
  ##   caTools_exact = caTools::runmean(x, n, alg="exact", endrule="NA", align="right"),
  ##   datatable = frollmean(x, n, exact=FALSE),
  ##   datatable_exact = frollmean(x, n, exact=TRUE),
  ##   datatable_fmean = dtmean(x, n),
  ##   zoo_R_mean = zoo::rollapply(x, n, (mean), fill=NA, align="right"),
  ##   R_sum_mean = rsummean(x, n),
  ##   R_mean = rmean(x, n)
  ## )
  ## ll = lapply(l, tail, 5)
  ## validate = which(names(ll)=="R_mean")
  ## sapply(ll[-validate], function(x) format(sum(abs(x-ll[[validate]])), scientific=FALSE))
  
}

setDTthreads(oldDTthreads)
