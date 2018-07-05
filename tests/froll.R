## this file will be migrated to inst/tests/tests.Rraw when branch will be ready to merge
cc.used = all(sapply(c("test","froll"), exists))
if (!interactive() && !cc.used) {
  library(data.table)
  test = data.table:::test
  froll = data.table:::froll
}
oldDTthreads = setDTthreads(1)

## rolling features

#### atomic vectors input and single window returns atomic vectors
x = 1:6/2
ans1 = frollmean(x, 3)
ans2 = frollmean(x, 3, exact=TRUE)
expected = c(rep(NA_real_,2), seq(1,2.5,0.5))
test(9999.4, ans1, expected)
test(9999.4, ans2, expected)

#### multiple columns at once
d = as.data.table(list(1:6/2, 3:8/4))
ans1 = frollmean(d, 3)
ans2 = frollmean(d, 3, exact=TRUE)
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)),
  c(rep(NA_real_,2), seq(1,1.75,0.25))
)
test(9999.1, ans1, expected)
test(9999.1, ans2, expected)

#### multiple windows at once
ans1 = frollmean(d[, .(V1)], c(3, 4))
ans2 = frollmean(d[, .(V1)], c(3, 4), exact=TRUE)
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)),
  c(rep(NA_real_,3), seq(1.25,2.25,0.5))
)
test(9999.1, ans1, expected)
test(9999.1, ans2, expected)

#### multiple columns and multiple windows at once
ans1 = frollmean(d, c(3, 4))
ans2 = frollmean(d, c(3, 4), exact=TRUE)
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)), c(rep(NA_real_,3), seq(1.25,2.25,0.5)),
  c(rep(NA_real_,2), seq(1,1.75,0.25)), c(rep(NA_real_,3), seq(1.125,1.625,0.25))
)
test(9999.1, ans1, expected)
test(9999.1, ans2, expected)

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
test(9999.99, frollmean(x, n, adaptive=TRUE), list(c(NA, 1, 1.25), c(NA, 1, 1.25)))

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
test(9999.99, frollmean(x, list(1:10)), error="n must be integer, list is accepted for adaptive TRUE")
test(9999.99, frollmean(x, list(NA), adaptive=TRUE), error="n must be integer vector or list of integer vectors")
test(9999.99, frollmean(x, list(c(1:5,1:5), NA), adaptive=TRUE), error="n must be integer vector or list of integer vectors")

#### various length list vectors
l = list(1:6/2, 3:10/4)
ans = frollmean(l, c(3, 4))
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)), c(rep(NA_real_,3), seq(1.25,2.25,0.5)),
  c(rep(NA_real_,2), seq(1,2.25,0.25)), c(rep(NA_real_,3), seq(1.125,2.125,0.25))
)
test(9999.5, ans, expected)

#### exact
set.seed(108)
x = sample(c(rnorm(1e3, 1e6, 5e5), 5e9, 5e-9))
n = 15
ma = function(x, n, na.rm=FALSE) {
  ans = rep(NA_real_, nx<-length(x))
  for (i in n:nx) ans[i] = mean(x[(i-n+1):i], na.rm=na.rm)
  ans
}
fastma = function(x, n, na.rm) {
  if (!missing(na.rm)) stop("NAs are unsupported, wrongly propagated by cumsum")
  cs = cumsum(x)
  scs = shift(cs, n)
  scs[n] = 0
  as.double((cs-scs)/n)
}
ans1 = ma(x, n)
ans2 = fastma(x, n)
ans3 = frollmean(x, n, exact=TRUE)
ans4 = frollmean(x, n)
anserr = list(
  froll_exact_f = ans4-ans1,
  froll_exact_t = ans3-ans1,
  fastma = ans2-ans1
)
errs = sapply(lapply(anserr, abs), sum, na.rm=TRUE)
test(9999.99, errs[["froll_exact_t"]]==0)
test(9999.99, errs[["froll_exact_f"]]>errs[["froll_exact_t"]])
test(9999.99, errs[["fastma"]]>errs[["froll_exact_t"]])
test(9999.99, errs[["fastma"]]>errs[["froll_exact_f"]])

#### align: right/center/left
d = as.data.table(list(1:8/2, 3:10/4))
ans1 = frollmean(d, 5, align="right") # default
ans2 = frollmean(d, 5, align="right", exact=TRUE)
expected = list(
  c(rep(NA_real_,4), seq(1.5,3,0.5)),
  c(rep(NA_real_,4), seq(1.25,2,0.25))
)
test(9999.6, ans1, expected)
test(9999.6, ans2, expected)
ans1 = frollmean(d, 5, align="center") # x even, n odd
ans2 = frollmean(d, 5, align="center", exact=TRUE)
expected = list(
  c(rep(NA_real_, 2), seq(1.5,3,0.5), rep(NA_real_, 2)),
  c(rep(NA_real_, 2), seq(1.25,2,0.25), rep(NA_real_, 2))
)
test(9999.6, ans1, expected)
test(9999.6, ans2, expected)
ans1 = frollmean(d, 6, align="center") # x even, n even
ans2 = frollmean(d, 6, align="center", exact=TRUE)
expected = list(
  c(rep(NA_real_, 2), seq(1.75,2.75,0.5), rep(NA_real_,3)),
  c(rep(NA_real_, 2), seq(1.375,1.875,0.25), rep(NA_real_,3))
)
test(9999.8, ans1, expected)
test(9999.8, ans2, expected)
de = rbind(d, data.table(4.5, 2.75))
ans1 = frollmean(de, 5, align="center") # x odd, n odd
ans2 = frollmean(de, 5, align="center", exact=TRUE)
expected = list(
  c(rep(NA_real_, 2), seq(1.5,3.5,0.5), rep(NA_real_, 2)),
  c(rep(NA_real_, 2), seq(1.25,2.25,0.25), rep(NA_real_, 2))
)
test(9999.9, ans1, expected)
test(9999.9, ans2, expected)
ans1 = frollmean(de, 6, align="center") # x odd, n even
ans2 = frollmean(de, 6, align="center", exact=TRUE)
expected = list(
  c(rep(NA_real_, 2), seq(1.75,3.25,0.5), rep(NA_real_,3)),
  c(rep(NA_real_, 2), seq(1.375,2.125,0.25), rep(NA_real_,3))
)
test(9999.10, ans1, expected)
test(9999.10, ans2, expected)
ans1 = frollmean(d, 5, align="left")
ans2 = frollmean(d, 5, align="left", exact=TRUE)
expected = list(
  c(seq(1.5,3,0.5), rep(NA_real_,4)),
  c(seq(1.25,2,0.25), rep(NA_real_,4))
)
test(9999.11, ans1, expected)
test(9999.11, ans2, expected)

#### handling NAs align na.rm
d = as.data.table(list(1:8/2, 3:10/4))
d[c(2L, 7L), "V1" := NA][c(1:2,8L), "V2" := NA]
ans1 = frollmean(d, 3, align="right") # default
ans2 = frollmean(d, 3, align="right", exact=TRUE)
expected = list(
  c(rep(NA_real_,4), seq(2,2.5,0.5), rep(NA_real_, 2)),
  c(rep(NA_real_,4), seq(1.5,2,0.25), rep(NA_real_, 1))
)
test(9999.6, ans1, expected)
test(9999.6, ans2, expected)
ans1 = frollmean(d, 3, align="right", na.rm=TRUE)
ans2 = frollmean(d, 3, align="right", exact=TRUE, na.rm=TRUE)
expected = list(
  c(rep(NA_real_,2), 1, 1.75, 2, 2.5, 2.75, 3.5),
  c(rep(NA_real_,2), 1.25, 1.375, 1.5, 1.75, 2, 2.125)
)
test(9999.6, ans1, expected)
test(9999.6, ans2, expected)
ans1 = frollmean(d, 3, align="center") # x even, n odd
ans2 = frollmean(d, 3, align="center", exact=TRUE)
expected = list(
  c(rep(NA_real_,3), seq(2,2.5,0.5), rep(NA_real_, 3)),
  c(rep(NA_real_,3), seq(1.5,2,0.25), rep(NA_real_, 2))
)
test(9999.6, ans1, expected)
test(9999.6, ans2, expected)
ans1 = frollmean(d, 3, align="center", na.rm=TRUE) # x even, n odd
ans2 = frollmean(d, 3, align="center", exact=TRUE, na.rm=TRUE)
expected = list(
  c(rep(NA_real_,1), 1, 1.75, 2, 2.5, 2.75, 3.5, rep(NA_real_,1)),
  c(rep(NA_real_,1), 1.25, 1.375, 1.5, 1.75, 2, 2.125, rep(NA_real_,1))
)
test(9999.6, ans1, expected)
test(9999.6, ans2, expected)
ans1 = frollmean(d, 4, align="center") # x even, n even
ans2 = frollmean(d, 4, align="center", exact=TRUE)
expected = list(
  c(rep(NA_real_,3), 2.25, rep(NA_real_, 4)),
  c(rep(NA_real_,3), 1.625, 1.875, rep(NA_real_, 3))
)
test(9999.8, ans1, expected)
test(9999.8, ans2, expected)
ans1 = frollmean(d, 4, align="center", na.rm=TRUE) # x even, n even
ans2 = frollmean(d, 4, align="center", exact=TRUE, na.rm=TRUE)
expected = list(
  c(rep(NA_real_,1), 4/3, 2, 2.25, 2.5, 9.5/3, rep(NA_real_,2)),
  c(rep(NA_real_,1), 1.375, 1.5, 1.625, 1.875, 2, rep(NA_real_,2))
)
test(9999.8, ans1, expected)
test(9999.8, ans2, expected)
de = rbind(d, data.table(4.5, 2.75))
ans1 = frollmean(de, 3, align="center") # x odd, n odd
ans2 = frollmean(de, 3, align="center", exact=TRUE)
expected = list(
  c(rep(NA_real_,3), 2, 2.5, rep(NA_real_, 4)),
  c(rep(NA_real_,3), 1.5, 1.75, 2, rep(NA_real_, 3))
)
test(9999.9, ans1, expected)
test(9999.9, ans2, expected)
ans1 = frollmean(de, 3, align="center", na.rm=TRUE) # x odd, n odd
ans2 = frollmean(de, 3, align="center", exact=TRUE, na.rm=TRUE)
expected = list(
  c(rep(NA_real_,1), 1, 1.75, 2, 2.5, 2.75, 3.5, 4.25, rep(NA_real_,1)),
  c(rep(NA_real_,1), 1.25, 1.375, 1.5, 1.75, 2, 2.125, 2.5, rep(NA_real_,1))
)
test(9999.9, ans1, expected)
test(9999.9, ans2, expected)
ans1 = frollmean(de, 4, align="center") # x odd, n even
ans2 = frollmean(de, 4, align="center", exact=TRUE)
expected = list(
  c(rep(NA_real_, 3), 2.25, rep(NA_real_,5)),
  c(rep(NA_real_, 3), 1.625, 1.875, rep(NA_real_,4))
)
test(9999.10, ans1, expected)
test(9999.10, ans2, expected)
ans1 = frollmean(de, 4, align="center", na.rm=TRUE) # x odd, n even
ans2 = frollmean(de, 4, align="center", exact=TRUE, na.rm=TRUE)
expected = list(
  c(rep(NA_real_, 1), 4/3, 2, 2.25, 2.5, 9.5/3, 11.5/3, rep(NA_real_,2)),
  c(rep(NA_real_, 1), 1.375, 1.5, 1.625, 1.875, 2, 7/3, rep(NA_real_,2))
)
test(9999.10, ans1, expected)
test(9999.10, ans2, expected)
ans1 = frollmean(d, 3, align="left")
ans2 = frollmean(d, 3, align="left", exact=TRUE)
expected = list(
  c(rep(NA_real_, 2), 2, 2.5, rep(NA_real_,4)),
  c(rep(NA_real_, 2), 1.5, 1.75, 2, rep(NA_real_,3))
)
test(9999.11, ans1, expected)
test(9999.11, ans2, expected)
ans1 = frollmean(d, 3, align="left", na.rm=TRUE)
ans2 = frollmean(d, 3, align="left", exact=TRUE, na.rm=TRUE)
expected = list(
  c(1, 1.75, 2, 2.5, 2.75, 3.5, rep(NA_real_,2)),
  c(1.25, 1.375, 1.5, 1.75, 2, 2.125, rep(NA_real_,2))
)
test(9999.11, ans1, expected)
test(9999.11, ans2, expected)
#### handling NAs for NaN output also
d = as.data.table(list(1:6/2, 3:8/4))
d[c(2L, 5L), V1:=NA][4:6, V2:=NA]
ans1 = frollmean(d, 2:3)
ans2 = frollmean(d, 2:3, exact=TRUE)
expected = list(c(NA, NA, NA, 1.75, NA, NA), rep(NA_real_, 6), c(NA, 0.875, 1.125, NA, NA, NA), c(NA, NA, 1, NA, NA, NA))
test(9999.99, ans1, expected)
test(9999.99, ans2, expected)
ans1 = frollmean(d, 2:3, na.rm=TRUE)
ans2 = frollmean(d, 2:3, exact=TRUE, na.rm=TRUE)
expected = list(c(NA, 0.5, 1.5, 1.75, 2, 3), c(NA, NA, 1, 1.75, 1.75, 2.5), c(NA, 0.875, 1.125, 1.25, NaN, NaN), c(NA, NA, 1, 1.125, 1.25, NaN))
test(9999.99, ans1, expected)
test(9999.99, ans2, expected)

#### fill constant
test(9999.99, frollmean(1:5, 4, fill=0), c(0, 0, 0, 2.5, 3.5))
test(9999.99, frollmean(1:5, 4, fill=-5), c(-5, -5, -5, 2.5, 3.5))
test(9999.99, frollmean(1:5, 4, fill=100), c(100, 100, 100, 2.5, 3.5))
test(9999.99, frollmean(1:5, 4, fill=Inf), c(Inf, Inf, Inf, 2.5, 3.5))
test(9999.99, frollmean(1:5, 4, fill=NaN), c(NaN, NaN, NaN, 2.5, 3.5))

#### fill coercion
test(9999.99, frollmean(1:3, 2, fill=0), c(0, 1.5, 2.5))
test(9999.99, frollmean(1:3, 2, fill=0L), c(0, 1.5, 2.5))
test(9999.99, frollmean(1:3, 2, fill=NA_integer_), c(NA_real_, 1.5, 2.5))
test(9999.99, frollmean(1:3, 2, fill=1:2), error="fill must be a vector of length 1")
test(9999.99, frollmean(1:3, 2, fill=NA), c(NA_real_, 1.5, 2.5))
test(9999.99, frollmean(1:3, 2, fill=TRUE), error="fill must be numeric")
test(9999.99, frollmean(1:3, 2, fill=FALSE), error="fill must be numeric")
test(9999.99, frollmean(1:3, 2, fill="a"), error="fill must be numeric")
test(9999.99, frollmean(1:3, 2, fill=factor("a")), error="fill must be numeric")
test(9999.99, frollmean(1:3, 2, fill=list(NA)), error="fill must be numeric")

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
#### n==1
test(9999.99, frollmean(1:4, 1), as.double(1:4))
test(9999.99, frollmean(1:4, 1, exact=TRUE), as.double(1:4))
test(9999.99, frollmean(1:4, 1, align="center"), as.double(1:4))
test(9999.99, frollmean(1:4, 1, align="center", exact=TRUE), as.double(1:4))
test(9999.99, frollmean(1:4, 1, align="left"), as.double(1:4))
test(9999.99, frollmean(1:4, 1, align="left", exact=TRUE), as.double(1:4))
#### length(x)==1 && n==1
test(9999.99, frollmean(5, 1), 5)
test(9999.99, frollmean(list(1, 10, 5), 1), list(1, 10, 5))
test(9999.99, frollmean(5, 1, align="left"), 5)
test(9999.99, frollmean(list(1, 10, 5), 1, align="left"), list(1, 10, 5))
test(9999.99, frollmean(5, 1, align="center"), 5)
test(9999.99, frollmean(list(1, 10, 5), 1, align="center"), list(1, 10, 5))
#### length(x)==1 && n==2
test(9999.99, frollmean(5, 2), NA_real_)
test(9999.99, frollmean(list(1, 10, 5), 2), list(NA_real_, NA_real_, NA_real_))
test(9999.99, frollmean(5, 2, align="left"), NA_real_)
test(9999.99, frollmean(list(1, 10, 5), 2, align="left"), list(NA_real_, NA_real_, NA_real_))
test(9999.99, frollmean(5, 2, align="center"), NA_real_)
test(9999.99, frollmean(list(1, 10, 5), 2, align="center"), list(NA_real_, NA_real_, NA_real_))
#### n==Inf
test(9999.99, frollmean(1:5, Inf), error="n must be positive integer values", warning="NAs introduced by coercion to integer range")
#### n==c(5, Inf)
test(9999.99, frollmean(1:5, c(5, Inf)), error="n must be positive integer values", warning="NAs introduced by coercion to integer range")
#### is.complex(n)
test(9999.99, frollmean(1:5, 3i), error="n must be integer")
#### is.character(n)
test(9999.99, frollmean(1:5, "a"), error="n must be integer")
#### is.factor(n)
test(9999.99, frollmean(1:5, as.factor("a")), error="n must be integer")
#### is.list(n)
test(9999.99, frollmean(1:5, list(1:5)), error="n must be integer, list is accepted for adaptive TRUE")

#### non-finite values (NA, NaN, Inf, -Inf)
ma = function(x, n, na.rm=FALSE, nf.rm=FALSE) {
  if (!is.double(x)) x = as.double(x)
  if (!is.integer(n)) n = as.integer(n)
  ans = rep(NA_real_, nx<-length(x))
  if (nf.rm) x[!is.finite(x)] = NA_real_ # exact=F consistency due to https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17441
  for (i in n:nx) ans[i]=mean(x[(i-n+1):i], na.rm=na.rm)
  ans
}
n = 4
x = 1:16
x[5] = NaN
test(9999.99, identical(frollmean(x, n), ma(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE), ma(x, n)))
x[6] = NA
test(9999.99, identical(frollmean(x, n), ma(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE), ma(x, n)))
#### test inconsistency of NaN-NA order is consistent to https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17441
x[5] = NA
x[6] = NaN
test(9999.99, identical(frollmean(x, n), ma(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE), ma(x, n)))
x[5] = Inf
test(9999.99, identical(frollmean(x, n), ma(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE), ma(x, n)))
x[6] = -Inf
test(9999.99, identical(frollmean(x, n), ma(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE), ma(x, n)))
x[5:7] = c(NA, Inf, -Inf)
test(9999.99, identical(frollmean(x, n), ma(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE), ma(x, n)))

#### adaptive window
ama = function(x, n, na.rm=FALSE, fill=NA, nf.rm=FALSE) {
  # adaptive moving average in R
  stopifnot((nx<-length(x))==length(n))
  if (nf.rm) x[!is.finite(x)] = NA_real_
  ans = rep(NA_real_, nx)
  for (i in seq_along(x)) {
    ans[i] = if (i >= n[i])
      mean(x[(i-n[i]+1):i], na.rm=na.rm)
    else as.double(fill)
  }
  ans
}

x = rnorm(1e3)
n = rep(20L, 1e3) # pseudo adaptive
test(9999.99, frollmean(x, n[1L]), frollmean(x, n, adaptive=TRUE)) # n auto wrapped in list
test(9999.99, frollmean(x, n[1L]), frollmean(x, list(n), adaptive=TRUE))
test(9999.99, frollmean(x, n[1L]), frollmean(x, n, exact=TRUE, adaptive=TRUE))

x = c(1:4,2:5,4:6,5L)
n = c(2L, 2L, 2L, 5L, 4L, 5L, 1L, 1L, 2L, 3L, 6L, 3L)
ans1 = ama(x, n)
ans2 = frollmean(x, list(n), adaptive=TRUE)
ans3 = frollmean(x, list(n), exact=TRUE, adaptive=TRUE)
test(9999.99, ans1, ans2)
test(9999.99, ans1, ans3)

x = data.table(x=x, y=x/2) # multiple columns and multiple windows
ln = list(n, n+1L)
ans1 = list(ama(x[[1L]], ln[[1L]]), ama(x[[1L]], ln[[2L]]), ama(x[[2L]], ln[[1L]]), ama(x[[2L]], ln[[2L]]))
ans2 = frollmean(x, ln, adaptive=TRUE)
ans3 = frollmean(x, ln, exact=TRUE, adaptive=TRUE)
test(9999.99, ans1, ans2)
test(9999.99, ans1, ans3)

#### adaptive fill
x = c(1:4,2:5,4:6,5L)
n = c(2L, 2L, 2L, 5L, 4L, 5L, 1L, 1L, 2L, 3L, 6L, 3L)
ans1 = ama(x, n, fill=150)
ans2 = frollmean(x, n, adaptive=TRUE, fill=150)
ans3 = frollmean(x, n, adaptive=TRUE, exact=TRUE, fill=150)
test(9999.99, ans1, ans2)
test(9999.99, ans1, ans3)

#### adaptive na.rm
x = c(1:4,NA,2:5,NA,4:6,NA,5L)
n = c(2L, 2L, 2L, 5L, 3L, 4L, 5L, 1L, 2L, 1L, 2L, 4L, 3L, 6L, 3L)
ans1 = ama(x, n)
ans2 = frollmean(x, n, adaptive=TRUE)
ans3 = frollmean(x, n, exact=TRUE, adaptive=TRUE)
test(9999.99, ans1, ans2)
test(9999.99, ans1, ans3)
ans1 = ama(x, n, na.rm=TRUE)
ans2 = frollmean(x, n, na.rm=TRUE, adaptive=TRUE)
ans3 = frollmean(x, n, na.rm=TRUE, exact=TRUE, adaptive=TRUE)
test(9999.99, ans1, ans2)
test(9999.99, ans1, ans3)
#### TODO test 5e9 vector where 3e9 are NAs to confirm uint_fast64_t running NA counter
if (FALSE) {
  #x = sample(c(rnorm(2e9), rep(NA_real_, 3e9)))
  #n = sample(c(1e3, 1e4, 1e5), length(x), TRUE)
  #ans = frollmean(x, list(n), adaptive=TRUE)
}

#### adaptive limitations
test(9999.99, frollmean(1:2, 1:2, adaptive=TRUE, align="right"), c(1, 1.5))
test(9999.99, frollmean(1:2, 1:2, adaptive=TRUE, align="center"), error="using adaptive TRUE and align argument different than 'right' is not implemented")
test(9999.99, frollmean(1:2, 1:2, adaptive=TRUE, align="left"), error="using adaptive TRUE and align argument different than 'right' is not implemented")
test(9999.99, frollmean(list(1:2, 1:3), list(1:2), adaptive=TRUE), error="adaptive rolling function can only process 'x' having equal length of elements, like data.table or data.frame. If you want to call rolling function on list having variable length of elements call it for each field separately")

#### adaptive exact
fastama = function(x, n, na.rm, fill=NA) {
  if (!missing(na.rm)) stop("fast adaptive moving average implemented in R does not handle NAs, input having NAs will result in incorrect answer so not even try to compare to it")
  # fast implementation of adaptive moving average in R, in case of NAs incorrect answer
  stopifnot((nx<-length(x))==length(n))
  cs = cumsum(x)
  ans = rep(NA_real_, nx)
  for (i in seq_along(cs)) {
    ans[i] = if (i == n[i])
      cs[i]/n[i]
    else if (i > n[i])
      (cs[i]-cs[i-n[i]])/n[i]
    else as.double(fill)
  }
  ans
}
x = c(1:3, 1e9L, 2:5, 5e9, 4:6)
n = c(2L, 2L, 2L, 5L, 4L, 5L, 1L, 1L, 2L, 3L, 6L, 3L)
ans1 = ama(x, n)
ans2 = frollmean(x, n, adaptive=TRUE)
ans3 = frollmean(x, n, adaptive=TRUE, exact=TRUE)
ans4 = fastama(x, n)
test(9999.99, ans1, ans2)
test(9999.99, ans1, ans3)
test(9999.99, ans1, ans4)

x = sample(c(rnorm(1e3, 1e2), rnorm(1e1, 1e9, 1e2), abs(rnorm(1e1, 1e-9, 1e-2))))
n = sample(1:20, length(x), TRUE)
ans1 = ama(x, n)
ans2 = frollmean(x, n, adaptive=TRUE)
ans3 = frollmean(x, n, adaptive=TRUE, exact=TRUE)
ans4 = fastama(x, n)
anserr = list(
  froll_exact_f = ans1-ans2,
  froll_exact_t = ans1-ans3,
  fastama = ans1-ans4
)
errs = lapply(lapply(anserr, abs), sum, na.rm=TRUE)
test(9999.99, errs[["froll_exact_t"]] < errs[["froll_exact_f"]])
test(9999.99, errs[["froll_exact_t"]] < errs[["fastama"]])

x = sample(c(1:100, 5e9, 5e-9))
n = sample(1:10, length(x), TRUE)
ans1 = ama(x, n)
ans2 = frollmean(x, n, adaptive=TRUE)
ans3 = frollmean(x, n, adaptive=TRUE, exact=TRUE)
ans4 = fastama(x, n)
anserr = list(
  froll_exact_f = ans1-ans2,
  froll_exact_t = ans1-ans3,
  fastama = ans1-ans4
)
errs = lapply(lapply(anserr, abs), sum, na.rm=TRUE)
test(9999.99, errs[["froll_exact_t"]] < errs[["froll_exact_f"]])
test(9999.99, errs[["froll_exact_t"]] < errs[["fastama"]])

## edge cases adaptive
#### is.integer(n)
test(9999.99, frollmean(1:5, 1:5, adaptive=TRUE), seq(1,3,0.5))
#### is.integer(n) && length(n)!=length(x)
test(9999.99, frollmean(1:10, 1:5, adaptive=TRUE), error="length of integer vector(s) provided as list to 'n' argument must be equal to number of observations provided in 'x'")
#### is.list(n) && length(n[[1L]])!=length(x)
test(9999.99, frollmean(1:10, list(1:5), adaptive=TRUE), error="length of integer vector(s) provided as list to 'n' argument must be equal to number of observations provided in 'x'")

#### non-finite values (NA, NaN, Inf, -Inf)
n = c(4,1,4,5,5,4,6,5,4,4,2,3,4,3,2,4)
x = 1:16
x[5] = NaN
test(9999.99, identical(frollmean(x, n, adaptive=TRUE), ama(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE, adaptive=TRUE), ama(x, n)))
x[6] = NA
test(9999.99, identical(frollmean(x, n, adaptive=TRUE), ama(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE, adaptive=TRUE), ama(x, n)))
#### test inconsistency of NaN-NA order is consistent to https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17441
x[5] = NA
x[6] = NaN
test(9999.99, identical(frollmean(x, n, adaptive=TRUE), ama(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE, adaptive=TRUE), ama(x, n)))
x[5] = Inf
test(9999.99, identical(frollmean(x, n, adaptive=TRUE), ama(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE, adaptive=TRUE), ama(x, n)))
x[6] = -Inf
test(9999.99, identical(frollmean(x, n, adaptive=TRUE), ama(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE, adaptive=TRUE), ama(x, n)))
x[5:7] = c(NA, Inf, -Inf)
test(9999.99, identical(frollmean(x, n, adaptive=TRUE), ama(x, n, nf.rm=TRUE)))
test(9999.99, identical(frollmean(x, n, exact=TRUE, adaptive=TRUE), ama(x, n)))

## test verbose messages
x = 1:10
n = 3
test(9999.99, frollmean(x, n, verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmean: running for input length 10, window 3, align 1, hasna 0, narm 0"))
test(9999.99, frollmean(list(x, x+1), n, verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 2x1",
  "frollfunR: 2 column(s) and 1 window(s), entering parallel execution, but actually single threaded due to enabled verbose which is not thread safe",
  "frollmean: running for input length 10, window 3, align 1, hasna 0, narm 0",
  "frollmean: running for input length 10, window 3, align 1, hasna 0, narm 0"))
test(9999.99, frollmean(x, c(n, n+1), verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x2",
  "frollfunR: 1 column(s) and 2 window(s), entering parallel execution, but actually single threaded due to enabled verbose which is not thread safe",
  "frollmean: running for input length 10, window 3, align 1, hasna 0, narm 0",
  "frollmean: running for input length 10, window 4, align 1, hasna 0, narm 0"))
test(9999.99, frollmean(list(x, x+1), c(n, n+1), verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 2x2",
  "frollfunR: 2 column(s) and 2 window(s), entering parallel execution, but actually single threaded due to enabled verbose which is not thread safe",
  "frollmean: running for input length 10, window 3, align 1, hasna 0, narm 0",
  "frollmean: running for input length 10, window 4, align 1, hasna 0, narm 0"))
test(9999.99, frollmean(x, n, exact=TRUE, verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmeanExact: running for input length 10, window 3, align 1, hasna 0, narm 0"))
test(9999.99, frollmean(x, n, align="center", verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmean: running for input length 10, window 3, align 0, hasna 0, narm 0",
  "frollmean: align 0, shift answer by -1"))
test(9999.99, frollmean(x, n, align="left", verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmean: running for input length 10, window 3, align -1, hasna 0, narm 0",
  "frollmean: align -1, shift answer by -2"))
nn = c(1:4,2:3,1:4)
test(9999.99, frollmean(x, nn, adaptive=TRUE, verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmeanAdaptive: running for input length 10, hasna 0, narm 0"))
test(9999.99, frollmean(x, nn, exact=TRUE, adaptive=TRUE, verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmeanExactAdaptive: running for input length 10, hasna 0, narm 0"))

x[8] = NA
test(9999.99, frollmean(x, n, verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmean: running for input length 10, window 3, align 1, hasna 0, narm 0",
  "frollmean: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs"))
test(9999.99, frollmean(x, n, exact=TRUE, verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmeanExact: running for input length 10, window 3, align 1, hasna 0, narm 0",
  "frollmeanExact: NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run"))
test(9999.99, frollmean(x, nn, adaptive=TRUE, verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmeanAdaptive: running for input length 10, hasna 0, narm 0",
  "frollmeanAdaptive: NA (or other non-finite) value(s) are present in input, re-running with extra care for NAs"))
test(9999.99, frollmean(x, nn, exact=TRUE, adaptive=TRUE, verbose=TRUE), output=c(
  "frollfunR: allocating memory for results 1x1",
  "frollfunR: single column and single window, parallel processing by multiple answer vectors skipped",
  "frollmeanExactAdaptive: running for input length 10, hasna 0, narm 0",
  "frollmeanExactAdaptive: NA (or other non-finite) value(s) are present in input, na.rm was FALSE so in 'exact' implementation NAs were handled already, no need to re-run"))

## validation

set.seed(108)
makeNA = function(x, ratio=0.1) {id=sample(length(x), as.integer(length(x) * ratio)); x[id]=NA; x}
num = 9999.9
#### against zoo
if (requireNamespace("zoo", quietly=TRUE)) {
  drollapply = function(...) as.double(zoo::rollapply(...)) # rollapply is not consistent in data type of answer, force to double
  zoo_compare = function(x, n) {
    num.step = 0.0001
    #### fun, align, na.rm, exact
    for (fun in c("mean")) { # ,"sum"
      for (align in c("right","center","left")) {
        for (na.rm in c(FALSE, TRUE)) {
          for (exact in c(FALSE, TRUE)) {
            num <<- num + num.step
            eval(substitute(
              test(.num,
                   froll(.fun, x, n, align=.align, na.rm=.na.rm, exact=.exact),
                   drollapply(x, n, FUN=.fun, fill=NA, align=.align, na.rm=.na.rm)),
              
              list(.num=num, .fun=fun, .align=align, .na.rm=na.rm, .exact=exact)
            ))
          }
        }
      }
    }
  }
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
  x = makeNA(rnorm(1e3)); n = 50
  zoo_compare(x, n)
  x = makeNA(rnorm(1e3+1)); n = 50
  zoo_compare(x, n)
  x = makeNA(rnorm(1e3)); n = 51
  zoo_compare(x, n)
  x = makeNA(rnorm(1e3+1)); n = 51
  zoo_compare(x, n)
}
#### adaptive moving average compare
afun = function(fun, x, n, na.rm=FALSE, fill=NA, nf.rm=FALSE) {
  # adaptive moving average in R
  stopifnot((nx<-length(x))==length(n))
  ans = rep(NA_real_, nx)
  if (nf.rm) x[!is.finite(x)] = NA_real_
  FUN = match.fun(fun)
  for (i in seq_along(x)) {
    ans[i] = if (i >= n[i])
      FUN(x[(i-n[i]+1):i], na.rm=na.rm)
    else as.double(fill)
  }
  ans
}
afun_compare = function(x, n) {
  num.step = 0.0001
  #### fun, na.rm, exact
  for (fun in c("mean")) { # ,"sum"
    for (na.rm in c(FALSE, TRUE)) {
      for (exact in c(FALSE, TRUE)) {
        num <<- num + num.step
        eval(substitute(
          test(.num,
               froll(.fun, x, n, na.rm=.na.rm, exact=.exact, adaptive=TRUE),
               afun(.fun, x, n, fill=NA, na.rm=.na.rm, nf.rm=!.exact)),
          list(.num=num, .fun=fun, .na.rm=na.rm, .exact=exact)
        ))
      }
    }
  }
}
#### no NA
x = rnorm(1e3); n = sample(50, length(x), TRUE) # x even, n even
afun_compare(x, n)
x = rnorm(1e3+1); n = sample(50, length(x), TRUE) # x odd, n even
afun_compare(x, n)
x = rnorm(1e3); n = sample(51, length(x), TRUE) # x even, n odd
afun_compare(x, n)
x = rnorm(1e3+1); n = sample(51, length(x), TRUE) # x odd, n odd
afun_compare(x, n)
#### leading and trailing NAs
x = c(rep(NA, 60), rnorm(1e3), rep(NA, 60)); n = sample(50, length(x), TRUE)
afun_compare(x, n)
x = c(rep(NA, 60), rnorm(1e3+1), rep(NA, 60)); n = sample(50, length(x), TRUE)
afun_compare(x, n)
x = c(rep(NA, 60), rnorm(1e3), rep(NA, 60)); n = sample(51, length(x), TRUE)
afun_compare(x, n)
x = c(rep(NA, 60), rnorm(1e3+1), rep(NA, 60)); n = sample(51, length(x), TRUE)
afun_compare(x, n)
#### random NA
x = makeNA(rnorm(1e3)); n = sample(50, length(x), TRUE)
afun_compare(x, n)
x = makeNA(rnorm(1e3+1)); n = sample(50, length(x), TRUE)
afun_compare(x, n)
x = makeNA(rnorm(1e3)); n = sample(51, length(x), TRUE)
afun_compare(x, n)
x = makeNA(rnorm(1e3+1)); n = sample(51, length(x), TRUE)
afun_compare(x, n)
rm(num)

if (dev_and_benchmark_area<-FALSE) {
  
  # TODO benchmark alt C impl exact=F: tmp<-cumsum(x); (tmp-shift(tmp, k))/k
  # if faster than current sliding window add add to api
  
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
  
  ## adaptive moving average R
  ama = function(x, n, na.rm=FALSE, fill=NA, nf.rm=FALSE) {
    # adaptive moving average in R
    stopifnot((nx<-length(x))==length(n))
    if (nf.rm) x[!is.finite(x)] = NA_real_
    ans = rep(NA_real_, nx)
    for (i in seq_along(x)) {
      ans[i] = if (i >= n[i])
        mean(x[(i-n[i]+1):i], na.rm=na.rm)
      else as.double(fill)
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
      ans[i] = if (i == n[i])
        cs[i]/n[i]
      else if (i > n[i])
        (cs[i]-cs[i-n[i]])/n[i]
      else as.double(fill)
    }
    ans
  }
  
}

setDTthreads(oldDTthreads)
cat("froll unit tests successfully passed\n")
