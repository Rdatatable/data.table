## this file will be migrated to inst/tests/tests.Rraw when branch will be ready to merge
if (!interactive()) {
  library(data.table)
  test = data.table:::test
}

## rolling features

#### multiple columns at once
d = as.data.table(list(1:6/2, 3:8/4))
ans = rollmean(d, 3)
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)),
  c(rep(NA_real_,2), seq(1,1.75,0.25))
)
test(9999.1, ans, expected)

#### multiple windows at once
ans = rollmean(d[, .(V1)], c(3, 4))
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)), c(rep(NA_real_,3), seq(1.25,2.25,0.5))
)
test(9999.2, ans, expected)

#### multiple columns and multiple windows at once
ans = rollmean(d, c(3, 4))
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)), c(rep(NA_real_,3), seq(1.25,2.25,0.5)),
  c(rep(NA_real_,2), seq(1,1.75,0.25)), c(rep(NA_real_,3), seq(1.125,1.625,0.25))
)
test(9999.3, ans, expected)

#### atomic vectors input and single window returns atomic vectors
ans = rollmean(d[["V1"]], 3)
expected = c(rep(NA_real_,2), seq(1,2.5,0.5))
test(9999.4, ans, expected)

#### various length list vectors
l = list(1:6/2, 3:10/4)
ans = rollmean(l, c(3, 4))
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)), c(rep(NA_real_,3), seq(1.25,2.25,0.5)),
  c(rep(NA_real_,2), seq(1,2.25,0.25)), c(rep(NA_real_,3), seq(1.125,2.125,0.25))
)
test(9999.5, ans, expected)

#### align: right/center/left
ans = rollmean(d, 3, align="right") # default
expected = list(
  c(rep(NA_real_,2), seq(1,2.5,0.5)),
  c(rep(NA_real_,2), seq(1,1.75,0.25))
)
test(9999.6, ans, expected)
#ans = rollmean(d, 3, align="center") # x even, n odd
#expected = list(
#  c(NA_real_, seq(1,2.5,0.5), NA_real_),
#  c(NA_real_, seq(1,1.75,0.25), NA_real_)
#)
#test(9999.7, ans, expected)
#ans = rollmean(d, 4, align="center") # x even, n even
#expected = list(
#  c(NA_real_, seq(1.25,2.25,0.5), rep(NA_real_,2)),
#  c(NA_real_, seq(1.125,1.625,0.25), rep(NA_real_,2))
#)
#test(9999.8, ans, expected)
#de = rbind(d, data.table(3.5, 2.25))
#ans = rollmean(de, 3, align="center") # x odd, n odd
#expected = list(
#  c(NA_real_, seq(1,3,0.5), NA_real_),
#  c(NA_real_, seq(1,2,0.25), NA_real_)
#)
#test(9999.9, ans, expected)
#ans = rollmean(de, 4, align="center") # x odd, n even
#expected = list(
#  c(NA_real_, seq(1.25,2.75,0.5), rep(NA_real_,2)),
#  c(NA_real_, seq(1.125,1.875,0.25), rep(NA_real_,2))
#)
#test(9999.10, ans, expected)
#ans = rollmean(d, 3, align="left")
#expected = list(
#  c(seq(1,2.5,0.5), rep(NA_real_,2)),
#  c(seq(1,1.75,0.25), rep(NA_real_,2))
#)
#test(9999.11, ans, expected)

#### handling NAs
#d = as.data.table(list(1:6/2, 3:8/4))
#d[c(2L, 5L), V1:=NA][4:6, V2:=NA]
#ans = rollmean(d, 2:3)
#expected = list(c(NA, NA, NA, 1.75, NA, NA), rep(NA_real_, 6), c(NA, 0.875, 1.125, rep(NA, 3)), c(NA, NA, 1, rep(NA, 3)))
#test(9999.99, ans, expected)
ans = rollmean(d, 2:3, na.rm=TRUE)
expected = list(c(NA, 0.5, 1.5, 1.75, 2, 3), c(NA, NA, 1, 1.75, 1.75, 2.5), c(NA, 0.875, 1.125, 1.25, NaN, NaN), c(NA, NA, 1, 1.125, 1.25, NaN))
test(9999.99, ans, expected)

#### fill constant
test(9999.99, rollmean(1:5, 4, fill=0), c(0, 0, 0, 2.5, 3.5))
test(9999.99, rollmean(1:5, 4, fill=-5), c(-5, -5, -5, 2.5, 3.5))
test(9999.99, rollmean(1:5, 4, fill=100), c(100, 100, 100, 2.5, 3.5))
test(9999.99, rollmean(1:5, 4, fill=Inf), c(Inf, Inf, Inf, 2.5, 3.5))
test(9999.99, rollmean(1:5, 4, fill=NaN), c(NaN, NaN, NaN, 2.5, 3.5))

# these are the tests that waiting for na.rm=FALSE and ISNAN to be resolved
# for na.rm=FALSE they can now produce incorrect result
# still it is consistent to zoo::rollmean
# it was reported and confirmed as bug on 2018-04-23
#test(9999.99, rollmean(c(1L, NA, 3L, 4L, 5L), 4, fill=0), c(0, 0, 0, NA, NA))
#test(9999.99, rollmean(c(1L, NA, 3L, 4L, 5L), 2, fill=0), c(0, 0, 0, NA, NA))
#x = c(1L, NA, 3L, 4L, 5L)
# bad
#rollmean(x, 2, fill=0)
#zoo::rollmean(x, 2, fill=0, align="right")
#zoo::rollapply(x, 2, mean, fill=0, align="right", na.rm=FALSE)
# good
#rollmean(x, 2, fill=0, na.rm=TRUE)
#zoo::rollapply(x, 2, mean, fill=0, align="right", na.rm=TRUE)
# bad
#rollmean(x, 2, fill=NA)
#zoo::rollapply(x, 2, mean, fill=NA, align="right")
# good
#rollmean(x, 2, fill=NA, na.rm=TRUE)
#zoo::rollapply(x, 2, mean, fill=NA, align="right", na.rm=TRUE)

#### adaptive window
NULL

## edge cases

#### length(x)==0
test(9999.99, rollmean(numeric(0), 2), numeric(0))
test(9999.99, rollmean(list(1:3, numeric()), 2), list(c(NA_real_, 1.5, 2.5), numeric(0)))

#### length(n)==0
test(9999.99, rollmean(1:3, integer()), error="n must be non 0 length")
test(9999.99, rollmean(list(1:3, 2:4), integer()), error="n must be non 0 length")

#### n==0
test(9999.99, rollmean(1:3, c(2,0)), list(c(NA_real_, 1.5, 2.5), rep(NaN, 3)))
test(9999.99, rollmean(list(1:3, 2:4), 0), list(rep(NaN, 3), rep(NaN, 3)))

#### n<0
test(9999.99, rollmean(1:3, -2), error="n must be non-negative integer values")

#### n[[1L]]>0 && n[[2L]]<0
test(9999.99, rollmean(1:3, c(2, -2)), error="n must be non-negative integer values")

#### n[[1L]]==n[[2L]]
test(9999.99, rollmean(1:3, c(2, 2)), list(c(NA_real_, 1.5, 2.5), c(NA_real_, 1.5, 2.5)))
test(9999.99, rollmean(list(1:3, 4:6), c(2, 2)), list(c(NA_real_, 1.5, 2.5), c(NA_real_, 1.5, 2.5), c(NA_real_, 4.5, 5.5), c(NA_real_, 4.5, 5.5)))

#### n>length(x)
test(9999.99, rollmean(list(1:3, 4:6), 4), list(c(NA_real_, NA_real_, NA_real_), c(NA_real_, NA_real_, NA_real_)))

#### n==length(x)
test(9999.99, rollmean(list(1:3, 4:6), 3), list(c(NA_real_, NA_real_, 2), c(NA_real_, NA_real_, 5)))

#### n<length(x[[1L]]) && n>length(x[[2L]])
test(9999.99, rollmean(list(1:5, 1:2), 3), list(c(NA_real_, NA_real_, 2, 3, 4), c(NA_real_, NA_real_)))

#### length(x)==1 && n==1
test(9999.99, rollmean(5, 1), 5)
test(9999.99, rollmean(list(1, 10, 5), 1), list(1, 10, 5))

#### length(x)==1 && n==2
test(9999.99, rollmean(5, 2), NA_real_)
test(9999.99, rollmean(list(1, 10, 5), 2), list(NA_real_, NA_real_, NA_real_))

#### n==Inf
test(9999.99, rollmean(1:5, Inf), error="n must be non-negative integer values", warning="NAs introduced by coercion to integer range")

#### n==c(5, Inf)
test(9999.99, rollmean(1:5, c(5, Inf)), error="n must be non-negative integer values", warning="NAs introduced by coercion to integer range")

#### is.complex(n)
#rollmean(1:5, 3i)

#### is.character(n)
#rollmean(1:5, "a")

#### is.factor(n)
#rollmean(1:5, as.factor("a"))

#### !adaptive && is.list(n)
#rollmean(11:15, list(1:5), adaptive=FALSE)

#### adaptive && is.integer(n)
#rollmean(11:15, 1:5, adaptive=TRUE)

#### adaptive && is.integer(n) && length(n)!=length(x)
#rollmean(11:15, 1:5, adaptive=TRUE)

#### adaptive && is.list(n) && length(n[[1L]])!=length(x)
#rollmean(11:15, list(1:4), adaptive=TRUE)

## validation

#### against zoo
if (requireNamespace("zoo", quietly=TRUE)) {
  set.seed(5)

  #### align
  x = rnorm(1e3) # x even, n even
  test(9999.51, rollmean(x, 50), zoo::rollmean(x, 50, fill=NA, align="right"))
  #test(9999.52, rollmean(x, 50, align="center"), zoo::rollmean(x, 50, fill=NA))
  #test(9999.53, rollmean(x, 50, align="left"), zoo::rollmean(x, 50, fill=NA, align="left"))
  x = rnorm(1e3+1) # x odd, n even
  test(9999.54, rollmean(x, 50), zoo::rollmean(x, 50, fill=NA, align="right"))
  #test(9999.55, rollmean(x, 50, align="center"), zoo::rollmean(x, 50, fill=NA))
  #test(9999.56, rollmean(x, 50, align="left"), zoo::rollmean(x, 50, fill=NA, align="left"))
  x = rnorm(1e3) # x even, n odd
  test(9999.57, rollmean(x, 51), zoo::rollmean(x, 51, fill=NA, align="right"))
  #test(9999.58, rollmean(x, 51, align="center"), zoo::rollmean(x, 51, fill=NA))
  #test(9999.59, rollmean(x, 51, align="left"), zoo::rollmean(x, 51, fill=NA, align="left"))
  x = rnorm(1e3+1) # x odd, n odd
  test(9999.60, rollmean(x, 51), zoo::rollmean(x, 51, fill=NA, align="right"))
  #test(9999.61, rollmean(x, 51, align="center"), zoo::rollmean(x, 51, fill=NA))
  #test(9999.62, rollmean(x, 51, align="left"), zoo::rollmean(x, 51, fill=NA, align="left"))

  #### na.rm FALSE
  d = as.data.table(list(1:6/2, 3:8/4))
  d[c(2L, 5L), V1:=NA][4:6, V2:=NA]
  #ans = rollmean(d, 2:3)
  #unexpected = list(
  #  zoo::rollmean(d[[1L]], 2L, fill=NA, align="right"),
  #  zoo::rollmean(d[[1L]], 3L, fill=NA, align="right"),
  #  zoo::rollmean(d[[2L]], 2L, fill=NA, align="right"),
  #  zoo::rollmean(d[[2L]], 3L, fill=NA, align="right")
  #) # reported on 2018-04-23 with zoo 1.8-1
  #expected = list(
  #  zoo::rollapply(d[[1L]], 2L, mean, fill=NA, align="right"),
  #  zoo::rollapply(d[[1L]], 3L, mean, fill=NA, align="right"),
  #  zoo::rollapply(d[[2L]], 2L, mean, fill=NA, align="right"),
  #  zoo::rollapply(d[[2L]], 3L, mean, fill=NA, align="right")
  #)
  #test(9999.99, ans, expected)
  #### na.rm TRUE
  ans = rollmean(d, 2:3, na.rm=TRUE)
  expected = list(
    zoo::rollapply(d[[1L]], 2L, mean, na.rm=TRUE, fill=NA, align="right"),
    zoo::rollapply(d[[1L]], 3L, mean, na.rm=TRUE, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 2L, mean, na.rm=TRUE, fill=NA, align="right"),
    zoo::rollapply(d[[2L]], 3L, mean, na.rm=TRUE, fill=NA, align="right")
  )
  test(9999.99, ans, expected)

}

#### adaptive window against https://stackoverflow.com/a/21368246/2490497
