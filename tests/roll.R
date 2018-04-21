## this file will be migrated to inst/tests/tests.Rraw when branch will be ready to merge
if (!interactive()) {
  library(data.table)
  test = function(num, x, y) stopifnot(all.equal(x, y))
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
#ans = rollmean(d, 3, align="center")
#expected = list(
#  c(rep(NA_real_,2), seq(1,2.5,0.5)),
#  c(rep(NA_real_,2), seq(1,1.75,0.25))
#)
#test(9999.7, ans, expected)
#ans = rollmean(d, 3, align="left")
#expected = list(
#  c(rep(NA_real_,2), seq(1,2.5,0.5)),
#  c(rep(NA_real_,2), seq(1,1.75,0.25))
#)
#test(9999.8, ans, expected)

#### handling NAs
NULL
#### fill constant
NULL
#### adaptive window
NULL

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

}

#### adaptive window against https://stackoverflow.com/a/21368246/2490497
