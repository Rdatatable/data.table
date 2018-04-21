## this file will be migrated to inst/tests/tests.Rraw when branch will be ready to merge

test = function(num, x, y) stopifnot(all.equal(x, y))

## rolling features

d = as.data.table(list(1:6/2, 3:8/4))

#### multiple columns at once
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

#### align: left/center/right
NULL
#### fill constant
NULL
#### adaptive window
NULL

## validation

#### against zoo
if (requireNamespace("zoo", quietly=TRUE)) {
  NULL
}

#### adaptive window against https://stackoverflow.com/a/21368246/2490497

## cleanup
if (FALSE) {
  test = data.table:::test.data.table
} else {
  rm(test)
}
