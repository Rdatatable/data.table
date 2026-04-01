library(data.table)
tres <- readRDS("run-7687.rds")
tref <- atime::references_best(tres)
plot(tref)
dcast(
  tref$meas, N + unit ~ expr.name, value.var="empirical"
)[, ratio := HEAD/Before][, inverse := 1/ratio][!is.na(ratio)][N==max(N)]
tpred <- predict(tref, seconds=1, kilobytes=1e4)
tpred <- predict(tref, seconds=1, kilobytes=2e4)
plot(tpred)
dcast(
  tpred$prediction, unit ~ expr.name, value.var="N"
)[, ratio := Before/HEAD][, inverse := 1/ratio][]
