tinfo <- atime::atime_pkg_test_info("~/R/data.table")
names(tinfo$test.call)
tname <- "DT[by] fixed in #4558"
tcall <- tinfo$test.call[[tname]]
hist.sha.list <- as.list(tcall[c("Before", "Regression", "Fixed")])
pr.sha.list <- tcall$sha.vec
HEAD <- grep("HEAD", names(pr.sha.list), value=TRUE)
tcall[c("Before", "Regression", "Fixed")] <- NULL
tcall$sha.vec <- c(hist.sha.list, pr.sha.list)[c(HEAD, "Before")]
names(tcall$sha.vec) <- c("HEAD","Before")
##tcall$seconds.limit <- 1
tres <- eval(tcall)
saveRDS(tres, "run-7687.rds")
library(data.table)
print(meas_wide <- dcast(
  tres$meas, N ~ expr.name, value.var="kilobytes"
)[, ratio := HEAD/Before][max(which(!is.na(ratio)))])
q(status=ifelse(meas_wide$ratio>1.4, 1, 0))
