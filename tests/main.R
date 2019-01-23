require(data.table)

# From R 3.6.0 onwards, we can check that && and || are using only length-1 logicals (in the test suite)
# rather than relying on x && y being equivalent to x[[1L]] && y[[1L]]  silently.
orig__R_CHECK_LENGTH_1_LOGIC2_ <- Sys.getenv("_R_CHECK_LENGTH_1_LOGIC2_", unset = NA_character_)
Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = TRUE)
# This environment variable is restored to its previous state (including not defined) at the end of this file

ans = test.data.table()  # runs the main test suite of 5,000+ tests in /inst/tests/tests.Rraw

# summary
test.data.table.summary = function(env) {
  attach(ans_env)
  plat = paste0("endian==", .Platform$endian,
                ", sizeof(long double)==", .Machine$sizeof.longdouble,
                ", sizeof(pointer)==", .Machine$sizeof.pointer,
                ", TZ=", suppressWarnings(Sys.timezone()),
                ", locale='", Sys.getlocale(), "'",
                ", l10n_info()='", paste0(names(l10n_info()), "=", l10n_info(), collapse="; "), "'")
  DT = head(timings[-1L][order(-time)],10)   # exclude id 1 as in dev that includes JIT
  if ((x<-timings[,sum(nTest)]) != ntest) warning("Timings count mismatch:",x,"vs",ntest)
  cat("\n10 longest running tests took ", as.integer(tt<-DT[, sum(time)]), "s (", as.integer(100*tt/(ss<-timings[,sum(time)])), "% of ", as.integer(ss), "s)\n", sep="")
  print(DT, class=FALSE)
  if (memtest) {
    ..inittime = inittime
    m = fread("memtest.csv")[inittime==..inittime]
    if (nrow(m)) {
      ps_na = all(is.na(m[["PS_rss"]])) # OS with no 'ps -o rss R' support
      png("memtest.png")
      p = par(mfrow=c(if (ps_na) 2 else 3, 2))
      if (!ps_na) {
        m[, plot(test, PS_rss, pch=18, xlab="test num", ylab="mem MB", main="ps -o rss R")]
        m[, plot(timestamp, PS_rss, type="l", xlab="timestamp", ylab="mem MB", main="ps -o rss R")]
      }
      m[, plot(test, GC_used, pch=18, xlab="test num", ylab="mem MB", main="gc used")]
      m[, plot(timestamp, GC_used, type="l", xlab="timestamp", ylab="mem MB", main="gc used")]
      m[, plot(test, GC_max_used, pch=18, xlab="test num", ylab="mem MB", main="gc max used")]
      m[, plot(timestamp, GC_max_used, type="l", xlab="timestamp", ylab="mem MB", main="gc max used")]
      par(p)
      dev.off()
    } else {
      warning("test.data.table runs with memory testing but did not collect any memory statistics.")
    }
  }
  if (nfail > 0) {
    if (nfail>1) {s1="s";s2="s: "} else {s1="";s2=" "}
    cat("\r")
    stop(nfail," error",s1," out of ",ntest," in ",timetaken(started.at)," on ",date(),". [",plat,"].",
         " Search inst/tests/tests.Rraw for test number",s2,paste(whichfail,collapse=", "),".")
    # important to stop() here, so that 'R CMD check' fails
  }
  cat(plat,"\n\nAll ",ntest," tests in inst/tests/tests.Rraw completed ok in ",timetaken(started.at)," on ",date(),"\n",sep="")
  # date() is included so we can tell exactly when these tests ran on CRAN. Sometimes a CRAN log can show error but that can be just
  # stale due to not updating yet since a fix in R-devel, for example.
}
test.data.table.summary(attr(ans, "details", TRUE))

if (is.na(orig__R_CHECK_LENGTH_1_LOGIC2_)) {
  Sys.unsetenv("_R_CHECK_LENGTH_1_LOGIC2_")
} else {
  Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = orig__R_CHECK_LENGTH_1_LOGIC2_)
}

# integration tests for packages excluded from Suggests in 1.10.5
# for list of used packages see inst/tests/tests-DESCRIPTION
with.other.packages = as.logical(Sys.getenv("TEST_DATA_TABLE_WITH_OTHER_PACKAGES","FALSE"))
if (with.other.packages) test.data.table(with.other.packages=with.other.packages)

# Turn off verbose repeat to save time (particularly Travis, but also CRAN) :
# test.data.table(verbose=TRUE)
# Calling it again in the past revealed some memory bugs but also verbose mode checks the verbose messages run ok
# TO DO: check we test each verbose message at least once, instead of a full repeat of all tests
