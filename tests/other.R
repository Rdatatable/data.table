library(data.table)
if (!as.logical(Sys.getenv("TEST_DATA_TABLE_WITH_OTHER_PACKAGES", "FALSE"))) {
  cat("Skipping tests in 'other' and quitting, set TEST_DATA_TABLE_WITH_OTHER_PACKAGES to proceed.\n")
  q('no')
}

test.data.table(script="other.Rraw")
