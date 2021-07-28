require(data.table)
# integration tests for packages excluded from Suggests in 1.10.5
# for list of used packages see inst/tests/tests-DESCRIPTION
if (as.logical(Sys.getenv("TEST_DATA_TABLE_WITH_OTHER_PACKAGES","FALSE"))) {

  options(warn=1)
  # test.data.table() turns on R's warnPartial* options and currently there
  # are partial argument names used in base and other packages. Without the
  # options(warn=1), other.Rout just contains "There were 16 warnings (use
  # warnings() to see them)". However, a print(warnings()) after test.data.table()
  # just results in NULL in other.Rout. Hence options(warn=1) because that
  # worked to display the warnings, not because we want them displayed at the
  # time per se.
    
  test.data.table(script="other.Rraw")
}

