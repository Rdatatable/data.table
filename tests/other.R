require(data.table)
# integration tests for packages excluded from Suggests in 1.10.5
# for list of used packages see inst/tests/tests-DESCRIPTION
if (as.logical(Sys.getenv("TEST_DATA_TABLE_WITH_OTHER_PACKAGES","FALSE"))) test.data.table(script="other.Rraw")
