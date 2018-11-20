require(data.table)
test.data.table()  # runs the main test suite of 5,000+ tests in /inst/tests/tests.Rraw

# integration tests for packages excluded from Suggests in 1.10.5
# for list of used packages see inst/tests/tests-DESCRIPTION
with.other.packages = as.logical(Sys.getenv("TEST_DATA_TABLE_WITH_OTHER_PACKAGES","FALSE"))
if (with.other.packages) test.data.table(with.other.packages=with.other.packages)

# Turn off verbose repeat to save time (particularly Travis, but also CRAN) :
# test.data.table(verbose=TRUE)
# Calling it again in the past revealed some memory bugs but also verbose mode checks the verbose messages run ok
# TO DO: check we test each verbose message at least once, instead of a full repeat of all tests



