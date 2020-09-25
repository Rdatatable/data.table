require(data.table)

# test.data.table()  # runs the main test suite of 5,000+ tests in /inst/tests/tests.Rraw

# turning on showProgress temporarily to find where segfault is that GLCI shows with R 3.4 and R 3.1
# showProgress is default interactive() so it's off for CRAN but also off for GLCI
# strict R-devel passes locally as does R 3.1 locally, so now running with strict torture locally but that's still running
test.data.table(showProgress=TRUE)

# Turn off verbose repeat to save time (particularly Travis, but also CRAN) :
# test.data.table(verbose=TRUE)
# Calling it again in the past revealed some memory bugs but also verbose mode checks the verbose messages run ok
# TO DO: check we test each verbose message at least once, instead of a full repeat of all tests
