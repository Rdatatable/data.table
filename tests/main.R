require(data.table)

test.data.table()  # runs the main test suite of 5,000+ tests in /inst/tests/tests.Rraw

# Turn on showProgress temporarily for GLCI and CRAN (not interactive()) when a segfault
# occurs and it's taking time to reproduce locally. See comments in PR#4090.
# test.data.table(showProgress=TRUE)

# Turn off verbose repeat to save time (particularly Travis, but also CRAN) :
# test.data.table(verbose=TRUE)
# Calling it again in the past revealed some memory bugs but also verbose mode checks the verbose messages run ok
# TO DO: check we test each verbose message at least once, instead of a full repeat of all tests
