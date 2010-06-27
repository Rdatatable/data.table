\name{test.data.table}
\alias{test.data.table}
\title{ Runs a set of tests. }
\description{
  Runs a set of tests that data.table is working, 98 tests at the time of writing this text.
  Ideally the this function should be ported to RUnit.
}
\usage{
test.data.table()
}
% \arguments{
% }
\details{
   Runs a series of tests. These can be used to see features and examples of usage - type 'test.data.table' to see the tests.
}
\value{
    TRUE if all tests were successful. FALSE otherwise.
}
% \references{ }
\author{ Matthew Dowle }
% \note{ }
\seealso{ \code{\link{data.table}} }
\examples{
    try(library(ggplot2))
    test.data.table()		# this will run inside the data.table namespace
    eval(body(test.data.table))		# as though you copied and pasted its body into the console
		# there may be differences (e.g. the first may find non-exported functions inside data.table)
		# whereas the 2nd is a truer test.  We do both ways for full robustness.
}
\keyword{ data }
