\name{test}
\alias{test}
\title{ Test assertions for equality, exceptions and console output }
\description{
  An internal function used to tests equality, errors, warnings, messages, console output.
}
\usage{
test(num, x, y = TRUE,
     error = NULL, warning = NULL, message = NULL,
     output = NULL, notOutput = NULL)
}
\arguments{
\item{num}{ A unique identifier for a test, helpful in identifying the source of failure when testing is not working. Currently, we use a manually-incremented system with tests formatted as \code{n.m}, where essentially \code{n} indexes an issue and \code{m} indexes aspects of that issue. For the most part, your new PR should only have one value of \code{n} (scroll to the end of \code{inst/tests/tests.Rraw} to see the next available ID) and then index the tests within your PR by increasing \code{m}. Note -- \code{n.m} is interpreted as a number, so \code{123.4} and \code{123.40} are actually the same -- please \code{0}-pad as appropriate. Test identifiers should be provided in increasing order. }
\item{x}{ An input object to be evaluated. }
\item{y}{ Pre-defined output against which you are testing \code{x}, by default \code{TRUE}. }
\item{error}{ When you are testing behaviour of code that you expect to fail with an error, supply the expected error message to this argument. It is interpreted as a regular expression, so you can be abbreviated, but try to include the key portion of the error so as not to accidentally include a different error message. }
\item{warning}{ Same as \code{error}, in the case that you expect your code to issue a warning. Note that since the code evaluates successfully, you should still supply \code{y}. }
\item{message}{ Same as \code{warning} but expects \code{message} exception. }
\item{output}{ If you are testing the printing/console output behaviour of some feature. Again, regex-compatible. }
\item{notOutput}{ Or if you are testing that a feature does not print particular console output. }
}
\details{
   The atomic testing function used in \code{data.table} test scripts that are run by \code{\link{test.data.table}}.
}
\value{
Logical \code{TRUE} when test passes, \code{FALSE} when test fails. Invisibly.
}
\seealso{ \code{\link{test.data.table}} }
\examples{
test = data.table:::test
test(1, x = sum(1:5), y = 15L)
test(2, log(-1), NaN, warning="NaNs")
test(3, sum("a"), error="invalid.*character")
# test failure example
stopifnot(
  test(4, TRUE, FALSE) == FALSE
)
}
\keyword{ data }
