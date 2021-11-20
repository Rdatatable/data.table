\name{test}
\alias{test}
\title{ Test assertions for equality, exceptions and console output }
\description{
  An internal testing function used in \code{data.table} test scripts that are run by \code{\link{test.data.table}}.
}
\usage{
test(num, x, y = TRUE,
     error = NULL, warning = NULL, message = NULL,
     output = NULL, notOutput = NULL, ignore.warning = NULL)
}
\arguments{
\item{num}{ A unique identifier for a test, helpful in identifying the source of failure when testing is not working. Currently, we use a manually-incremented system with tests formatted as \code{n.m}, where essentially \code{n} indexes an issue and \code{m} indexes aspects of that issue. For the most part, your new PR should only have one value of \code{n} (scroll to the end of \code{inst/tests/tests.Rraw} to see the next available ID) and then index the tests within your PR by increasing \code{m}. Note -- \code{n.m} is interpreted as a number, so \code{123.4} and \code{123.40} are actually the same -- please \code{0}-pad as appropriate. Test identifiers are checked to be in increasing order at runtime to prevent duplicates being possible. }
\item{x}{ An input expression to be evaluated. }
\item{y}{ Pre-defined value to compare to \code{x}, by default \code{TRUE}. }
\item{error}{ When you are testing behaviour of code that you expect to fail with an error, supply the expected error message to this argument. It is interpreted as a regular expression, so you can be abbreviated, but try to include the key portion of the error so as not to accidentally include a different error message. }
\item{warning}{ Same as \code{error}, in the case that you expect your code to issue a warning. Note that since the code evaluates successfully, you should still supply \code{y}. }
\item{message}{ Same as \code{warning} but expects \code{message} exception. }
\item{output}{ If you are testing the printing/console output behaviour; e.g. with \code{verbose=TRUE} or \code{options(datatable.verbose=TRUE)}. Again, regex-compatible and case sensitive. }
\item{notOutput}{ Or if you are testing that a feature does \emph{not} print particular console output. Case insensitive (unlike output) so that the test does not incorrectly pass just because the string is not found due to case. }
\item{ignore.warning}{ A single character string. Any warnings emitted by \code{x} that contain this string are dropped. Remaining warnings are compared to the expected \code{warning} as normal. }
}
\note{
   \code{NA_real_} and \code{NaN} are treated as equal, use \code{identical} if distinction is needed. See examples below.

   If \code{warning=} is not supplied then you are automatically asserting no warning is expected; the test will fail if any warning does occur. Similarly for \code{message=}.

   Multiple warnings are supported; supply a vector of strings to \code{warning=}. If \code{x} does not produce the correct number of warnings in the correct order, the test will fail.

   Strings passed to \code{notOutput=} should be minimal; e.g. pick out single words from the output that you desire to check does not occur. The reason being so that the test does not incorrectly pass just because the output has slightly changed. For example \code{notOutput="revised"} is better than \code{notOutput="revised flag to true"}. \code{notOutput=} is automatically case insensitive for this reason.
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
# NA_real_ vs NaN
test(5.01, NA_real_, NaN)
test(5.03, all.equal(NaN, NA_real_))
test(5.02, identical(NaN, NA_real_), FALSE)
}
\keyword{ data }
