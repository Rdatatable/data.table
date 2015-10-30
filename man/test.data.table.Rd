\name{test.data.table}
\alias{test.data.table}
\title{ Runs a set of tests. }
\description{
  Runs a set of tests to check data.table is working correctly.
}
\usage{
test.data.table(verbose=FALSE, pkg="pkg", silent=FALSE)
}
\arguments{
\item{verbose}{ If TRUE sets datatable.verbose to TRUE for the duration of the tests. }
\item{pkg}{Root directory name under which all package content (ex: DESCRIPTION, src/, R/, inst/ etc..) resides.}
\item{silent}{Logical, default FASLE, when TRUE it will not raise error on in case of test fails.}
}
\details{
   Runs a series of tests. These can be used to see features and examples of usage, too. Running test.data.table will tell you the full location of the test file(s) to open.
}
\value{
When \code{silent} equals to TRUE it will return TRUE if all tests were successful. FALSE otherwise. If \code{silent} equals to FALSE it will return TRUE if all tests were successful. Error otherwise.
}
\seealso{ \code{\link{data.table}} }
\examples{
\dontrun{
test.data.table()
}}
\keyword{ data }

