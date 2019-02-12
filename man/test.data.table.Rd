\name{test.data.table}
\alias{test.data.table}
\title{ Runs a set of tests. }
\description{
  Runs a set of tests to check data.table is working correctly.
}
\usage{
test.data.table(verbose=FALSE, pkg="pkg", silent=FALSE,
                with.other.packages=FALSE, benchmark=FALSE,
                script="tests.Rraw")
}
\arguments{
\item{verbose}{ If \code{TRUE} sets datatable.verbose to \code{TRUE} for the duration of the tests. }
\item{pkg}{Root directory name under which all package content (ex: DESCRIPTION, src/, R/, inst/ etc..) resides.}
\item{silent}{Logical, default \code{FALSE}, when \code{TRUE} it will not raise error on in case of test fails.}
\item{with.other.packages}{ Deprecated. Run compatibility tests with other packages. Use \code{script='other.Rraw'} instead. }
\item{benchmark}{ Deprecated. Run the benchmark script. Use \code{script='benchmark.Rraw'} instead. }
\item{script}{ Run arbitrary R test script. }
}
\details{
   Runs a series of tests. These can be used to see features and examples of usage, too. Running test.data.table will tell you the full location of the test file(s) to open.
}
\value{
When \code{silent} equals to \code{TRUE} it will return \code{TRUE} if all tests were successful. \code{FALSE} otherwise. If \code{silent} equals to \code{FALSE} it will return \code{TRUE} if all tests were successful. Error otherwise.
}
\seealso{ \code{\link{data.table}} }
\examples{
\dontrun{
  library(data.table)
  test.data.table()
}
}
\keyword{ data }

