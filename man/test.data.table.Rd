\name{test.data.table}
\alias{test.data.table}
\title{ Runs a set of tests. }
\description{
  Runs a set of tests to check data.table is working correctly.
}
\usage{
test.data.table(script = "tests.Rraw", verbose = FALSE, pkg = ".",
                silent = FALSE,
                showProgress = interactive() && !silent)
}
\arguments{
\item{script}{ Run arbitrary R test script. }
\item{verbose}{ \code{TRUE} sets \code{options(datatable.verbose=TRUE)} for the duration of the tests. This tests there are no errors in the branches that produce the verbose output, and produces a lot of output. The output is normally used for tracing bugs or performance tuning. Tests which specifically test the verbose output is correct (typically looking for an expected substring) always run regardless of this option. }
\item{pkg}{ Root directory name under which all package content (ex: DESCRIPTION, src/, R/, inst/ etc..) resides. Used only in \emph{dev-mode}. }
\item{silent}{ Controls what happens if a test fails. Like \code{silent} in \code{\link{try}}, \code{TRUE} causes the error message to be suppressed and \code{FALSE} to be returned, otherwise the error is returned. }
\item{showProgress}{ Output 'Running test <n> ...\\r' at the start of each test? }
}
\details{
  Runs a series of tests. These can be used to see features and examples of usage, too. Running test.data.table will tell you the full location of the test file(s) to open.

  Setting \code{silent=TRUE} sets \code{showProgress=FALSE} too, via the default of \code{showProgress}.
}
\value{
  If all tests were successful, \code{TRUE} is returned. Otherwise, see the \code{silent} argument above. \code{silent=TRUE} is intended for use at the start of production scripts; e.g. \code{stopifnot(test.data.table(silent=TRUE))} to check \code{data.table} is passing its own tests before proceeding.
}
\seealso{ \code{\link{data.table}}, \code{\link{test}} }
\keyword{ data }
