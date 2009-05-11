\name{duplicated}
\alias{duplicated.data.table}
\alias{unique.data.table}
\title{ Determine Duplicate Rows }
\description{
     \code{duplicated} determines which rows of a data table have duplicate keys
     of rows with smaller subscripts, and returns a logical vector
     indicating which rows are duplicates by key.

     \code{unique} returns a data table with duplicated rows (by key) removed.
}
\usage{
\method{duplicated}{data.table}(x, ...)

\method{unique}{data.table}(x, ...)
}
\arguments{
  \item{x}{ A data.table. }
  \item{\dots}{ Not used at this time. }
}
\details{
  Because data.tables are sorted by key, tests for duplication are
  relatively quick.

  Note that only keys are checked for duplication. Unkeyed columns are
  not checked.
}
\value{
     \code{duplicated} returns a logical vector of length \code{nrow(x)}
  indicating which rows are duplicates by key.

     \code{unique} returns a data table with duplicated rows (by key) removed.
}
% \references{ }
\author{ Tom Short }
% \note{ }
\seealso{ \code{\link{data.table}}, \code{\link{duplicated}}, \code{\link{unique}}}
\examples{
    (dt <- data.table(A = rep(1:3, each=4), B = rep(1:4, each=3), C = rep(1:2, 6), key = "A,B"))
    duplicated(dt)
    unique(dt)
}
\keyword{ data }
