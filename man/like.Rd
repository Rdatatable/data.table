\name{like}
\alias{like}
\alias{\%like\%}
\title{ Convenience function for calling regexpr. }
\description{
  Intended for use in data.table i clause 'where'.
}
\usage{
like(vector,pattern)
vector %like% pattern
}
\arguments{
   \item{vector}{ Either a character vector or a factor. A factor is faster (as you must have in a data.table). }
   \item{pattern}{ Passed on to \code{\link{regexpr}}. }
}
\details{
}
\value{
    Integer vector containing the locations of vector which match the pattern using regexpr.
}
\references{ }
\author{ Matthew Dowle }
\note{ Current implementation does not make use of sorted keys. }
\seealso{ \code{\link{data.table}}, \code{\link{regexpr}} }
\examples{
    DT = data.table(Name=c("Mary","George","Martha"), Salary=c(2,3,4))
    DT[Name \%like\% "^Mar"]   # select from DT where Name like 'Mar'
}
\keyword{ data }
