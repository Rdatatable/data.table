\name{like}
\alias{like}
\alias{\%like\%}
\title{ Convenience function for calling regexpr. }
\description{
  Intended for use in \code{i} in \code{[.data.table}.
}
\usage{
like(vector,pattern)
vector \%like\% pattern
}
\arguments{
   \item{vector}{ Either a \code{character} vector or a \code{factor}. A \code{factor} is faster. }
   \item{pattern}{ Passed on to \code{\link{grepl}}. }
}
% \details{
% }
\value{
    Logical vector, \code{TRUE} for items that match \code{pattern}.
}
\note{ Current implementation does not make use of sorted keys. }
\seealso{ \code{\link{data.table}}, \code{\link{grepl}} }
\examples{
DT = data.table(Name=c("Mary","George","Martha"), Salary=c(2,3,4))
DT[Name \%like\% "^Mar"]
}
\keyword{ data }
