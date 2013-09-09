\name{between}
\alias{between}
\alias{\%between\%}
\title{ Convenience function for range subset logic. }
\description{
  Intended for use in [.data.table i.
}
\usage{
between(x,lower,upper,incbounds=TRUE)
x %between% c(lower,upper)
}
\arguments{
   \item{x}{ Any vector e.g. numeric, character, date, ... }
   \item{lower}{ Lower range bound. }
   \item{upper}{ Upper range bound. }
   \item{incbounds}{ \code{TRUE} means inclusive bounds i.e. [lower,upper]. \code{FALSE} means exclusive bounds i.e. (lower,upper). }
}
% \details{
% }
\value{
    Logical vector as the same length as \code{x} with value \code{TRUE} for those that lie within the range [lower,upper] or (lower,upper).
}
\note{ Current implementation does not make use of ordered keys. }
\seealso{ \code{\link{data.table}}, \code{\link{like}} }
\examples{
    DT = data.table(a=1:5, b=6:10)
    DT[b \%between\% c(7,9)]
}
\keyword{ data }
