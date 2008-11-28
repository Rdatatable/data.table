\name{between}
\alias{between}
\alias{\%between\%}
\title{ Convenience function for range subset logic. }
\description{
  Intended for use in data.table i clause 'where'.
}
\usage{
between(x,lower,upper,incbounds=TRUE)
x %between% c(lower,upper)
}
\arguments{
   \item{x}{ Any vector e.g. numeric, character, date, ... }
   \item{lower}{ Lower range bound. }
   \item{upper}{ Upper range bound. }
   \item{incbounds}{ TRUE means inclusive bounds i.e. [lower,upper]. FALSE means exclusive bounds i.e. (lower,upper). }
}
% \details{
% }
\value{
    Integer vector containing the locations of x which lie within the range [lower,upper] or (lower,upper).
}
% \references{ }
\author{ Matthew Dowle }
\note{ Current implementation does not make use of sorted keys. }
\seealso{ \code{\link{data.table}}, \code{\link{like}} }
\examples{
    DT = data.table(a=1:5, b=6:10)
    DT[b \%between\% c(7,9)]   # select from DT where b between 7 and 9
}
\keyword{ data }
