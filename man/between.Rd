\name{between}
\alias{between}
\alias{\%between\%}
\title{ Convenience function for range subset logic. }
\description{
  Intended for use in \code{i} in \code{[.data.table}. From \code{v1.9.8}, \code{between} is vectorised. 
}
\usage{
between(x,lower,upper,incbounds=TRUE)
x \%between\% y
}
\arguments{
   \item{x}{ Any orderable vector, i.e., those with relevant methods for \code{`<=`}, such as \code{numeric}, \code{character}, \code{Date}, ... }
   \item{lower}{ Lower range bound. Usually of length=\code{1} or \code{length(x)}.}
   \item{upper}{ Upper range bound. Usually of same length as \code{lower}.}
   \item{y}{ A length-2 \code{vector} or \code{list}, with \code{y[[1]]} interpreted as \code{lower} and \code{y[[2]]} as \code{upper}.} 
   \item{incbounds}{ \code{TRUE} means inclusive bounds, i.e., [lower,upper]. \code{FALSE} means exclusive bounds, i.e., (lower,upper). }
}
% \details{
% }
\value{
    Logical vector as the same length as \code{x} with value \code{TRUE} for those that lie within the specified range.
}
\note{ Current implementation does not make use of ordered keys. \code{incbounds} is set to \code{TRUE} for the infix notation \code{\%between\%}. }
\seealso{ \code{\link{data.table}}, \code{\link{like}} }
\examples{
DT = data.table(x=1:5, y=6:10, z=c(5:1))
DT[y \%between\% c(7,9)]
# NEW feature in v1.9.8, vectorised between
DT[z \%between\% list(x,y)]
}
\keyword{ data }
