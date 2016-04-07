\name{between}
\alias{between}
\alias{\%between\%}
\alias{anywhere}
\alias{\%anywhere\%}
\title{ Convenience functions for range subsets. }
\description{
  Intended for use in \code{i} in \code{[.data.table}.

  \code{between} answers the question: Is \code{x[i]} in between \code{lower[i]} and \code{upper[i]}. \code{lower} and \code{upper} are recycled if they are not identical to \code{length(x)}. This is equivalent to \code{x >= lower & x <= upper}, when \code{incbounds=TRUE} and \code{x > lower & y < upper} when \code{FALSE}.

  \code{anywhere} on the other hand answers the question: Is \code{x[i]} is in between \emph{any of the intervals}specified by \code{lower, upper}. There is no need for recycling here. A \code{non-equi} join is performed internally in this case to determine if \code{x[i]} is in between \emph{any} of the intervals in \code{lower, upper}.
}
\usage{
between(x,lower,upper,incbounds=TRUE)
x \%between\% y
anywhere(x,lower,upper,incbounds=TRUE)
x \%anywhere\% y
}
\arguments{
   \item{x}{ Any orderable vector, i.e., those with relevant methods for \code{`<=`}, such as \code{numeric}, \code{character}, \code{Date}, ... }
   \item{lower}{ Lower range bound. Must be of same length as \code{upper}. Recycled to \code{length(x)} in case of \code{between}.}
   \item{upper}{ Upper range bound. Must be of same length as \code{lower}. Recycled to \code{length(x)} in case of \code{between}.}
   \item{y}{ A length-2 \code{vector} or \code{list}, with \code{y[[1]]} interpreted as \code{lower} and \code{y[[2]]} as \code{upper}.} 
   \item{incbounds}{ \code{TRUE} means inclusive bounds, i.e., [lower,upper]. \code{FALSE} means exclusive bounds, i.e., (lower,upper). }
}
\details{
    When \code{lower} and \code{upper} are length-1 vectors, \code{between} and \code{anywhere} are the same. In that case, \code{anywhere} is likely to be faster since it uses \emph{binary search} based \code{non-equi} join instead of \code{vector scan} as in the case of \code{between}.
}
\value{
    Logical vector as the same length as \code{x} with value \code{TRUE} for those that lie within the specified range.
}
\note{ Current implementation does not make use of ordered keys. \code{incbounds} is set to \code{TRUE} for the infix notation \code{\%between\%}. }
\seealso{ \code{\link{data.table}}, \code{\link{like}} }
\examples{
X = data.table(a=1:5, b=6:10, c=c(5:1))
X[b \%between\% c(7,9)]
X[between(b, 7, 9)] # same as above
# NEW feature in v1.9.8, vectorised between
X[c \%between\% list(a,b)]
X[between(c, a, b)] # same as above
X[between(c, a, b, incbounds=FALSE)] # open interval

# anywhere()
Y = data.table(a=c(8,3,10,7,-10), val=runif(5))
range = data.table(start = 1:5, end = 6:10)
Y[a \%anywhere\% range]
Y[anywhere(a, range$start, range$end)] # same as above
Y[anywhere(a, range$start, range$end, incbounds=FALSE)] # open interval
}
\keyword{ data }
