\name{between}
\alias{between}
\alias{\%between\%}
\alias{inrange}
\alias{\%inrange\%}
\title{ Convenience functions for range subsets. }
\description{
Intended for use in \code{i} in \code{[.data.table}.

\code{between} is equivalent to \code{x >= lower & x <= upper} when
\code{incbounds=TRUE}, or \code{x > lower & y < upper} when \code{FALSE}. With a caveat that
\code{NA} in \code{lower} or \code{upper} are taken as a missing bound and return \code{TRUE} not \code{NA}.

\code{inrange} checks whether each value in \code{x} is in between any of
the intervals provided in \code{lower,upper}.
}
\usage{
between(x, lower, upper, incbounds=TRUE)
x \%between\% y

inrange(x, lower, upper, incbounds=TRUE)
x \%inrange\% y
}
\arguments{
\item{x}{ Any orderable vector, i.e., those with relevant methods for
\code{`<=`}, such as \code{numeric}, \code{character}, \code{Date}, etc. in
case of \code{between} and a numeric vector in case of \code{inrange}.}
\item{lower}{ Lower range bound. Either length 1 or same length as \code{x}.}
\item{upper}{ Upper range bound. Either length 1 or same length as \code{x}.}
\item{y}{ A length-2 \code{vector} or \code{list}, with \code{y[[1]]}
interpreted as \code{lower} and \code{y[[2]]} as \code{upper}.}
\item{incbounds}{ \code{TRUE} means inclusive bounds, i.e., [lower,upper].
\code{FALSE} means exclusive bounds, i.e., (lower,upper).
It is set to \code{TRUE} by default for infix notations.}
}
\details{

\emph{non-equi} joins were implemented in \code{v1.9.8}. They extend
binary search based joins in \code{data.table} to other binary operators
including \code{>=, <=, >, <}. \code{inrange} makes use of this new
functionality and performs a range join.

}
\value{
Logical vector the same length as \code{x} with value \code{TRUE} for those
that lie within the specified range.
}
\note{ Current implementation does not make use of ordered keys for
\code{\%between\%}. }
\seealso{
\code{\link{data.table}}, \code{\link{like}}, \code{\link{\%chin\%}}
}
\examples{
X = data.table(a=1:5, b=6:10, c=c(5:1))
X[b \%between\% c(7,9)]
X[between(b, 7, 9)] # same as above
# NEW feature in v1.9.8, vectorised between
X[c \%between\% list(a,b)]
X[between(c, a, b)] # same as above
X[between(c, a, b, incbounds=FALSE)] # open interval

# inrange()
Y = data.table(a=c(8,3,10,7,-10), val=runif(5))
range = data.table(start = 1:5, end = 6:10)
Y[a \%inrange\% range]
Y[inrange(a, range$start, range$end)] # same as above
Y[inrange(a, range$start, range$end, incbounds=FALSE)] # open interval
}
\keyword{ data }
