\name{first}
\alias{first}
\title{ First item of an object }
\description{
Returns the first item of a vector or list, or the first row of a data.frame
or data.table.
}
\usage{
first(x, \dots)
}
\arguments{
\item{x}{ A vector, list, data.frame or data.table. Otherwise the S3 method
of \code{xts::first} is deployed. }
\item{\dots}{ Not applicable for \code{data.table::first}. Any arguments here
are passed through to \code{xts::first}. }
}
\value{
If no other arguments are supplied it depends on the type of x. The first item
of a vector or list. The first row of a \code{data.frame} or \code{data.table}.
Otherwise, whatever \code{xts::first} returns (if package xts has been loaded,
otherwise a helpful error).

If any argument is supplied in addition to \code{x} (such as \code{n} or
\code{keep} in \code{xts::first}), regardless of \code{x}'s type, then
\code{xts::first} is called if xts has been loaded, otherwise a helpful error.
}
\seealso{ \code{\link{NROW}}, \code{\link{head}}, \code{\link{tail}},
\code{\link{last}} }
\examples{
first(1:5) # [1] 1
x = data.table(x=1:5, y=6:10)
first(x) # same as x[1]
}
\keyword{ data }
