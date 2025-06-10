\name{last}
\alias{first}
\alias{last}
\title{ First/last item of an object }
\description{
Returns the first/last item of a vector or list, or the first/last row of a data.frame
or data.table. The main difference to head/tail is that the default for \code{n} is 1
rather than 6.
}
\usage{
first(x, n=1L, \dots)
last(x, n=1L, \dots)
}
\arguments{
\item{x}{ A vector, list, data.frame or data.table. Otherwise the S3 method
of \code{xts::first} is deployed. }
\item{n}{ A numeric vector length 1. How many items to select. }
\item{\dots}{ Not applicable for \code{data.table} first/last. Any arguments here
are passed through to \code{xts}'s first/last. }
}
\details{
Note: \code{first(x)} and \code{last(x)} are designed to behave like \code{head(x, 1)} and \code{tail(x, 1)}, respectively. Consequently, they differ from base R subsetting (\code{x[1]}): they return a zero-length vector for empty inputs (instead of \code{NA}) and do not preserve attributes like names.
}
\value{
If no other arguments are supplied it depends on the type of \code{x}. The first/last item
of a vector or list. The first/last row of a \code{data.frame} or \code{data.table}.
For other types, or if any argument is supplied in addition to \code{x} (such as \code{n}, or
\code{keep} in \code{xts}) regardless of \code{x}'s type, then \code{xts::first}/
\code{xts::last} is called if \code{xts} has been loaded, otherwise \code{utils::head}/\code{utils::tail}.
}
\seealso{ \code{\link{NROW}}, \code{\link{head}}, \code{\link{tail}} }
\examples{
first(1:5) # [1] 1
x = data.table(x=1:5, y=6:10)
first(x) # same as head(x, 1)

last(1:5) # [1] 5
x = data.table(x=1:5, y=6:10)
last(x) # same as tail(x, 1)
}
\keyword{ data }
