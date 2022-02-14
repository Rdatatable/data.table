\name{last}
\alias{first}
\alias{last}
\title{ First/last item of an object }
\description{
Returns the first/last item of a vector, list, \code{data.frame} or \code{data.table}. The main difference
to head/tail is that the default for \code{n} is 1 rather than 6.
}
\usage{
first(x, n=1L, na.rm=FALSE, \dots)
last(x, n=1L, na.rm=FALSE, \dots)
}
\arguments{
\item{x}{ A vector, list, \code{data.frame} or \code{data.table}. \code{xts} objects dispatch to the \code{xts} method. }
\item{n}{ A numeric vector length 1. How many items to select. }
\item{na.rm}{ \code{FALSE}, \code{TRUE} or \code{"row"}. Should missing values be skipped when selecting values? Selects always at least 1 element. }
\item{\dots}{ Not applicable for \code{data.table} first/last. Any arguments here
are passed through to \code{xts}'s first/last. }
}
\value{
The first/last \code{n} items of a vector or list.
When \code{na.rm=TRUE} the first/last item in each column of a \code{data.frame} or \code{data.table}.
When \code{na.rm="row"} the first/last \code{n} rows which contain no NA in any column; i.e. if an NA is found that row is removed.
}
\seealso{ \code{\link{NROW}}, \code{\link{head}}, \code{\link{tail}} }
\examples{
first(1:5)
x = data.table(x=1:5, y=6:10)
first(x) # same as head(x, 1)

last(1:5) # [1] 5
x = data.table(x=1:5, y=6:10)
last(x) # same as tail(x, 1)

x = c(NA, 1, 2, NA)
first(x)
first(x, na.rm=TRUE)
last(x)
last(x, na.rm=TRUE)

x = data.table(x=c(NA, 1, 2, NA), y=1:4)
first(x)
first(x, na.rm=TRUE)
first(x, na.rm="row")
last(x)
last(x, na.rm=TRUE)
last(x, na.rm="row")
}
\keyword{ data }
