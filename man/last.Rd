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
first(x, n=1L, na.rm=FALSE, \dots)
last(x, n=1L, na.rm=FALSE, \dots)
}
\arguments{
\item{x}{ A vector, list, data.frame or data.table. Otherwise the S3 method
of \code{xts::first} is deployed. }
\item{n}{ A numeric vector length 1. How many items to select. }
\item{na.rm}{ \code{TRUE} or \code{FALSE} (default). When \code{TRUE}, missing values in \code{x}
(\code{NA}, or \code{NULL} list elements) are skipped, so that the first/last \code{n}
\emph{non-missing} items are returned; fewer than \code{n} are returned if fewer than \code{n}
non-missing items are found. Only supported when \code{x} is a plain vector or list (not
\code{data.frame}, \code{data.table}, matrix/array, or \code{xts}). }
\item{\dots}{ Not applicable for \code{data.table} first/last. Any arguments here
are passed through to \code{xts}'s first/last. }
}
\note{
For zero-length vectors, \code{first(x)} and \code{last(x)} mimic \code{head(x, 1)} and \code{tail(x, 1)} by returning an empty vector instead of \code{NA}. However, unlike \code{head()}/\code{tail()} and base R subsetting (e.g., \code{x[1]}), they do not preserve attributes like names.
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

first(c(NA, 2, NA, 4), na.rm=TRUE) # [1] 2
last(c(1, NA, 3, NA), n=2, na.rm=TRUE) # [1] 1 3

DT = data.table(g=c(1,1,1,2,2), x=c(1,NA,3,NA,5))
DT[, first(x, na.rm=TRUE), by=g] # first non-missing x per group
DT[, first(x, n=2, na.rm=TRUE), by=g] # up to 2 non-missing x per group; fewer rows if fewer are non-missing
}
\keyword{ data }
