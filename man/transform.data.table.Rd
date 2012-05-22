\name{transform.data.table}
\alias{transform}
\alias{transform.data.table}
\alias{within}
\alias{within.data.table}
\title{ Data table utilities }
\description{
  Utilities for data.table transformation.
}
\usage{
\method{transform}{data.table}(`_data`, \ldots)
\method{within}{data.table}(data, expr, \ldots)
}
\arguments{
  \item{data, _data}{ data.table to be transformed.}

  \item{\ldots}{ for \code{transform}, Further arguments of the form
    \code{tag=value}. Ignored for \code{within}.}

  \item{expr}{ expression to be evaluated within the data.table.}
}
\details{
\code{within} is like \code{with}, but modifications (columns changed,
added, or removed) are updated in the returned data.table.

Note that \code{transform} will keep the key of the
\code{data.table} provided the ``targets'' of the transform (i.e. the
columns that appear in \ldots) are not in the key of the data.table. 
\code{within} also retains the key provided the key columns are not ``touched''.
}
\value{
   The modified value of \code{data}.
}
\seealso{ \code{\link{transform}} and \code{\link{within}} }
\examples{
dt <- data.table(a=rep(1:5, 1), b=1:10)

transform(dt, c = a^2)

#within(dt, {
#  b <- rev(b)
#  c <- a^2
#  rm(a)
#})

# dt[, transform, c = max(b), by="a"]  # like "ave"

}
\keyword{ data }
