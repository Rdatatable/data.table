\name{roll}
\alias{roll}
\alias{rolling}
\alias{sliding}
\alias{moving}
\alias{rollmean}
\title{Rolling functions}
\description{
  \code{rollmean} in dev so far.
}

\usage{
rollmean(x, n, fill=NA, align=c("right", "center", "left"), adaptive=FALSE)
}
\arguments{
  \item{x}{ A vector, list, data.frame or data.table. }
  \item{n}{ Non-negative integer vector. }
  \item{fill}{ Value to pad by. }
  \item{align}{ Align character, default \code{"right"}. }
  \item{adaptive}{ logical, should adaptive rolling function be
    calculated, default \code{FALSE}. }
}
\details{
  \code{roll*} functions accepts vectors, lists, data.frames or data.tables. It always returns a list except when the input is a \code{vector} and \code{length(n) == 1} in which case a \code{vector} is returned, for convenience. This is so that it can be used conveniently within data.table's syntax.

  Argument \code{n} allows multiple values.
}
\value{
  A list or a vector.
}

\examples{
rollmean(as.double(1:16), c(3, 4))
}
\seealso{
  \code{\link{shift}}, \code{\link{data.table}}
}
\keyword{ data }
