\name{psum}
\alias{psum}
\alias{pprod}
\alias{pany}
\alias{pall}
\title{ Rowwise summary functions }
\description{
These functions extend the logic of \code{\link[base]{pmin}} and \code{\link[base]{pmax}} to work for \code{sum}, \code{prod}, \code{any}, and \code{all} as well.
}
\usage{
  psum(\dots, na.rm=FALSE)
  pprod(\dots, na.rm=FALSE)
  pany(\dots, na.rm=FALSE)
  pall(\dots, na.rm=FALSE)
}
\arguments{
  \item{\dots}{ A sequence of logical, integer, numeric, or complex vectors; only singletons will be recycled. }
  \item{na.rm}{ Should missing (\code{NA}) values be excluded from the calculation? See Details. }
}
\details{
Mixed will be coerced to the appropriate storage type for output, e.g. mixed integer and numeric will result in numeric output.

Integer overflow is converted to \code{NA} with warning when \code{na.rm=FALSE}; when \code{na.rm=TRUE}, execution stops.

To match the behavior of \code{NA + NA} and that of \code{pmin(NA, na.rm=TRUE)}, when all inputs are \code{NA} and \code{na.rm=TRUE}, the output will be \code{NA} -- \emph{cf.} \code{min(NA, na.rm=TRUE)}.
}
\value{
A vector of the same length as the input vectors.
}
\seealso{
  \code{\link[base]{pmin}}, \code{\link{frollmean}}, \code{\link{GForce}}
}
\examples{
psum(1:10, 11:20)
psum(10:1, pi)
pprod(c(NA, 2:10), c(11:19, NA), na.rm=TRUE)
psum(NA, c(NA, 2), na.rm=TRUE)

pany(1:10 > 5, (5:14)^2 < 150)
pall(1:10 <= 5, sin(pi/2*0:9) > 0)
}
\keyword{ data }
