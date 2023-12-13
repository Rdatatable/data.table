\name{fctr}
\alias{fctr}
\alias{factor}
\title{Create a factor retaining original order}
\description{
  Calls code{\link[base:factor]{factor}} using default arguments adjusted to retain original order.
}
\usage{
fctr(x, levels=unique(x), ..., rev=FALSE)
}
\arguments{
  \item{x}{ Object to be turned into factor. }
  \item{levels}{ Levels for new factor, by default, unlike base R, \code{unique(x)}. }
  \item{\dots}{ Other arguments passed to code{\link[base:factor]{factor}}. }
  \item{rev}{ Logical, decides if order of \code{levels} should be reversed. }
}
\value{
  Factor vector having levels ordered according to the order of elements in input.
}
\examples{
levels(factor(c("b","a","c")))
levels(fctr(c("b","a","c")))
levels(fctr(c("b","a","c"), rev=TRUE))
}
\keyword{ data }
