\name{fctr}
\alias{fctr}
\alias{factor}
\title{Create a factor retaining original ordering}
\description{
  Creates a \code{\link[base]{factor}}.

  By default, the output will have its levels in the original order, i.e., \code{levels = unique(x)}, as opposed to \code{factor}'s default where \code{levels = sort(unique(x))}.
}
\usage{
fctr(x, levels=unique(x), ..., sort=FALSE, rev=FALSE)
}
\arguments{
  \item{x}{ Object to be turned into a factor. }
  \item{levels}{ Levels for the new factor; \code{unique(x)} by default. }
  \item{\dots}{ Other arguments passed to \code{\link[base]{factor}}. }
  \item{sort}{ Logical, default \code{FALSE}. Should \code{levels} be sorted? }
  \item{rev}{ Logical, default \code{FALSE}. Should \code{levels} be reversed? Applied \emph{after} \code{sort}. }
}
\value{
  Factor vector having levels ordered according to the order of elements in input and arguments \code{sort}, \code{rev}.
}
\examples{
levels(factor(c("b","a","c")))
levels(fctr(c("b","a","c")))
levels(fctr(c("b","a","c"), rev=TRUE))
levels(fctr(c("b","a","c"), sort=TRUE))
levels(fctr(c("b","a","c"), sort=TRUE, rev=TRUE))
}
\keyword{ data }
