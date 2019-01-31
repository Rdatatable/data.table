\name{fna.fill}
\alias{fill}
\alias{locf}
\alias{nocb}
\alias{na.fill}
\alias{fna.fill}
\title{Fill missing values}
\description{
  Fast fill missing values using constant value, \emph{last observation carried forward} or \emph{next observation carried backward}.
}
\usage{
fna.fill(x, type=c("const","locf","nocb"), fill=NA)
}
\arguments{
  \item{x}{ vector, list, data.frame or data.table of numeric columns. }
  \item{type}{ character, one of \emph{"const"}, \emph{"locf"} or \emph{"nocb"}. Defaults to \code{"const"}. }
  \item{fill}{ numeric or integer, value to be used to fill when \code{type=="const"}. }
}
\details{
  Only \emph{double} and \emph{integer} data types are currently supported.
}
\value{
  A list except when the input is a \code{vector} in which case a \code{vector} is returned.
}
\examples{
x = 1:10
x[c(1:2, 5:6, 9:10)] = NA
fna.fill(x, "locf")
dt = data.table(v1=x, v2=shift(x))
fna.fill(dt, "nocb")
}
\seealso{
  \code{\link{shift}}, \code{\link{data.table}}
}
\keyword{ data }
