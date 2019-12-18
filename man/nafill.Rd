\name{nafill}
\alias{nafill}
\alias{fill}
\alias{setnafill}
\alias{locf}
\alias{nocb}
\alias{na.fill}
\title{Fill missing values}
\description{
  Fast fill missing values using constant value, \emph{last observation carried forward} or \emph{next observation carried backward}.
}
\usage{
nafill(x, type=c("const","locf","nocb"), fill=NA, nan=NA)
setnafill(x, type=c("const","locf","nocb"), fill=NA, nan=NA, cols=seq_along(x))
}
\arguments{
  \item{x}{ vector, list, data.frame or data.table of numeric columns. }
  \item{type}{ character, one of \emph{"const"}, \emph{"locf"} or \emph{"nocb"}. Defaults to \code{"const"}. }
  \item{fill}{ numeric or integer, value to be used to fill when \code{type=="const"}. }
  \item{nan}{ (numeric \code{x} only) Either \code{NaN} or \code{NA}; if the former, \code{NaN} is treated as distinct from \code{NA}, otherwise, they are treated the same during replacement? }
  \item{cols}{ numeric or character vector specifying columns to be updated. }
}
\details{
  Only \emph{double} and \emph{integer} data types are currently supported.

  Note that both \code{nafill} and \code{setnafill} provide some verbose output when \code{getOption('datatable.verbose')} is \code{TRUE}.
}
\value{
  A list except when the input is a \code{vector} in which case a \code{vector} is returned. For \code{setnafill} the input argument is returned, updated by reference.
}
\examples{
x = 1:10
x[c(1:2, 5:6, 9:10)] = NA
nafill(x, "locf")

dt = data.table(v1=x, v2=shift(x)/2, v3=shift(x, -1L)/2)
nafill(dt, "nocb")

setnafill(dt, "locf", cols=c("v2","v3"))
dt
}
\seealso{
  \code{\link{shift}}, \code{\link{data.table}}
}
\keyword{ data }
