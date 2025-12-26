\name{frolladapt}
\alias{frolladapt}
\title{Adapt rolling window to irregularly spaced time series}
\description{
  Helper function to generate adaptive window size based on the irregularly spaced time series index, to be passed as \code{n} argument to adaptive \code{\link{froll}} function (or \code{N} argument to adaptive \code{\link{frollapply}}). Experimental.
}
\usage{
  frolladapt(x, n, align="right", partial=FALSE, give.names=FALSE)
}
\arguments{
  \item{x}{ Integer. Must be sorted with no duplicates or missing values. Other objects with numeric storage (including most commonly \code{Date} and \code{POSIXct}) will be coerced to integer, which, for example, in case of \code{POSIXct} means truncating to whole seconds. It does not support vectorized input. }
  \item{n}{ Integer, positive, rolling window size. Up to \code{n} values nearest to each value of \code{x}, with distance in the units of \code{x} and according to the window implied by \code{align}, are included in each rolling aggregation window. Thus when \code{x} is a \code{POSIXct}, \code{n} are seconds, and when \code{x} is a \code{Date}, \code{n} are days. It supports vectorized input, then it needs to be a vector. }
  \item{align}{ Character, default \code{"right"}. Other alignments have not yet been implemented. }
  \item{partial}{ Logical, default \code{FALSE}. Should the rolling window size(s) provided in \code{n} be trimmed to available observations? For details see \code{\link{froll}}. }
  \item{give.names}{ Logical, default \code{FALSE}. When \code{TRUE}, names are automatically generated corresponding to names of \code{n}. If answer is an integer vector, then the argument is ignored, see examples. }
}
\details{
  Argument \code{n} allows multiple values to generate multiple adaptive windows, unlike \code{x}, as mixing different time series would make no sense.
}
\value{
  When \code{length(n)==1L} then integer vector (\emph{adaptive window} size) of length of \code{x}. Otherwise a list of \code{length(n)} having integer vectors (\emph{adaptive window} sizes) of length of \code{x} for each window size provided in \code{n}.
}
\examples{
idx = as.Date("2022-10-23") + c(0,1,4,5,6,7,9,10,14)
dt = data.table(index=idx, value=seq_along(idx))
dt
dt[, n3 := frolladapt(index, n=3L)]
dt
dt[, rollmean3 := frollmean(value, n3, adaptive=TRUE)]
dt
dt[, n3p := frolladapt(index, n=3L, partial=TRUE)]
dt[, rollmean3p := frollmean(value, n3p, adaptive=TRUE)]
dt

n34 = frolladapt(idx, c(small=3, big=4), give.names=TRUE)
n34
dt[, frollmean(value, n34, adaptive=TRUE, give.names=TRUE)]
}
\seealso{
  \code{\link{froll}}, \code{\link{frollapply}}
}
\keyword{ data }
