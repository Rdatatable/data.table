\name{frolladapt}
\alias{frolladapt}
\title{Adapt rolling window to irregularly spaced time series}
\description{
  Helper function to generate adaptive window size based on the irregularly spaced time series index. Generated adaptive window can be then used in rolling functions. See \code{\link{froll}} and \code{\link{frollapply}} for details.
}
\usage{
  frolladapt(x, n, partial=FALSE, give.names=FALSE, .validate=FALSE)
}
\arguments{
  \item{x}{ Integer. Other objects of type numeric (including \code{Date}, \code{POSIXct} and any others numeric-based class) will be coerced to integer, which, for example, in case of \code{POSIXct} means truncating to whole seconds. Must be sorted, have no duplicate and have no missing values. }
  \item{n}{ Integer vector giving rolling window size(s). This is the \emph{total} number of included values in aggregate function. Value corresponds to unit of \code{x}. When \code{x} is a \code{POSIXct} then \code{n} are seconds, when \code{x} is a \code{Date} then \code{n} are days. }
  \item{partial}{ Logical, default \code{FALSE}. Should the rolling window size(s) provided in \code{N} be trimmed to available observations. For details see \code{\link{froll}}. }
  \item{give.names}{ Logical, default \code{FALSE}. When \code{TRUE}, names are automatically generated corresponding to names of \code{N}. If answer is an integer vector, then the argument is ignored, see examples. }
  \item{.validate}{ Logical, default \code{FALSE}. Makes extra validation vs reference implementation. Will be removed before merging to master branch. }
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
