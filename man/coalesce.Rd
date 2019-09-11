\name{fcoalesce}
\alias{fcoalesce}
\alias{setcoalesce}
\title{ Coalescing missing values }
\description{
Fill in missing values in a vector by successively pulling from candidate vectors in order. As per the ANSI SQL function COALESCE, \code{dplyr::coalesce} and \code{hutils::coalesce}. Unlike \code{BBmisc::coalesce} which just returns the first non-NULL vector.
Written in C, and multithreaded for numeric and factor types.
}
\usage{
  fcoalesce(\dots)
}
\arguments{
  \item{\dots}{ A set of same-class vectors. These vectors can be supplied as separate arguments or as a single plain list, data.table or data.frame, see examples. }
}
\details{
Factor type is supported only when the factor levels of each item are equal.

\code{NaN} is considered missing (note \code{is.na(NaN)} and \code{all.equal(NA_real_, NaN)} are both \code{TRUE}).
}
\value{
Atomic vector of the same type and length as the first vector, having \code{NA} values replaced by corresponding non-\code{NA} values from the other vectors.
If the first item is \code{NULL}, the result is \code{NULL}.
}
\seealso{
  \code{\link{fifelse}}
}
\examples{
x = c(11L, NA, 13L, NA, 15L, NA)
y = c(NA, 12L, 5L, NA, NA, NA)
z = c(11L, NA, 1L, 14L, NA, NA)
fcoalesce(x, y, z)
fcoalesce(list(x,y,z))   # same
fcoalesce(x, list(y,z))  # same
}
\keyword{ data }

