\name{coalesce}
\alias{coalesce}
\alias{setcoalesce}
\title{ Coalescing missing values }
\description{
Fill in missing values in a vector by successively pulling from candidate vectors in order. As per the ANSI SQL function COALESCE, \code{dplyr::coalesce} and \code{hutils::coalesce}.
Written in C, and multithreaded for numeric and factor types.
}
\usage{
  coalesce(\dots)
}
\arguments{
  \item{\dots}{ A set of same-class vectors. These vectors can be supplied as separate arguments or as a single plain list, data.table or data.frame, see examples. }
}
\details{
Factor type is supported only when the factor levels of each item are equal.
}
\value{
Atomic vector of the same type and length as the first vector, having \code{NA} values replaced by corresponding non-\code{NA} values from the other vectors.
}
\seealso{
  \code{\link{fifelse}}
}
\examples{
x = c(11L, NA, 13L, NA, 15L, NA)
y = c(NA, 12L, 5L, NA, NA, NA)
z = c(11L, NA, 1L, 14L, NA, NA)
coalesce(x, y, z)
coalesce(list(x,y,z))   # same
coalesce(x, list(y,z))  # same
}
\keyword{ data }

