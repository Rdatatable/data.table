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
\examples{
x = c(11L, NA, 13L, NA, 15L, NA)
y = c(NA, 12L, 5L, NA, NA, NA)
z = c(11L, NA, 1L, 14L, NA, NA)
coalesce(x, y, z)
coalesce(list(x,y,z))   # same
coalesce(x, list(y,z))  # same

\dontrun{
# default 4 threads on a laptop with 16GB RAM and 8 logical CPU
N = 100e6
x = replicate(5, {x=sample(N); x[sample(N, N/2)]=NA; x}, simplify=FALSE)  # 2GB
system.time(y1 <- do.call(dplyr::coalesce, x))
system.time(y2 <- do.call(hutils::coalesce, x))
system.time(y3 <- do.call(data.table::coalesce, x))
identical(y1,y2) && identical(y1,y3)
#   user  system elapsed (seconds)
#  4.935   1.876   6.810  # dplyr v0.8.1
#  3.122   0.831   3.956  # hutils v1.5.0
#  0.915   0.099   0.379  # data.table v1.12.4
}
}
\keyword{ data }

