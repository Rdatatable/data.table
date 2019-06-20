\name{coalesce}
\alias{coalesce}
\alias{setcoalesce}
\title{ Coalescing missing values }
\description{
Fill in missing values in a vector by successively pulling from candidate vectors in order. As per the ANSI SQL function COALESCE, \code{dplyr::coalesce} and \code{hutils::coalesce}.
Written in C, and multithreaded for numeric and factor types.
}
\usage{
  coalesce(x, \dots)
}
\arguments{
  \item{x}{ An atomic vector. Or, if \dots is missing, x may be a \code{list} of atomic vectors. }
  \item{\dots}{ Vectors of same class as \code{x} to be used successively as replacements for missing values of \code{x}; singletons are also accepted. }
}
\details{
Factor type is supported only when factor levels are equal.
}
\value{
Atomic vector of same type as \code{x} provided on input, having \code{NA} values replaced by corresponding non-\code{NA} values of arguments provided to \dots.
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

