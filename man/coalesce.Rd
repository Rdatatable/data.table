\name{coalesce}
\alias{coalesce}
\alias{setcoalesce}
\title{ Coalescing missing values }
\description{
Fill in missing values in a vector by successively pulling from candidate vectors in order (\emph{a la} the ANSI SQL function of the same name).
}
\usage{
  coalesce(x, \dots, .dots=NULL)
}
\arguments{
  \item{x}{ A vector of values }
  \item{\dots}{ Vectors to be used successively as replacements for missing values of \code{x}; singletons are also accepted. }
  \item{.dots}{ Alternative interface to specify replacements by supplying a list directly. }
}
\details{
Factor type is not yet supported.
}
\value{
Atomic vector of same type as \code{x} provided on input, having \code{NA} values replaced by corresponding non-\code{NA} values of arguments provided to \dots.
}
\examples{
x = c(11L, NA, 13L, NA, 15L, NA)
y = c(NA, 12L, 5L, NA, NA, NA)
z = c(11L, NA, 1L, 14L, NA, NA)
coalesce(x, y, z)
}
\keyword{ data }


