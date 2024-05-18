\name{frev}
\alias{frev}
\alias{rev}
\title{Fast reverse}
\description{
  Similar to \code{\link[base]{rev}} but \emph{much faster}.
}

\usage{
frev(x)
}
\arguments{
  \item{x}{ An atomic \code{vector} or \code{list}. }
}

\details{
  \code{frev} does not retain attributes (similar to \code{\link[base]{rev}}).
}

\value{
  \code{frev} returns the input reversed.
}

\examples{
# on vectors
x = setNames(1:26, letters)
frev(x[1:10])

# list
frev(list(1, "a"))
}
\keyword{ data }
