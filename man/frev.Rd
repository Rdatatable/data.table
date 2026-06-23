\name{frev}
\alias{frev}
\alias{rev}
\title{Fast reverse}
\description{
  Similar to \code{\link[base]{rev}} but \emph{faster}.
}

\usage{
frev(x)
}
\arguments{
  \item{x}{ An atomic \code{vector} or \code{list}. }
}

\details{
  Similar to \code{\link[base]{rev}}, \code{frev} only retains three attributes: names, class, and factor levels.
}

\value{
  Returns the input reversed.
}

\examples{
# on vectors
x = setNames(1:10, letters[1:10])
frev(x)

# list
frev(list(1, "a", TRUE))
}
\keyword{ data }
