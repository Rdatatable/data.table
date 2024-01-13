\name{frev}
\alias{frev}
\alias{rev}
\title{Fast reverse}
\description{
  Similar to \code{base::rev} but \emph{much faster}.
}

\usage{
frev(x, copy=TRUE)
}
\arguments{
  \item{x}{ atomic \code{vector} or \code{list} }
  \item{copy}{ logical (default is \code{TRUE}). If \code{FALSE} vector is reversed in-place. }
}

\value{
  \code{frev} returns a \code{factor}.
}

\examples{
# on vectors
x = setNames(1:26, letters)
frev(x[1:10])
# reverse in-place
frev(x, copy=FALSE)
x

# list
frev(list(1, "a"))
}
\keyword{ data }
