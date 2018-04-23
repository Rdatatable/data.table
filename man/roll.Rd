\name{roll}
\alias{roll}
\alias{rolling}
\alias{sliding}
\alias{moving}
\alias{rollmean}
\title{Rolling functions}
\description{
  Rolling functions to calculate aggregates on sliding window.
}

\usage{
rollmean(x, n, fill=NA, align=c("right", "left", "center"), na.rm=FALSE,
adaptive=is.list(n))
}
\arguments{
  \item{x}{ vector, list, data.frame or data.table of numeric fields. }
  \item{n}{ integer vector or list of integer vectors, window size. }
  \item{fill}{ numeric, value to pad by, default \code{NA}. }
  \item{align}{ character, define if window frame covers preceding rows
    \code{"right"}, following rows \code{"left"} or centered
    \code{"center"}, default \code{"right"}. }
  \item{na.rm}{ logical, should missing values be removed when
    calculating window, default \code{FALSE}. }.
  \item{adaptive}{ logical, should adaptive rolling function be
    calculated, default \code{is.list(n)}. }
}
\details{
  \code{roll*} functions accepts vectors, lists, data.frames or
  data.tables. They always returns a list except when the input is a
  \code{vector} and \code{length(n) == 1} in which case a \code{vector}
  is returned, for convenience. This is so that it can be used
  conveniently within data.table's syntax. 

  Argument \code{n} allows multiple values and if \code{adaptive ==
    TRUE} then it expects a list, each list element can be integer
  vector of window sizes corresponding to each row-column from \code{x}.
}
\value{
  A list or a vector.
}
\note{
  Users coming from most popular package for rolling function \code{zoo}
  might expect following difference in data.table implementation.
  \itemize{
    \item{ rollmean will always return same length of results as provided
    input.}
    \item{ fill by default \code{NA}. }
    \item( fill supports accept only constant values, no support for
    \emph{na.locf} or other functions. }
    \item{ align by default "right". }
    \item{ integers always coerced to double (also for rollsum,
    rollprod, rollmin, rollmax).  }
    \item{ when \code{adaptive==TRUE} \code{n} can be vector same length as \code{x}, or list
    of such vectors. }
  }
}
\examples{
rollmean(1:16, c(3, 4))
# see tests/roll.R for now
}
\seealso{
  \code{\link{shift}}, \code{\link{data.table}}
}
\keyword{ data }
