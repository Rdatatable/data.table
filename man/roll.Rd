\name{roll}
\alias{roll}
\alias{froll}
\alias{rolling}
\alias{sliding}
\alias{moving}
\alias{frollmean}
\alias{frollsum}
\title{Rolling functions}
\description{
  Fast rolling functions to calculate aggregates on sliding window.
}

\usage{
frollmean(x, n, fill=NA, exact=FALSE, align=c("right", "left",
  "center"), partial=FALSE, na.rm=FALSE, hasNA=NA, adaptive=is.list(n),
  verbose=getOption("datatable.verbose"))
frollsum(x, n, fill=NA, exact=FALSE, align=c("right", "left",
  "center"), partial=FALSE, na.rm=FALSE, hasNA=NA, adaptive=is.list(n),
  verbose=getOption("datatable.verbose"))
}
\arguments{
  \item{x}{ vector, list, data.frame or data.table of numeric fields. }
  \item{n}{ integer vector, for adaptive rolling function also list of
    integer vectors, rolling window size. }
  \item{fill}{ numeric, value to pad by, default \code{NA}. }
  \item{exact}{ logical, default \code{FALSE}, should floating point
    rounding error be corrected, see details for more information. }
  \item{align}{ character, define if window frame covers preceding rows
    \code{"right"}, following rows \code{"left"} or centered
    \code{"center"}, default \code{"right"}. }
  \item{partial}{ logical, should partial window width be also
    calculated, if \code{TRUE} then \code{fill} argument is ignored,
    default \code{FALSE}. }
  \item{na.rm}{ logical, should missing values be removed when
    calculating window, default \code{FALSE}. }
  \item{hasNA}{ logical, if it is known that \code{x} contains \code{NA}
    then setting to \code{TRUE} will speed up, default \code{NA}. }
  \item{adaptive}{ logical, should adaptive rolling function be
    calculated, default \code{is.list(n)}. See details below. }
  \item{verbose}{ logical, default \code{getOption("datatable.verbose")},
    \code{TRUE} turns on status and information messages to the console, 
    it also disable parallel processing. }
}
\details{
  \code{froll*} functions accepts vectors, lists, data.frames or
  data.tables. They always returns a list except when the input is a
  \code{vector} and \code{length(n)==1} in which case a \code{vector}
  is returned, for convenience. This is so that it can be used
  conveniently within data.table's syntax.

  Argument \code{n} allows multiple values to calculate multiple rolling
  windows or if \code{adaptive==TRUE} then it expects a list, each list
  element must be integer vector of window size corresponding to every
  \code{column[row]} from \code{x}.

  Argument \code{exact} will make rolling functions to perform extra
  computation for floating point rounding error correction. This is useful
  mostly when when input data has some far outlier. In some edge cases
  \exact{TRUE} might result in bigger rounding error than \code{TRUE},
  see examples.
  
  Adaptive rolling functions are special cases where for each single
  observation own rolling window width has to be calculated. It is
  currently implemented with following limitations:
  \itemize{
    \item{ \code{align} only \code{"right"}. }
    \item{ \code{partial} only \code{FALSE}. }
    \item{ if list is passed to \code{x} then all list vectors must have
      equal length. }
  }
}
\value{
  A list except when the input is a \code{vector} and
  \code{length(n)==1} in which case a \code{vector} is returned.
}
\note{
  Users coming from most popular package for rolling functions
  \code{zoo} might expect following difference in data.table
  implementation.
  \itemize{
    \item{ rolling function will always return same length of results
      as provided input. }
    \item{ \code{fill} by default \code{NA}. }
    \item{ \code{fill} accept only constant values, no support for
      \emph{na.locf} or other functions. }
    \item{ \code{align} is by default \code{"right"}. }
    \item{ \code{na.rm} is respected, no need to use \code{frollapply}
      when having \code{NA} values. }
    \item{ integers are always coerced to double. }
    \item{ when \code{adaptive==FALSE} (default) then \code{n} must be a
      numeric vector, list is not accepted. }
    \item{ when \code{adaptive==TRUE} then \code{n} must be vector
      length equal to \code{nrow(x)}, or list of such vectors. }
  }
}
\examples{
d = as.data.table(list(1:6/2, 3:8/4))

# rollmean of single vector and single window
frollmean(d[, V1], 3)

# multiple columns at once
frollmean(d, 3)

# multiple windows at once
frollmean(d[, .(V1)], c(3, 4))

# multiple columns and multiple windows at once
frollmean(d, c(3, 4))

# floating point roundoff correction
x = c(1:10) # no outliers
n = 3
exact_f = frollmean(x, n)
exact_t = frollmean(x, n, exact=TRUE)
format(sum(abs(exact_f-exact_t), na.rm=TRUE), scientific=FALSE)
x = c(1:5, 1e9L, 7:10) # one outlier in the middle
n = 3
exact_f = frollmean(x, n)
exact_t = frollmean(x, n, exact=TRUE)
format(sum(abs(exact_f-exact_t), na.rm=TRUE), scientific=FALSE)
}
\seealso{
  \code{\link{shift}}, \code{\link{data.table}}
}
\references{
  \href{Round-off error}{https://en.wikipedia.org/wiki/Round-off_error}
}
\keyword{ data }
