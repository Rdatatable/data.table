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
  "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE,
  verbose=getOption("datatable.verbose"))
frollsum(x, n, fill=NA, exact=FALSE, align=c("right", "left",
  "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE,
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
  \item{na.rm}{ logical, should missing values be removed when
    calculating window, default \code{FALSE}. For details on handling
    other non finite values see details below. }
  \item{hasNA}{ logical, if it is known that \code{x} contains \code{NA}
    then setting to \code{TRUE} will speed up, default \code{NA}. }
  \item{adaptive}{ logical, should adaptive rolling function be
    calculated, default \code{FALSE}. See details below. }
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
  windows or if \code{adaptive=TRUE} then it expects a list, each list
  element must be integer vector of window size corresponding to every
  \code{column[row]} from \code{x}.

  Argument \code{exact} will make rolling functions to perform extra
  computation for floating point rounding error correction. This is useful
  mostly when when input data has distant outlier.
  
  When \code{exact=FALSE} is used then any \code{NaN, +Inf, -Inf} is
  treated as \code{NA}. For precise handling of non-finite values use
  \code{exact=TRUE}.
  
  Adaptive rolling functions are special cases where for each single
  observation own rolling window width has to be calculated. It is
  currently implemented with following limitations:
  \itemize{
    \item{ \code{align} only \code{"right"}. }
    \item{ if list of integer vectors is passed to \code{x} then all
      list vectors must have equal length. }
  }
}
\value{
  A list except when the input is a \code{vector} and
  \code{length(n)==1} in which case a \code{vector} is returned.
}
\note{
  Users coming from most popular package for rolling functions
  \code{zoo} might expect following difference in \code{data.table}
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
    \item{ when \code{adaptive=FALSE} (default) then \code{n} must be a
      numeric vector, list is not accepted. }
    \item{ when \code{adaptive=TRUE} then \code{n} must be vector
      length equal to \code{nrow(x)}, or list of such vectors. }
    \item{ there is no \code{partial} window support. }
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
## three above are embarrassingly parallel using openmp

# performance vs exactness
set.seed(108)
x = sample(c(rnorm(1e3, 1e6, 5e5), 5e9, 5e-9))
n = 15
ma = function(x, n, na.rm=FALSE, nf.rm=FALSE) {
  ans = rep(NA_real_, nx<-length(x))
  for (i in n:nx) ans[i] = mean(x[(i-n+1):i], na.rm=na.rm)
  ans
}
fastma = function(x, n, na.rm) {
  if (!missing(na.rm)) stop("NAs are unsupported, wrongly propagated by cumsum")
  cs = cumsum(x)
  scs = shift(cs, n)
  scs[n] = 0
  as.double((cs-scs)/n)
}
system.time(ans1<-ma(x, n))
system.time(ans2<-fastma(x, n))
system.time(ans3<-frollmean(x, n, exact=TRUE)) # parallel using openmp again
system.time(ans4<-frollmean(x, n))
anserr = list(
  froll_exact_f = ans4-ans1,
  froll_exact_t = ans3-ans1,
  fastma = ans2-ans1
)
errs = sapply(lapply(anserr, abs), sum, na.rm=TRUE)
sapply(errs, format, scientific=FALSE) # roundoff
}
\seealso{
  \code{\link{shift}}, \code{\link{data.table}}
}
\references{
  \href{Round-off error}{https://en.wikipedia.org/wiki/Round-off_error}
}
\keyword{ data }
