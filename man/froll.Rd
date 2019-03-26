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
  Fast rolling functions to calculate aggregates on sliding window. Function name and arguments are experimental.
}

\usage{
frollmean(x, n, fill=NA, algo=c("fast", "exact"), align=c("right",
  "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE,
  verbose=getOption("datatable.verbose"))
}
\arguments{
  \item{x}{ vector, list, data.frame or data.table of numeric columns. }
  \item{n}{ integer vector, for adaptive rolling function also list of
    integer vectors, rolling window size. }
  \item{fill}{ numeric, value to pad by. Defaults to \code{NA}. }
  \item{algo}{ character, default \code{"fast"}. When set to \code{"exact"},
    then slower algorithm is used. It suffers less from floating point
    rounding error, performs extra pass to adjust rounding error
    correction and carefully handles all non-finite values. If available
    it will use multiple cores. Might not be really \emph{exact} on
    Windows. See details for more information. }
  \item{align}{ character, define if rolling window covers preceding rows
    (\code{"right"}), following rows (\code{"left"}) or centered
    (\code{"center"}). Defaults to \code{"right"}. }
  \item{na.rm}{ logical. Should missing values be removed when
    calculating window? Defaults to \code{FALSE}. For details on handling
    other non-finite values, see details below. }
  \item{hasNA}{ logical. If it is known that \code{x} contains \code{NA}
    then setting to \code{TRUE} will speed up. Defaults to \code{NA}. }
  \item{adaptive}{ logical, should adaptive rolling function be
    calculated, default \code{FALSE}. See details below. }
  \item{verbose}{ logical, default \code{getOption("datatable.verbose")},
    \code{TRUE} turns on status and information messages to the console,
    it also disable parallel processing. }
}
\details{
  \code{froll*} functions accepts vectors, lists, data.frames or
  data.tables. They always return a list except when the input is a
  \code{vector} and \code{length(n)==1} in which case a \code{vector}
  is returned, for convenience. Thus rolling functions can be used
  conveniently within data.table syntax.

  Argument \code{n} allows multiple values to apply rolling functions on
  multiple window sizes. If \code{adaptive=TRUE}, then it expects a list.
  Each list element must be integer vector of window sizes corresponding
  to every single observation in each column.

  When \code{algo="fast"} then \emph{on-line} algorithm is used, also
  any \code{NaN, +Inf, -Inf} is treated as \code{NA}.
  Setting \code{algo="exact"} will make rolling functions to use
  compute-intensive algorithm that suffers less from floating point
  rounding error. It will additionally make extra pass to perform floating
  point error correction. It also handles \code{NaN, +Inf, -Inf}
  consistently to base R. Although might not be really exact on
  Windows when using multiple threads.

  Adaptive rolling functions are special cases where for each single
  observation has own corresponding rolling window width. Due to the logic
  of adaptive rolling functions, following restrictions apply:
  \itemize{
    \item{ \code{align} only \code{"right"}. }
    \item{ if list of vectors is passed to \code{x}, then all
      list vectors must have equal length. }
  }

  When multiple columns or multiple windows width are provided, then they
  are run in parallel. Eventually nested parallelism occurs when
  \code{algo="exact"}, see examples.
}
\value{
  A list except when the input is a \code{vector} and
  \code{length(n)==1} in which case a \code{vector} is returned.
}
\note{
  Users coming from most popular package for rolling functions
  \code{zoo} might expect following differences in \code{data.table}
  implementation.
  \itemize{
    \item{ rolling function will always return result of the same length
      as input. }
    \item{ \code{fill} defaults to \code{NA}. }
    \item{ \code{fill} accepts only constant values. It does not support
      for \emph{na.locf} or other functions. }
    \item{ \code{align} defaults to \code{"right"}. }
    \item{ \code{na.rm} is respected, and other functions are not needed
      when input contains \code{NA}. }
    \item{ integers are always coerced to double. }
    \item{ when \code{adaptive=FALSE} (default), then \code{n} must be a
      numeric vector. List is not accepted. }
    \item{ when \code{adaptive=TRUE}, then \code{n} must be vector of
      length equal to \code{nrow(x)}, or list of such vectors. }
    \item{ \code{partial} window feature is not supported, although it can
      be accomplished by using \code{adaptive=TRUE}, see examples. }
  }
  
  Be aware that rolling functions operates on the physical order of input.
  If the intent is to roll values in a vector by a logical window, for
  example an hour, or a day, one has to ensure that there are no gaps in
  input. For details see \href{https://github.com/Rdatatable/data.table/issues/3241}{issue #3241}.
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
## three calls above will use multiple cores when available

# partial window using adaptive rolling function
an = function(n, len) c(seq.int(n), rep(n, len-n))
n = an(3, nrow(d))
frollmean(d, n, adaptive=TRUE)

# performance vs exactness
set.seed(108)
x = sample(c(rnorm(1e3, 1e6, 5e5), 5e9, 5e-9))
n = 15
ma = function(x, n, na.rm=FALSE) {
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
system.time(ans3<-frollmean(x, n))
system.time(ans4<-frollmean(x, n, algo="exact"))
anserr = list(
  fastma = ans2-ans1,
  froll_fast = ans3-ans1,
  froll_exact = ans4-ans1
)
errs = sapply(lapply(anserr, abs), sum, na.rm=TRUE)
sapply(errs, format, scientific=FALSE) # roundoff
}
\seealso{
  \code{\link{shift}}, \code{\link{data.table}}
}
\references{
  \href{https://en.wikipedia.org/wiki/Round-off_error}{Round-off error}
}
\keyword{ data }
