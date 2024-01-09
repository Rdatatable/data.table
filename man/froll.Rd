\name{roll}
\alias{roll}
\alias{froll}
\alias{rolling}
\alias{sliding}
\alias{moving}
\alias{rollmean}
\alias{frollmean}
\alias{rollsum}
\alias{rollmax}
\alias{frollsum}
\alias{frollmax}
\alias{rollapply}
\alias{frollapply}
\title{Rolling functions}
\description{
  Fast rolling functions to calculate aggregates on sliding windows. Function name and arguments are experimental.
}
\usage{
frollmean(x, n, fill=NA, algo=c("fast", "exact"),
          align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE)
frollsum(x, n, fill=NA, algo=c("fast","exact"),
         align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE)
frollmax(x, n, fill=NA, algo=c("fast","exact"),
         align=c("right", "left", "center"), na.rm=FALSE, hasNA=NA, adaptive=FALSE)
frollapply(x, n, FUN, \dots, fill=NA, align=c("right", "left", "center"), adaptive)
}
\arguments{
  \item{x}{ Vector, \code{data.frame} or \code{data.table} of integer, numeric or logical columns over which to calculate the windowed aggregations. May also be a list, in which case the rolling function is applied to each of its elements. }
  \item{n}{ Integer vector giving rolling window size(s). This is the \emph{total} number of included values. Adaptive rolling functions also accept a list of integer vectors. }
  \item{fill}{ Numeric; value to pad by. Defaults to \code{NA}. }
  \item{algo}{ Character, default \code{"fast"}. When set to \code{"exact"}, a slower (but more accurate) algorithm is used. It
    suffers less from floating point rounding errors by performing an extra pass, and carefully handles all non-finite values.
    It will use mutiple cores where available. See Details for more information. }
  \item{align}{ Character, specifying the "alignment" of the rolling window, defaulting to \code{"right"}. \code{"right"} covers preceding rows (the window \emph{ends} on the current value); \code{"left"} covers following rows (the window \emph{starts} on the current value); \code{"center"} is halfway in between (the window is \emph{centered} on the current value, biased towards \code{"left"} when \code{n} is even). }
  \item{na.rm}{ Logical, default \code{FALSE}. Should missing values be removed when
    calculating window? For details on handling other non-finite values, see Details. }
  \item{hasNA}{ Logical. If it is known that \code{x} contains \code{NA}
    then setting this to \code{TRUE} will speed up calculation. Defaults to \code{NA}. }
  \item{adaptive}{ Logical, default \code{FALSE}. Should the rolling function be calculated adaptively? See Details below. }
  \item{FUN}{ The function to be applied to the rolling window; see Details for restrictions. }
  \item{\dots}{ Extra arguments passed to \code{FUN} in \code{frollapply}. }
}
\details{
  \code{froll*} functions accept vectors, lists, \code{data.frame}s or
  \code{data.table}s. They always return a list except when the input is a
  \code{vector} and \code{length(n)==1}, in which case a \code{vector}
  is returned, for convenience. Thus, rolling functions can be used
  conveniently within \code{data.table} syntax.

  Argument \code{n} allows multiple values to apply rolling functions on
  multiple window sizes. If \code{adaptive=TRUE}, then \code{n} must be a list,
  see \emph{Adaptive rolling functions} section below for details.

  When multiple columns or multiple window widths are provided, then they
  are run in parallel. The exception is for \code{algo="exact"}, which runs in
  parallel already. See \code{\link{setDTthreads}} for defaults and further details on parallelism in data.table.
}
\section{\code{hasNA} argument}{
  \code{hasNA} can be used to speed up processing in cases when it is known
  whether \code{x} contains infinite values \code{NA, NaN, +Inf, -Inf}.
  \itemize{
    \item{ Default \code{hasNA=NA} will use faster \code{NA} agnostic implementation,
    but when \code{NA}s are detected it will re-run \code{NA} aware implementation. }
    \item{ \code{hasNA=TRUE} will use \code{NA} aware implementation straightaway. }
    \item{ \code{hasNA=FALSE} will use faster \code{NA} agnostic implementation.
    Then depending on the rolling function it will either
    \itemize{
      \item{ (\emph{mean, sum}) detect \code{NA}s, raise warning, re-run \code{NA} aware. }
      \item{ (\emph{max}) not detect \code{NA}s and may silently produce an incorrect
      answer. }}
    Therefore \code{hasNA=FALSE} should be used with care.
    }
  }
}
\section{Implementation}{
  \itemize{
    \item{ \code{algo="fast"} uses \emph{"on-line"} algorithm, and
  all of \code{NaN, +Inf, -Inf} are treated as \code{NA}. Not all functions
  have \emph{fast} implementation available. As of now
  \emph{max} and \code{adaptive=TRUE} does not have, therefore it will
  automatically fall back to \emph{exact} implementation. \code{datatable.verbose}
  option can be used to check that. }
    \item{ \code{algo="exact"} will make rolling functions to use a more
  computationally-intensive algorithm. For each observation from input vector
  it will compute a function on a window from scratch (complexity \eqn{O(n^2)}).
  Depeneding on the function, this algorithm may suffers less from
  floating point rounding error (the same consideration applies to base \code{\link[base]{mean}}).
  Algorithm also handles \code{NaN, +Inf, -Inf} consistently to
  base R, unless - for some functions (e.g. \emph{max}) - \code{hasNA}is \code{FALSE}
  but NAs are present. In case of some functions (like \emph{mean}), it will additionally
  make extra pass to perform floating point error correction. Error
  corrections might not be truly exact on some platforms (like Windows)
  when using multiple threads. }
  }
}
\section{Adaptive rolling functions}{
  Adaptive rolling functions are a special case where each
  observation has its own corresponding rolling window width. \code{n}
  argument must be a list, then each list element must be an integer vector
  of window sizes corresponding to every single observation in each column;
  see Examples. Due to the logic or implementation of adaptive rolling
  functions, the following restrictions apply:
  \itemize{
    \item \code{align} does not support \code{"center"}.
    \item if list of vectors is passed to \code{x}, then all
      vectors within it must have equal length.
    \item functionality in not suported in \code{frollapply}.
  }
}
\section{\code{frollapply}}{
  \code{frollapply} computes rolling aggregate on arbitrary R functions.
  \code{adaptive} argument is not supported. The input
  \code{x} (first argument) to the function \code{FUN}
  is coerced to \emph{numeric} beforehand and \code{FUN}
  has to return a scalar \emph{numeric} value. Checks for that are made only
  during the first iteration when \code{FUN} is evaluated. Edge cases can be
  found in examples below. Any R function is supported, but it is not optimized
  using our own C implementation -- hence, for example, using \code{frollapply}
  to compute a rolling average is inefficient. It is also always single-threaded
  because there is no thread-safe API to R's C \code{eval}. Nevertheless we've
  seen the computation speed up vis-a-vis versions implemented in base R.
}
\section{\code{zoo} package users notice}{
  Users coming from most popular package for rolling functions
  \code{zoo} might expect following differences in \code{data.table}
  implementation.
  \itemize{
    \item rolling function will always return result of the same length
      as input.
    \item \code{fill} defaults to \code{NA}.
    \item \code{fill} accepts only constant values. It does not support
      for \emph{na.locf} or other functions.
    \item \code{align} defaults to \code{"right"}.
    \item \code{na.rm} is respected, and other functions are not needed
      when input contains \code{NA}.
    \item integers and logical are always coerced to double.
    \item when \code{adaptive=FALSE} (default), then \code{n} must be a
      numeric vector. List is not accepted.
    \item when \code{adaptive=TRUE}, then \code{n} must be vector of
      length equal to \code{nrow(x)}, or list of such vectors.
    \item \code{partial} window feature is not supported, although it can
      be accomplished by using \code{adaptive=TRUE}, see examples.
      \code{NA} is always returned for incomplete windows.
  }
}
\value{
  A list except when the input is a \code{vector} and
  \code{length(n)==1} in which case a \code{vector} is returned.
}
\note{
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

# frollsum
frollsum(d, 3:4)

# frollapply
frollapply(d, 3:4, sum)
f = function(x, ...) if (sum(x, ...)>5) min(x, ...) else max(x, ...)
frollapply(d, 3:4, f, na.rm=TRUE)

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
system.time(ans5<-frollapply(x, n, mean))
anserr = list(
  fastma = ans2-ans1,
  froll_fast = ans3-ans1,
  froll_exact = ans4-ans1,
  frollapply = ans5-ans1
)
errs = sapply(lapply(anserr, abs), sum, na.rm=TRUE)
sapply(errs, format, scientific=FALSE) # roundoff

# frollapply corner cases
f = function(x) head(x, 2)     ## FUN returns non length 1
try(frollapply(1:5, 3, f))
f = function(x) {              ## FUN sometimes returns non length 1
  n = length(x)
  # length 1 will be returned only for first iteration where we check length
  if (n==x[n]) x[1L] else range(x) # range(x)[2L] is silently ignored!
}
frollapply(1:5, 3, f)
options(datatable.verbose=TRUE)
x = c(1,2,1,1,1,2,3,2)
frollapply(x, 3, uniqueN)     ## FUN returns integer
numUniqueN = function(x) as.numeric(uniqueN(x))
frollapply(x, 3, numUniqueN)
x = c(1,2,1,1,NA,2,NA,2)
frollapply(x, 3, anyNA)       ## FUN returns logical
as.logical(frollapply(x, 3, anyNA))
options(datatable.verbose=FALSE)
f = function(x) {             ## FUN returns character
  if (sum(x)>5) "big" else "small"
}
try(frollapply(1:5, 3, f))
f = function(x) {             ## FUN is not type-stable
  n = length(x)
  # double type will be returned only for first iteration where we check type
  if (n==x[n]) 1 else NA # NA logical turns into garbage without coercion to double
}
try(frollapply(1:5, 3, f))
}
\seealso{
  \code{\link{shift}}, \code{\link{data.table}}
}
\references{
  \href{https://en.wikipedia.org/wiki/Round-off_error}{Round-off error}
}
\keyword{ data }
