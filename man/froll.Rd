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
  Fast rolling functions to calculate aggregates on sliding windows.
}
\usage{
  frollmean(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"),
    na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, hasNA, give.names=FALSE)
  frollsum(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"),
    na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, hasNA, give.names=FALSE)
  frollmax(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"),
    na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, hasNA, give.names=FALSE)
  frollapply(x, n, FUN, \dots, fill=NA, align=c("right","left","center"),
    adaptive=FALSE, partial=FALSE, give.names=FALSE)
}
\arguments{
  \item{x}{ Vector, \code{data.frame} or \code{data.table} of integer, numeric or logical columns over which to calculate the windowed aggregations. May also be a list, in which case the rolling function is applied to each of its elements. }
  \item{n}{ Integer vector giving rolling window size(s). This is the \emph{total} number of included values in aggregate function. Adaptive rolling functions also accept a list of integer vectors when applying multiple window sizes. }
  \item{fill}{ Numeric; value to pad by. Defaults to \code{NA}. }
  \item{algo}{ Character, default \code{"fast"}. When set to \code{"exact"}, a slower (but more accurate) algorithm is used. It
    suffers less from floating point rounding errors by performing an extra pass, and carefully handles all non-finite values.
    It will use multiple cores where available. See Details for more information. }
  \item{align}{ Character, specifying the "alignment" of the rolling window, defaulting to \code{"right"}. \code{"right"} covers preceding rows (the window \emph{ends} on the current value); \code{"left"} covers following rows (the window \emph{starts} on the current value); \code{"center"} is halfway in between (the window is \emph{centered} on the current value, biased towards \code{"left"} when \code{n} is even). }
  \item{na.rm}{ Logical, default \code{FALSE}. Should missing values be removed when calculating window? }
  \item{has.nf}{ Logical. If it is known whether \code{x} contains non-finite values (\code{NA}, \code{NaN}, \code{Inf}, \code{-Inf}), then setting this to \code{TRUE} or \code{FALSE} may speed up computation. Defaults to \code{NA}. See \emph{has.nf argument} section below for details. }
  \item{adaptive}{ Logical, default \code{FALSE}. Should the rolling function be calculated adaptively? See \emph{Adaptive rolling functions} section below for details. }
  \item{partial}{ Logical, default \code{FALSE}. Should the rolling window size(s) provided in \code{n} be computed also for leading incomplete running window. See \emph{\code{partial} argument} section below for details. }
  \item{FUN}{ The function to be applied to the rolling window in \code{frollapply}; See \emph{frollapply} section below for details. }
  \item{\dots}{ Extra arguments passed to \code{FUN} in \code{frollapply}. }
  \item{hasNA}{ Logical. Deprecated, use \code{has.nf} argument instead. }
  \item{give.names}{ Logical, default \code{FALSE}. When \code{TRUE}, names are automatically generated corresponding to names of \code{x} and names of \code{n}. If answer is an atomic vector, then the argument is ignored, see examples. }
}
\details{
  \code{froll*} functions accept vector, list, \code{data.frame} or \code{data.table}. Functions operate on a single vector; when passing a non-atomic input, then function is applied column-by-column, not to the complete set of columns at once.

  Argument \code{n} allows multiple values to apply rolling function on multiple window sizes. If \code{adaptive=TRUE}, then \code{n} can be a list to specify multiple window sizes for adaptive rolling computation. See \emph{Adaptive rolling functions} section below for details.

  When multiple columns and/or multiple windows widths are provided, then computations run in parallel (except for \code{frollapply}. The exception is for \code{algo="exact"}, which runs in parallel even for a single column and single window width. By default, data.table uses only half of the available CPUs, see \code{\link{setDTthreads}} for details on how to tune CPU usage.

  Adaptive rolling functions are a special case where each
  observation has its own corresponding rolling window width. Due to the logic
  of adaptive rolling functions, the following restrictions apply:
  \itemize{
    \item \code{align} only \code{"right"}.
    \item if list of vectors is passed to \code{x}, then all
      vectors within it must have equal length.
  }

  When multiple columns or multiple windows width are provided, then they
  are run in parallel. The exception is for \code{algo="exact"}, which runs in
  parallel already.

  \code{frollapply} computes rolling aggregate on arbitrary R functions.
  The input \code{x} (first argument) to the function \code{FUN}
  is coerced to \emph{numeric} beforehand and \code{FUN}
  has to return a scalar \emph{numeric} value. Checks for that are made only
  during the first iteration when \code{FUN} is evaluated. Edge cases can be
  found in examples below. Any R function is supported, but it is not optimized
  using our own C implementation -- hence, for example, using \code{frollapply}
  to compute a rolling average is inefficient. It is also always single-threaded
  because there is no thread-safe API to R's C \code{eval}. Nevertheless we've
  seen the computation speed up vis-a-vis versions implemented in base R.

  Setting \code{options(datatable.verbose=TRUE)} will display various
  information about how rolling function processed. It will not print
  information in real-time but only at the end of the processing.
}
\value{
  A list except when the input is a \code{vector} and \code{length(n)==1}, in which case a \code{vector} is returned, for convenience. Thus, rolling functions can be used conveniently within \code{data.table} syntax.
}
\note{
  Be aware that rolling functions operate on the physical order of input. If the intent is to roll values in a vector by a logical window, for example an hour or a day, then one has to use an adaptive rolling function or ensure that there are no gaps in the input. For details see \href{https://github.com/Rdatatable/data.table/issues/3241}{issue #3241}.
}
\section{\code{has.nf} argument}{
  \code{has.nf} can be used to speed up processing in cases when it is known if \code{x} contains (or not) non-finite values (\code{NA}, \code{NaN}, \code{Inf}, \code{-Inf}).
  \itemize{
    \item Default \code{has.nf=NA} uses faster implementation that does not support non-finite values, but when non-finite values are detected it will re-run non-finite supported implementation.
    \item \code{has.nf=TRUE} uses non-finite aware implementation straightaway.
    \item \code{has.nf=FALSE} uses faster implementation that does not support non-finite values. Then depending on the rolling function it will either:
    \itemize{
      \item (\emph{mean, sum}) detect non-finite, re-run non-finite aware.
      \item (\emph{max}) does not detect non-finites and may silently give incorrect answer.
    }
    In general \code{has.nf=FALSE && any(!is.finite(x))} should be considered as undefined behavior. Therefore \code{has.nf=FALSE} should be used with care.
  }
}
\section{Implementation}{
  Each rolling function has 4 different implementations. First factor that decides which implementation is being used is \code{adaptive} argument, see setion below for details. Then for each of those two algorithms (adaptive \code{TRUE} or \code{FALSE}) there are two \code{algo} argument values.
  \itemize{
    \item \code{algo="fast"} uses \emph{"on-line"}, single pass, algorithm.
    \itemize{
      \item \emph{max} rolling function will not do only a single pass but, on average \code{length(x)/n}, nested loops will be computed. The bigger the window the bigger advantage over algo \emph{exact} which computes \code{length(x)} nested loops. Note that \emph{exact} uses multiple CPUs so for a small window size and many CPUs it is possible it will be actually faster than \emph{fast} but in those cases elapsed timings will likely be far below a single second.
      \item Not all functions have \emph{fast} implementation available. As of now \emph{max} and \code{adaptive=TRUE} does not have, therefore it will automatically fall back to \emph{exact} implementation. \code{datatable.verbose} option can be used to check that.
    }
    \item \code{algo="exact"} will make rolling functions to use a more computationally-intensive algorithm. For each observation from input vector it will compute a function on a window from scratch (complexity \eqn{O(n^2)}).
    \itemize{
      \item Depeneding on the function, this algorithm may suffers less from floating point rounding error (the same consideration applies to base \code{\link[base]{mean}}).
      \item In case of \emph{mean} (and possibly other functions in future), it will additionally make extra pass to perform floating point error correction. Error corrections might not be truly exact on some platforms (like Windows) when using multiple threads.
    }
  }
}
\section{Adaptive rolling functions}{
  Adaptive rolling functions are a special case where each observation has its own corresponding rolling window width. Therefore, values passed to \code{n} argument must be series corresponding to observations in \code{x}. If multiple windows are meant to be computed, then a list of integer vectors is expected; each list element must be an integer vector of window size corresponding to observations in \code{x}; see Examples. Due to the logic or implementation of adaptive rolling functions, the following restrictions apply
  \itemize{
    \item \code{align} does not support \code{"center"}.
    \item if list of vectors is passed to \code{x}, then all vectors within it must have equal length due to the fact that length of adaptive window widths must match the length of vectors in \code{x}.
  }
}
\section{\code{partial} argument}{
  \code{partial=TRUE} is used to calculate rolling moments \emph{only} within the input itself. That is, at the boundaries (say, observation 2 for \code{n=4} and \code{align="right"}), we don't consider observations before the first as "missing", but instead shrink the window to be size \code{n=2}.
  In practice, this is the same as an \emph{adaptive} window, and could be accomplished, albeit less concisely, with a well-chosen \code{n} and \code{adaptive=TRUE}.
  In fact, we implement \code{partial=TRUE} using the same algorithms as \code{adaptive=TRUE}. Therefore \code{partial=TRUE} inherits the limitations of adaptive rolling functions, see above. Adaptive functions use more complex algorithms; if performance is important, \code{partial=TRUE} should be avoided in favour of computing only missing observations separately after the rolling function; see examples.
}
\section{\code{frollapply}}{
  \code{frollapply} computes rolling aggregate on arbitrary R functions. \code{adaptive} argument is not supported (to be changed). The input \code{x} (first argument) to the function \code{FUN} is coerced to \emph{numeric} beforehand(to be changed) and \code{FUN} has to return a scalar \emph{numeric} value (to be changed). Checks for that are made only during the first iteration when \code{FUN} is evaluated. Edge cases can be found in examples below. Any R function is supported, but it is not optimized using our own C implementation -- hence, for example, using \code{frollapply} to compute a rolling average is inefficient. It is also always single-threaded because there is no thread-safe API to R's C \code{eval}. Nevertheless we've seen the computation speed up vis-a-vis versions implemented in base R.
}
\section{\code{zoo} package users notice}{
  Users coming from most popular package for rolling functions \code{zoo} might expect following differences in \code{data.table} implementation
  \itemize{
    \item rolling function will always return result of the same length
      as input.
    \item \code{fill} defaults to \code{NA}.
    \item \code{fill} accepts only constant values. It does not support
      for \emph{na.locf} or other functions.
    \item \code{align} defaults to \code{"right"}.
    \item \code{na.rm} is respected, and other functions are not needed
      when input contains \code{NA}.
    \item integers and logical are always coerced to double
      (to be changed for frollapply).
    \item when \code{adaptive=FALSE} (default), then \code{n} must be a
      numeric vector. List is not accepted.
    \item when \code{adaptive=TRUE}, then \code{n} must be vector of
      length equal to \code{nrow(x)}, or list of such vectors.
    \item \code{by.column} argument is not yet supported in \code{frollapply}. For details and upvote see \href{https://github.com/Rdatatable/data.table/issues/4887}{issue #4887}.
  }
}
\examples{
# single vector and single window
frollmean(1:6, 3)

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

# frollsum
frollsum(d, 3:4)

# frollmax
frollmax(d, 3:4)

# partial=TRUE
x = 1:6/2
n = 3
ans1 = frollmean(x, n, partial=TRUE)
# same using adaptive=TRUE
an = function(n, len) c(seq.int(n), rep.int(n, len-n))
ans2 = frollmean(x, an(n, length(x)), adaptive=TRUE)
all.equal(ans1, ans2)
# speed up by using partial only for incomplete observations
ans3 = frollmean(x, n)
ans3[seq.int(n-1L)] = frollmean(x[seq.int(n-1L)], n, partial=TRUE)
all.equal(ans1, ans3)

# give.names
frollsum(list(x=1:5, y=5:1), c(tiny=2, big=4), give.names=TRUE)

# has.nf=FALSE should be used with care
frollmax(c(1,2,NA,4,5), 2)
frollmax(c(1,2,NA,4,5), 2, has.nf=FALSE)

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
  \code{\link{shift}}, \code{\link{data.table}}, \code{\link{setDTthreads}}
}
\references{
  \href{https://en.wikipedia.org/wiki/Round-off_error}{Round-off error}
}
\keyword{ data }
