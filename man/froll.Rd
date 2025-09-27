\name{froll}
\alias{rolling}
\alias{sliding}
\alias{moving}
\alias{running}
\alias{froll}
\alias{frollmean}
\alias{frollsum}
\alias{frollmax}
\alias{frollmin}
\alias{frollprod}
\alias{frollmedian}
\alias{roll}
\alias{rollmean}
\alias{rollsum}
\alias{rollmax}
\alias{rollmin}
\alias{rollprod}
\alias{rollmedian}
\title{Rolling functions}
\description{
  Fast rolling functions to calculate aggregates on a sliding window. For a user-defined rolling function see \code{\link{frollapply}}. For "time-aware" (irregularly spaced time series) rolling function see \code{\link{frolladapt}}.
}
\usage{
  frollmean(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"),
    na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA)
  frollsum(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"),
    na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA)
  frollmax(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"),
    na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA)
  frollmin(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"),
    na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA)
  frollprod(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"),
    na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA)
  frollmedian(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"),
    na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA)
}
\arguments{
  \item{x}{ Integer, numeric or logical vector, coerced to numeric, on which sliding window calculates an aggregate function. It supports vectorized input, then it needs to be a \code{data.table}, \code{data.frame} or a \code{list}, in which case a rolling function is applied to each column/vector. }
  \item{n}{ Integer, non-negative, rolling window size. This is the \emph{total} number of included values in aggregate function. In case of an adaptive rolling function window size has to be provided as a vector for each indivdual value of \code{x}. It supports vectorized input, then it needs to be a vector, or in case of an adaptive rolling a \code{list} of vectors. }
  \item{fill}{ Numeric; value to pad by. Defaults to \code{NA}. }
  \item{algo}{ Character, default \code{"fast"}. When set to \code{"exact"}, a slower (but more accurate) algorithm is used. It suffers less from floating point rounding errors by performing an extra pass, and carefully handles all non-finite values. It will use multiple cores where available. See Details for more information. }
  \item{align}{ Character, specifying the "alignment" of the rolling window, defaulting to \code{"right"}. \code{"right"} covers preceding rows (the window \emph{ends} on the current value); \code{"left"} covers following rows (the window \emph{starts} on the current value); \code{"center"} is halfway in between (the window is \emph{centered} on the current value, biased towards \code{"left"} when \code{n} is even). }
  \item{na.rm}{ Logical, default \code{FALSE}. Should missing values be removed when calculating window? }
  \item{has.nf}{ Logical. If it is known whether \code{x} contains non-finite values (\code{NA}, \code{NaN}, \code{Inf}, \code{-Inf}), then setting this to \code{TRUE} or \code{FALSE} may speed up computation. Defaults to \code{NA}. See \emph{has.nf argument} section below for details. }
  \item{adaptive}{ Logical, default \code{FALSE}. Should the rolling function be calculated adaptively? See \emph{Adaptive rolling functions} section below for details. }
  \item{partial}{ Logical, default \code{FALSE}. Should the rolling window size(s) provided in \code{n} be computed also for leading incomplete running window. See \emph{\code{partial} argument} section below for details. }
  \item{give.names}{ Logical, default \code{FALSE}. When \code{TRUE}, names are automatically generated corresponding to names of \code{x} and names of \code{n}. If answer is an atomic vector, then the argument is ignored, see examples. }
  \item{hasNA}{ Logical. Deprecated, use \code{has.nf} argument instead. }
}
\details{
  \code{froll*} functions accept vector, list, \code{data.frame} or \code{data.table}. Functions operate on a single vector; when passing a non-atomic input, then function is applied column-by-column, not to the complete set of columns at once.

  Argument \code{n} allows multiple values to apply rolling function on multiple window sizes. If \code{adaptive=TRUE}, then \code{n} can be a list to specify multiple window sizes for adaptive rolling computation. See \emph{Adaptive rolling functions} section below for details.

  When multiple columns and/or multiple window widths are provided, then computations run in parallel. The exception is for \code{algo="exact"}, which runs in parallel even for single column and single window width. By default, data.table uses only half of available CPUs, see \code{\link{setDTthreads}} for details on how to tune CPU usage.

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

  Setting \code{options(datatable.verbose=TRUE)} will display various
  information about how rolling function processed. It will not print
  information in real-time but only at the end of the processing.
}
\value{
  For a non \emph{vectorized} input (\code{x} is not a list, and \code{n} specify single rolling window) a \code{vector} is returned, for convenience. Thus, rolling functions can be used conveniently within \code{data.table} syntax. For a \emph{vectorized} input a list is returned.
}
\note{
  Be aware that rolling functions operate on the physical order of input. If the intent is to roll values in a vector by a logical window, for example an hour, or a day, then one has to ensure that there are no gaps in the input, or use adaptive rolling function to handle gaps, for which we provide helper function \code{\link{frolladapt}} to generate adaptive window size.
}
\section{\code{has.nf} argument}{
  \code{has.nf} can be used to speed up processing in cases when it is known if \code{x} contains (or not) non-finite values (\code{NA}, \code{NaN}, \code{Inf}, \code{-Inf}).
  \itemize{
    \item Default \code{has.nf=NA} uses faster implementation that does not support non-finite values, but when non-finite values are detected it will re-run non-finite supported implementation.
    \item \code{has.nf=TRUE} uses non-finite aware implementation straightaway.
    \item \code{has.nf=FALSE} uses faster implementation that does not support non-finite values. Then depending on the rolling function it will either:
    \itemize{
      \item (\emph{mean, sum, prod}) detect non-finite, re-run non-finite aware.
      \item (\emph{max, min, median}) does not detect non-finites and may silently produce an incorrect answer.
    }
    In general \code{has.nf=FALSE && any(!is.finite(x))} should be considered as undefined behavior. Therefore \code{has.nf=FALSE} should be used with care.
  }
}
\section{Implementation}{
  Each rolling function has 4 different implementations. First factor that decides which implementation is used is the \code{adaptive} argument (either \code{TRUE} or \code{FALSE}), see section below for details. Then for each of those two algorithms there are usually two implementations depending on the \code{algo} argument.
  \itemize{
    \item \code{algo="fast"} uses \emph{"online"}, single pass, algorithm.
    \itemize{
      \item \emph{max} and \emph{min} rolling function will not do only a single pass but, on average, they will compute \code{length(x)/n} nested loops. The larger the window, the greater the advantage over the \emph{exact} algorithm, which computes \code{length(x)} nested loops. Note that \emph{exact} uses multiple CPUs so for a small window sizes and many CPUs it may actually be faster than \emph{fast}. However, in such cases the elapsed timings will likely be far below a single second.
      \item \emph{median} will use a novel algorithm described by \emph{Jukka Suomela} in his paper \emph{Median Filtering is Equivalent to Sorting (2014)}. See references section for the link. Implementation here is extended to support arbitrary length of input and an even window size. Despite extensive validation of results this function should be considered experimental. When missing values are detected it will fall back to slower \code{algo="exact"} implementation.
      \item Not all functions have \emph{fast} implementation available. As of now adaptive \emph{max}, adaptive \emph{min} and adaptive \emph{median} do not have \emph{fast} implementation, therefore it will automatically fall back to \emph{exact} implementation. \code{datatable.verbose} option can be used to check that.
    }
    \item \code{algo="exact"} will make the rolling functions use a more computationally-intensive algorithm. For each observation in the input vector it will compute a function on a rolling window from scratch (complexity \eqn{O(n^2)}).
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
    \item integers and logical are always coerced to numeric.
    \item when \code{adaptive=FALSE} (default), then \code{n} must be a
      numeric vector. List is not accepted.
    \item when \code{adaptive=TRUE}, then \code{n} must be vector of
      length equal to \code{nrow(x)}, or list of such vectors.
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

# other functions
frollsum(d, 3:4)
frollmax(d, 3:4)
frollmin(d, 3:4)
frollprod(d, 3:4)
frollmedian(d, 3:4)

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
}
\seealso{
  \code{\link{frollapply}}, \code{\link{frolladapt}}, \code{\link{shift}}, \code{\link{data.table}}, \code{\link{setDTthreads}}
}
\references{
  \href{https://en.wikipedia.org/wiki/Round-off_error}{Round-off error}, \href{https://arxiv.org/abs/1406.1717}{"Median Filtering is Equivalent to Sorting" by Jukka Suomela}
}
\keyword{ data }
