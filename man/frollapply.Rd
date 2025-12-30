\name{frollapply}
\alias{frollapply}
\alias{rollapply}
\title{Rolling user-defined function}
\description{
  Fast rolling user-defined function (\emph{UDF}) to calculate on a sliding window. Experimental. Please read, at least, \emph{caveats} section below. For "time-aware" (irregularly spaced time series) rolling function see \code{\link{frolladapt}}.
}
\usage{
  frollapply(X, N, FUN, \dots, by.column=TRUE, fill=NA,
    align=c("right","left","center"), adaptive=FALSE, partial=FALSE,
    give.names=FALSE, simplify=TRUE, x, n)
}
\arguments{
  \item{X}{ Atomic vector, \code{data.frame}, \code{data.table} or a \code{list} on which sliding window calculates \code{FUN} function. How the \code{X} is handled depends on the \code{by.column} argument. It supports vectorized input, for \code{by.column=TRUE} it needs to be a \code{data.table}, \code{data.frame} or a \code{list}, and for \code{by.column=FALSE} list of data.frames/data.tables, but not a list of lists. }
  \item{N}{ Integer, non-negative, non-NA, rolling window size. This is the \emph{total} number of included values in aggregate function. In case of an adaptive rolling function window size has to be provided as a vector for each individual value of \code{X}. It supports vectorized input, then it needs to be a vector, or in case of an adaptive rolling a \code{list} of vectors. }
  \item{FUN}{ The function to be applied on subsets of \code{X}. }
  \item{\dots}{ Extra arguments passed to \code{FUN}. Note that argument names passed to \dots should not overlap with arguments of \code{frollapply}. }
  \item{by.column}{ Logical. When \code{TRUE} (default) then \code{X} of types list/data.frame/data.table is treated as vectorized input rather an object to apply rolling window on. Setting to \code{FALSE} allows rolling window to be applied on multiple variables, using data.frame, data.table or a list, as a whole. For details see \emph{\code{by.column} argument} section below. }
  \item{fill}{ An object; value to pad by for an incomplete window iteration. Defaults to \code{NA}. When \code{partial=TRUE} this argument is ignored. }
  \item{align}{ Character, specifying the "alignment" of the rolling window, defaulting to \code{"right"}. For details see \code{\link{froll}}. }
  \item{adaptive}{ Logical, default \code{FALSE}. Should the rolling function be calculated adaptively? For details see \code{\link{froll}}. }
  \item{partial}{ Logical, default \code{FALSE}. Should the rolling window size(s) provided in \code{N} be trimmed to available observations? For details see \code{\link{froll}}. }
  \item{give.names}{ Logical, default \code{FALSE}. When \code{TRUE}, names are automatically generated corresponding to names of \code{X} and names of \code{N}. If answer is an atomic vector, then the argument is ignored, see examples. }
  \item{simplify}{ Logical or a function. When \code{TRUE} (default) then internal \code{simplifylist} function is applied on a list storing results of all computations. When \code{FALSE} then list is returned without any post-processing. Argument can take a function as well, then the function is applied to a list that would have been returned when \code{simplify=FALSE}. If results are not automatically simplified when \code{simplify=TRUE} then, for backward compatibility, one should use \code{simplify=FALSE} explicitly. See \emph{\code{simplify} argument} section below for details. }
  \item{x}{ Deprecated, use \code{X} instead. }
  \item{n}{ Deprecated, use \code{N} instead. }
}
\value{
  Argument \code{simplify} impacts the type returned. Its default value \code{TRUE} is set for convenience and backward compatibility, but it is advised to use \code{simplify=unlist} (or other desired function) instead.
  \itemize{
    \item \code{simplify=FALSE} will always return list where each element will be a result of each iteration.
    \item \code{simplify=unlist} (or any other function) will return object returned by provided function as supplied with results of \code{frollapply} using \code{simplify=FALSE}.
    \item \code{simplify=TRUE} will try to simplify results by \code{unlist}, \code{rbind} or other functions, its behavior is subject to change, see \emph{\code{simplify} argument} section below for more details.
  }
}
\section{\code{by.column} argument}{
  Setting \code{by.column} to \code{FALSE} allows to apply function on multiple variables rather than a single vector. Then \code{X} expects to be data.table, data.frame or a list of equal length vectors, and window size provided in \code{N} refers to number of rows (or length of a vectors in a list). See examples for use cases. Error \emph{"incorrect number of dimensions"} can be commonly observed when \code{by.column} was not set to \code{FALSE} when \code{FUN} expects its input to be a data.table or data.frame.
}
\section{\code{simplify} argument}{
  When set to \code{TRUE} (default), then results from rolling function which are normally stored in a list may be simplified either with \code{unlist} or \code{rbindlist}. It also attempts to match type, size and names of \code{fill} argument to the results of a function.
  One should avoid \code{simplify=TRUE} when writing robust code. One reason is performance, as explained in \emph{Performance consideration} section below. Another is backward compatibility. For backward compatibility and performance one should always provide desired function to \code{simplify} explicitly. In future version we may change internal \code{simplifylist} function, then \code{simplify=TRUE} may return object of a different type, breaking downstream code.
}
\section{Caveats}{
  With great power comes great responsibility.
  \enumerate{
    \item An optimization used to avoid repeated allocation of window subsets (explained more deeply in \emph{Implementation} section below) may, in special cases, return rather surprising results:
\preformatted{
setDTthreads(1)
frollapply(c(1, 9), N=1L, FUN=identity) ## unexpected
#[1] 9 9
frollapply(c(1, 9), N=1L, FUN=list) ## unexpected
#      V1
#   <num>
#1:     9
#2:     9
setDTthreads(2, throttle=1) ## disable throttle
frollapply(c(1, 9), N=1L, FUN=identity) ## good only because threads >= input
#[1] 1 9                                ## on Linux and Macos
frollapply(c(1, 5, 9), N=1L, FUN=identity) ## unexpected again
#[1] 5 5 9
}
    Problem occurs, in rather unlikely scenarios for rolling computations, when objects returned from a function can be its input (i.e. \code{identity}), or a reference to it (i.e. \code{list}), then one has to add extra \code{copy} call:
\preformatted{
setDTthreads(1)
frollapply(c(1, 9), N=1L, FUN=function(x) copy(identity(x))) ## only 'copy' would be equivalent here
#[1] 1 9
frollapply(c(1, 9), N=1L, FUN=function(x) copy(list(x)))
#      V1
#   <num>
#1:     1
#2:     9
}
    \item \code{FUN} calls are internally passed to \code{parallel::mcparallel} to evaluate them in parallel. We inherit few limitations from \code{parallel} package explained below. This optimization can be disabled completely by calling \code{setDTthreads(1)}, in which case the limitations listed below do not apply because all evaluations of \code{FUN} will be made sequentially without use of \code{parallel} package. Note that on Windows platform this optimization is always disabled due to lack of \emph{fork} used by \code{parallel} package. One can use \code{options(datatable.verbose=TRUE)} to get extra information if \code{frollapply} is running multithreaded or not.
      \itemize{
        \item Warnings produced inside the function are silently ignored; for consistency we ignore warnings also when running single threaded path.
        \item \code{FUN} should not use any on-screen devices, GUI elements, tcltk, multithreaded libraries.
        \item \code{setDTthreads(1L)} is passed to forked processes, therefore any data.table code inside \code{FUN} will be forced to be single threaded. It is advised not to call \code{setDTthreads} inside \code{FUN}. \code{frollapply} is already parallelized, and nested parallelism is rarely a good idea.
        \item Any operation that could misbehave when run in parallel has to be handled. For example, writing to the same file from multiple CPU threads.
\preformatted{
old = setDTthreads(1L)
frollapply(iris, 5L, by.column=FALSE, FUN=fwrite, file="rolling-data.csv", append=TRUE)
setDTthreads(old)
}
        \item Objects returned from forked processes, \code{FUN}, are serialized. This may cause problems for objects that are meant not to be serialized, like data.table. We are handling that for data.table class internally in \code{frollapply} whenever \code{FUN} is returning data.table (which is checked on the results of the first \code{FUN} call so it assumes function is type stable). If data.table is nested in another object returned from \code{FUN} then the problem may still manifest, in such case one has to call \code{setDT} on objects returned from \code{FUN}. This can be also nicely handled via \code{simplify} argument when passing a function that calls \code{setDT} on nested data.table objects returned from \code{FUN}. Anyway, returning data.table from \code{FUN} should, in majority of cases, be avoided from the performance reasons, see \emph{UDF optimization} section for details.
\preformatted{
setDTthreads(2, throttle=1) ## disable throttle
## frollapply will fix DT in most cases
ans = frollapply(1:2, 2, data.table)
.selfref.ok(ans)
#[1] TRUE
ans = frollapply(1:2, 2, data.table, simplify=FALSE)
.selfref.ok(ans[[2L]])
#[1] TRUE

## nested DT not fixed
ans = frollapply(1:2, 2, function(x) list(data.table(x)), fill=list(data.table(NA)), simplify=FALSE)
.selfref.ok(ans[[2L]][[1L]])
#[1] FALSE
#### now if we want to use it
set(ans[[2L]][[1L]],, "newcol", 1L)
#Error in set(ans[[2L]][[1L]], , "newcol", 1L) :
#  This data.table has either been loaded from disk (e.g. using readRDS()/load()) or constructed manually (e.g. using structure()). Please run setDT() or setalloccol() on it first (to pre-allocate space for new columns) before assigning by reference to it.
#### fix as explained in error message
ans = lapply(ans, lapply, setDT)
.selfref.ok(ans[[2L]][[1L]])
#[1] TRUE

## fix inside frollapply via simplify
simplifix = function(x) lapply(x, lapply, setDT)
ans = frollapply(1:2, 2, function(x) list(data.table(x)), fill=list(data.table(NA)), simplify=simplifix)
.selfref.ok(ans[[2L]][[1L]])
#[1] TRUE

## automatic fix may not work for a non-type stable function
f = function(x) (if (x[1L]==1L) data.frame else data.table)(x)
ans = frollapply(1:3, 2, f, fill=data.table(NA), simplify=FALSE)
.selfref.ok(ans[[3L]])
#[1] FALSE
#### fix inside frollapply via simplify
simplifix = function(x) lapply(x, function(y) if (is.data.table(y)) setDT(y) else y)
ans = frollapply(1:3, 2, f, fill=data.table(NA), simplify=simplifix)
.selfref.ok(ans[[3L]])
#[1] TRUE

setDTthreads(2, throttle=1024) ## enable throttle
}
    }
    \item Due to possible future improvements of handling simplification of results returned from rolling function, the default \code{simplify=TRUE} may not be backward compatible for functions that produce results that haven't been already automatically simplified. See the \emph{\code{simplify} argument} section for details.
  }
}
\section{Performance consideration}{
  \code{frollapply} is meant to run any UDF function. If one needs to use a common function like \emph{mean, sum, max}, etc., then we have highly optimized, implemented in C language, rolling functions described in \code{\link{froll}} manual.\cr
  Most crucial optimizations are the ones to be applied on UDF. Those are discussed below in section \emph{UDF optimization}.
  \itemize{
    \item When using \code{by.column=FALSE}, subset the dataset before passing it to \code{X} to keep only columns relevant for the computation:
\preformatted{
x = setDT(lapply(1:1000, function(x) as.double(rep.int(x,1e4L))))
f = function(x) sum(x$V1 * x$V2)
system.time(frollapply(x, 100, f, by.column=FALSE))
#   user  system elapsed
#  0.373   0.069   0.234
system.time(frollapply(x[, c("V1","V2"), with=FALSE], 100, f, by.column=FALSE))
#   user  system elapsed
#  0.050   0.058   0.061
}
    \item Avoid \code{partial} argument, see \emph{\code{partial} argument} section of \code{\link{froll}} manual.
    \item Avoid \code{simplify=TRUE} and provide a function instead:
\preformatted{
x = rnorm(1e5)
system.time(frollapply(x, 2, function(x) 1L, simplify=TRUE))
#   user  system elapsed
#  0.227   0.095   0.236
system.time(frollapply(x, 2, function(x) 1L, simplify=unlist))
#   user  system elapsed
#  0.054   0.049   0.091
}
    \item CPU threads utilization in \code{frollapply} can be controlled by \code{\link{setDTthreads}}, which by default uses half of available CPU threads. Usage of multiple CPU threads will be throttled for small input, as described in \code{\link{setDTthreads}} manual.
    \item Parallel computation of \code{FUN} is handled by \code{parallel} package (part of R core since 2.14.0) and its \emph{fork} mechanism. \emph{Fork} is not available on Windows OS, therefore computations will always be single-threaded on that platform.
  }
}
\section{UDF optimization}{
  FUN will be evaluated many times so should be highly optimized. Tips below are not specific to \code{frollapply} and can be applied to any code meant to run in many iterations.
  \itemize{
    \item It is usually better to return the most lightweight objects from \code{FUN}, for example it will be faster to return a list rather a data.table. In the case presented below, \code{simplify=TRUE} is calling \code{rbindlist} on the results anyway, which makes the results equal:
\preformatted{
fun1 = function(x) {tmp=range(x); data.table(min=tmp[1L], max=tmp[2L])}
fun2 = function(x) {tmp=range(x); list(min=tmp[1L], max=tmp[2L])}
fill1 = data.table(min=NA_integer_, max=NA_integer_)
fill2 = list(min=NA_integer_, max=NA_integer_)
system.time(a<-frollapply(1:1e4, 100, fun1, fill=fill1, simplify=rbindlist))
#   user  system elapsed
#  0.934   0.347   0.706
system.time(b<-frollapply(1:1e4, 100, fun2, fill=fill2, simplify=rbindlist))
#   user  system elapsed
#  0.010   0.033   0.094
all.equal(a, b)
#[1] TRUE
}
    \item Code that is not dependent on a rolling window should be taken out as pre or post computation:
\preformatted{
x = c(1L,3L)
system.time(for (i in 1:1e6) sum(x+1L))
#   user  system elapsed
#  0.218   0.002   0.221
system.time({y = x+1L; for (i in 1:1e6) sum(y)})
#   user  system elapsed
#  0.160   0.001   0.161
}
    \item Being strict about data types removes the need for R to handle them automatically:
\preformatted{
x = vector("integer", 1e6)
system.time(for (i in 1:1e6) x[i] = NA)
#   user  system elapsed
#  0.114   0.000   0.114
system.time(for (i in 1:1e6) x[i] = NA_integer_)
#   user  system elapsed
#  0.029   0.000   0.030
}
    \item If a function calls another function under the hood, it is usually better to call the latter one directly:
\preformatted{
x = matrix(c(1L,2L,3L,4L), c(2L,2L))
system.time(for (i in 1:1e4) colSums(x))
#   user  system elapsed
#  0.033   0.000   0.033
system.time(for (i in 1:1e4) .colSums(x, 2L, 2L))
#   user  system elapsed
#  0.010   0.002   0.012
}
    \item There are many functions that may be optimized for scaling up with larger input, yet for a small input they may incur bigger overhead comparing to simpler counterparts. One may need to experiment on own data, but low overhead functions are likely to be faster when evaluated over many iterations:
\preformatted{
## uniqueN
x = c(1L,3L,5L)
system.time(for (i in 1:1e4) uniqueN(x))
#   user  system elapsed
#  0.078   0.001   0.080
system.time(for (i in 1:1e4) length(unique(x)))
#   user  system elapsed
#  0.018   0.000   0.018
## column subset
x = data.table(v1 = c(1L,3L,5L))
system.time(for (i in 1:1e4) x[, v1])
#   user  system elapsed
#  1.952   0.011   1.964
system.time(for (i in 1:1e4) x[["v1"]])
#   user  system elapsed
#  0.036   0.000   0.035
}
  }
}
\section{Implementation}{
  Evaluation of UDF comes with very limited capabilities for optimizations, therefore speed improvements in \code{frollapply} should not be expected as good as in other data.table fast functions. \code{frollapply} is implemented almost exclusively in R, rather than C. Its speed improvement comes from two optimizations that have been applied:
  \enumerate{
    \item No repeated allocation of a rolling window subset.\cr
    Object (type of \code{X} and size of \code{N}) is allocated once (for each CPU thread), and then for each iteration this object is being re-used by copying expected subset of data into it. This means we still have to subset data on each iteration, but we only copy data into pre-allocated window object, instead of allocating in each iteration. Allocation is carrying much bigger overhead than copy. The faster the \code{FUN} evaluates the more relative speedup we are getting, because allocation of a subset does not depend on how fast or slow \code{FUN} evaluates. See \emph{caveats} section for possible edge cases caused by this optimization.
    \item Parallel evaluation of \code{FUN} calls.\cr
    Until September 2025 all the multithreaded code in data.table was using \emph{OpenMP}. It can be used only in C language and it has very low overhead. Unfortunately it could not be applied in \code{frollapply} because to evaluate UDF from C code one has to call R's C api that is not thread safe (can be run only from single threaded C code). Therefore \code{frollapply} uses \code{\link[parallel]{parallel-package}}, which is included in base R, to provide parallelism at the R language level. It uses \emph{fork} parallelism, which has low overhead as well (unless results of computation are big in size which is not an issue for rolling statistics). \emph{Fork} is not available on Windows OS. See \emph{caveats} section for limitations caused by using this optimization.
  }
}
\note{
  Be aware that rolling functions operate on the physical order of input. If the intent is to roll values in a vector by a logical window, for example an hour, or a day, then one has to ensure that there are no gaps in the input, or use an adaptive rolling function to handle gaps, for which we provide helper function \code{\link{frolladapt}} to generate adaptive window size.
}
\examples{
frollapply(1:16, 4, median)
frollapply(1:9, 3, toString)

## vectorized input
x = list(1:10, 10:1)
n = c(3, 4)
frollapply(x, n, sum)
## give names
x = list(data1 = 1:10, data2 = 10:1)
n = c(small = 3, big = 4)
frollapply(x, n, sum, give.names=TRUE)

## by.column=FALSE
x = as.data.table(iris)
flow = function(x) {
  v1 = x[[1L]]
  v2 = x[[2L]]
  (v1[2L] - v1[1L] * (1+v2[2L])) / v1[1L]
}
x[, "flow" := frollapply(.(Sepal.Length, Sepal.Width), 2L, flow, by.column=FALSE),
  by = Species][]

## rolling regression: by.column=FALSE
f = function(x) coef(lm(v2 ~ v1, data=x))
x = data.table(v1=rnorm(120), v2=rnorm(120))
frollapply(x, 4, f, by.column=FALSE)
}
\seealso{
  \code{\link{froll}}, \code{\link{frolladapt}}, \code{\link{shift}}, \code{\link{data.table}}, \code{\link{setDTthreads}}
}
\keyword{ data }
