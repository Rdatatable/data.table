\name{setDTthreads}
\alias{setDTthreads}
\alias{getDTthreads}
\title{ Set or get number of threads that data.table should use }
\description{
  Set and get number of threads to be used in \code{data.table} functions that are parallelized with OpenMP.
}
\usage{
  setDTthreads(threads = 0, restore_after_fork = NULL)
  getDTthreads(verbose = getOption("datatable.verbose", FALSE))
}
\arguments{
  \item{threads}{ An integer >= 0. Default and recommended 0 means to use half of the logical CPUs available so as to leave plenty of resources for other processes. }
  \item{restore_after_fork}{ Should data.table be multi-threaded after a fork has completed? NULL leaves the current setting unchanged which by default is TRUE. See details below. }
  \item{verbose}{ Display the value of relevant OpenMP settings plus the \code{restore_after_fork} internal option. }
}
\value{
  A length 1 \code{integer}. The old value is returned by \code{setDTthreads} so you can store that prior value and pass it to \code{setDTthreads()} again after the section of your code where you control the number of threads.
}
\details{
  \code{data.table} automatically switches to single threaded mode upon fork (the mechanism used by \code{\link[parallel]{mclapply}} and the foreach package). Otherwise, nested parallelism would very likely overload your CPUs and result in much slower execution. As \code{data.table} becomes more parallel internally, we expect explicit user parallelism to be needed less often. The \code{restore_after_fork} option controls what happens after the explicit fork parallelism completes. It needs to be at C level so it is not a regular R option using \code{options()}. By default \code{data.table} will be multi-threaded again; restoring the prior setting of \code{getDTthreads()}. But problems have been reported in the past on Mac and Intel OpenMP libraries, whereas success has been reported on Linux. If you experience problems after fork, start a new R session and change the default behaviour by calling \code{setDTthreads(restore_after_fork=FALSE)} before retrying. Please raise issues on the data.table GitHub issues page.

  Attempting to \code{setDTthreads()} to more than the number of logical CPUs is intended to be ineffective; i.e., \code{getDTthreads()} will still return the number of logical CPUs in that case. The number of logical CPUs is determined by the OpenMP function \code{omp_get_num_procs()}.
  
  \code{setDTthreads()} affects \code{data.table} only and does not change R itself or other packages using OpenMP. We have followed the advice of section 1.2.1.1 in the R-exts manual: "\ldots or, better, for the regions in your code as part of their specification\ldots num_threads(nthreads)\ldots That way you only control your own code and not that of other OpenMP users."  All the parallel regions in data.table contain this \code{num_threads(getDTthreads())} directive. This is mandated by a \code{grep} in data.table's quality control CRAN release procedure script.
}
\keyword{ data }

