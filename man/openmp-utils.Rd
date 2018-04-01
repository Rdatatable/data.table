\name{setDTthreads}
\alias{setDTthreads}
\alias{getDTthreads}
\title{ Set or get number of threads that data.table should use }
\description{
Set and get number of threads to be used in \code{data.table} functions that are parallelized with OpenMP. Default value 0 means to utilize all CPU available with an appropriate number of threads calculated by OpenMP. \code{getDTthreads()} returns the number of threads that will be used. This affects \code{data.table} only and does not change R itself or other packages using OpenMP. The most common usage expected is \code{setDTthreads(1)} to limit \code{data.table} to one thread for pre-existing explicitly parallel user code; e.g. via packages parallel and foreach. Otherwise, nested parallelism may bite. As \code{data.table} becomes more parallel automatically internally, we expect explicit user parallelism to be needed less often.

Attempting to \code{setDTthreads()} to more than the number of logical CPUs is intended to be ineffective; i.e., \code{getDTthreads()} will still return the number of logical CPUs in that case. Further, there is a hard coded limit of 1024 threads (with warning when imposed) to prevent accidentally picking up the value of \code{INT_MAX} (2 billion; i.e. unlimited) from \code{omp_get_thread_limit()}. We have followed the advice of section 1.2.1.1 in the R-exts manual: "... or, better, for the regions in your code as part of their specification... num_threads(nthreads).. That way you only control your own code and not that of other OpenMP users."  All the parallel region in data.table contain this directive. This is mandated by a \code{grep} in the package's quality control release procedure script.
}
\usage{
setDTthreads(threads)
getDTthreads(verbose = getOption("datatable.verbose", FALSE))
}
\arguments{
  \item{threads}{ An integer >= 0. Default 0 means use all CPU available and leave the operating system to multi task. }
  \item{verbose}{ Display the value returned by some OpenMP function calls. }
}
\value{
A length 1 \code{integer}. The old value is returned by \code{setDTthreads} so you can store that value and pass it to \code{setDTthreads} again after the section of your code where you, probably, limited to one thread.
}
\keyword{ data }

