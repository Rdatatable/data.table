\name{setDTthreads}
\alias{setDTthreads}
\alias{getDTthreads}
\title{ Set or get number of threads that data.table should use }
\description{
Set and get number of threads to be used in \code{data.table} functions that are parallelized with OpenMP. Default value 0 means to utilize all CPU available with an appropriate number of threads calculated by OpenMP. \code{getDTthreads()} returns the number of threads that will be used. This affects \code{data.table} only and does not change R itself or other packages using OpenMP. The most common usage expected is \code{setDTthreads(1)} to limit \code{data.table} to one thread for pre-existing explictly parallel user code; e.g. via packages parallel and foreach. Otherwise, nested parallelism may bite. As \code{data.table} becomes more parallel automatically internally, we expect explicit user parallelism to be needed less often.
}
\usage{
setDTthreads(threads)
getDTthreads()
}
\arguments{
  \item{threads}{ An integer >= 0. Default 0 means use all CPU available and leave the operating system to multi task. }
}
\value{
A length 1 \code{integer}. The old value is returned by \code{setDTthreads} so you can store that value and pass it to \code{setDTthreads} again after the section of your code where you, probably, limited to one thread.
}
\keyword{ data }

