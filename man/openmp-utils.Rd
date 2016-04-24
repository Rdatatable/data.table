\name{setthreads}
\alias{setthreads}
\alias{getthreads}
\title{ Set or get threads for use with OpenMP}
\description{
Set (and get) number of threads to be used in \code{data.table} functions that are parallelised with OpenMP. Default is to use all threads.
}
\usage{
setthreads(threads)
getthreads()
}
\arguments{
  \item{threads}{ A positive integer length-1 value. }
}
\value{
\code{NULL} in case of \code{setthreads} and a length-1 \code{integer} in case of \code{getthreads}.
}
\keyword{ data }
