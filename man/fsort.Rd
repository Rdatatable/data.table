\name{fsort}
\alias{fsort}
\title{Fast parallel sort}
\description{
  Similar to \code{base::sort} but fast using parallelism. Experimental.
}

\usage{
fsort(x, decreasing = FALSE, na.last = FALSE, internal=FALSE, verbose=FALSE, \dots)
}
\arguments{
  \item{x}{ A vector. Type double, currently. }
  \item{decreasing}{ Decreasing order? }
  \item{na.last}{ Control treatment of \code{NA}s. If \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they are put first; if \code{NA}, they are removed; if \code{"keep"} they are kept with rank \code{NA}. }
  \item{internal}{ Internal use only. Temporary variable. Will be removed. }
  \item{verbose}{ Print tracing information. }
  \item{\dots}{ Not sure yet. Should be consistent with base R.}
}
\details{
  Process will raise error if \code{x} contains negative values.
  Unless \code{x} is already sorted \code{fsort} will redirect processing to slower single threaded \emph{order} followed by \emph{subset} in following cases:
  \itemize{
    \item data type other than \emph{double} (\emph{numeric})
    \item data having \code{NA}s
    \item \code{decreasing==FALSE}
  }
}
\value{
  The input in sorted order.
}

\examples{
x = runif(1e6)
system.time(ans1 <- sort(x, method="quick"))
system.time(ans2 <- fsort(x))
identical(ans1, ans2)
}

