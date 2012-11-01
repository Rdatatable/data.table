\name{last}
\alias{last}
\title{ Last item of an object }
\description{
   Returns last item of a vector, list or data.table.
}
\usage{
last(x,...)
}
\arguments{
  \item{x}{ A vector, list or data.table. Otherwise S3 method is dispatched, for compatibility with \code{xts::last}. }
  \item{...}{ If any arguments other than \code{x} are supplied, such as \code{n} or \code{keep} regardless of \code{x}'s type, then S3 dispatch is deployed. }
}
% \details{
% }
\value{
    The last item of a vector or list. If \code{x} is a \code{data.table}, the last row as a one row \code{data.table}. Otherwise, whatever \code{xts::last} returns.
}
\seealso{ \code{\link{NROW}} }
% \examples{
% }
\keyword{ data }

