\name{last}
\alias{last}
\title{ Last item of an object }
\description{
   Returns the last item of a vector or list, or the last row of a data.frame or data.table.
}
\usage{
last(x,...)
}
\arguments{
  \item{x}{ A vector, list, data.frame or data.table. Otherwise the S3 method of \code{xts::last} is deployed. }
  \item{...}{ Not applicable for \code{data.table::last}. Any arguments here are passed through to \code{xts::last}. }
}
% \details{
% }
\value{
    If no other arguments are supplied it depends on the type of x. The last item of a vector or list. The last row of a \code{data.frame} or \code{data.table}. Otherwise, whatever \code{xts::last} returns (if package xts has been loaded, otherwise a helpful error).
    If any argument is supplied in addition to \code{x} (such as \code{n} or \code{keep} in \code{xts::last}), regardless of \code{x}'s type, then \code{xts::last} is called if xts has been loaded, otherwise a helpful error.
}
\seealso{ \code{\link{NROW}}, \code{\link{head}}, \code{\link{tail}} }
% \examples{
% }
\keyword{ data }

