\name{cbindlist}
\alias{cbindlist}
\alias{cbind}
\alias{cbind.data.table}
\title{Column bind multiple data.tables}
\description{
  Column bind multiple \code{data.table}s.
}
\usage{
  cbindlist(l, copy=TRUE)
}
\arguments{
  \item{l}{ \code{list} of \code{data.table}s to merge. }
  \item{copy}{ \code{logical}, decides if columns has to be copied into resulting object (default) or just referred. }
}
\details{
  Column bind only stacks input elements. Works like \code{\link{data.table}}, but takes \code{list} type on input. Zero-column tables in \code{l} are omitted. Tables in \code{l} should have matching row count; recycling of length-1 rows is not yet implemented. Indices of the input tables are transferred to the resulting table, as well as the \emph{key} of the first keyed table.
}
\value{
  A new \code{data.table} based on the stacked objects. Eventually when \code{copy} is \code{FALSE}, then resulting object will share columns with \code{l} tables.
}
\note{
  If output object has any duplicate names, then key and indices are removed.
}
\seealso{
  \code{\link{data.table}}, \code{\link{rbindlist}}
}
\examples{
l = list(
  d1 = data.table(x=1:3, v1=1L),
  d2 = data.table(y=3:1, v2=2L),
  d3 = data.table(z=2:4, v3=3L)
)
cbindlist(l)
}
\keyword{ data }
