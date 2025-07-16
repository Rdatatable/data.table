\name{cbindlist}
\alias{cbindlist}
\alias{setcbindlist}
\alias{cbind}
\alias{cbind.data.table}
\title{Column bind multiple data.tables}
\description{
  Column bind multiple \code{data.table}s.
}
\usage{
  cbindlist(l)
  setcbindlist(l)
}
\arguments{
  \item{l}{ \code{list} of \code{data.table}s to merge. }
}
\details{
  Column bind only stacks input elements. Works like \code{\link{data.table}}, but takes \code{list} type on input. Zero-column tables in \code{l} are omitted. Tables in \code{l} should have matching row count; recycling of length-1 rows is not yet implemented. Indices of the input tables are transferred to the resulting table, as well as the \emph{key} of the first keyed table.
}
\value{
  A new \code{data.table} based on the stacked objects.
  
  For \code{setcbindlist}, columns in the output will be shared with the input, i.e., \emph{no copy is made}.
}
\note{
  No attempt is made to deduplicate resulting names. If the result has any duplicate names, keys and indices are removed.
}
\seealso{
  \code{\link{data.table}}, \code{\link{rbindlist}}, \code{\link{setDT}}
}
\examples{
d1 = data.table(x=1:3, v1=1L, key="x")
d2 = data.table(y=3:1, v2=2L, key="y")
d3 = data.table(z=2:4, v3=3L)
cbindlist(list(d1, d2, d3))
cbindlist(list(d1, d1))
d4 = setcbindlist(list(d1))
d4[, v1:=2L]
identical(d4, d1)
}
\keyword{ data }
