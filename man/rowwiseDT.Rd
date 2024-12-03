\name{rowwiseDT}
\alias{rowwiseDT}
\title{ Create a data.table row-wise }
\description{
  \code{rowwiseDT} creates a \code{data.table} object by specifying a row-by-row layout. This is convenient and highly readable for small tables.
}
\usage{
rowwiseDT(...)
}
\arguments{
  \item{...}{ Arguments that define the structure of a \code{data.table}. The column names come from named arguments (like \code{col=}), which must precede the data. See Examples. }
}
\value{
A \code{data.table}. The default is for each column to return as a vector. However, if any entry has a length that is not one (e.g., \code{list(1, 2)}), the whole column will be converted to a list column.
}
\seealso{
  \code{\link{data.table}}
}
\examples{
rowwiseDT(
  A=,B=, C=,
  1, "a",2:3,
  2, "b",list(5)
)
}
