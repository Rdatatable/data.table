\name{rowwiseDT}
\alias{rowwiseDT}
\title{ Create a data.table rowwisely }
\description{
  \code{rowwiseDT} allows the user to create a \code{data.table} object by specifying a row-by-row layout. It's convinient and highly readable when the table is small.
}
\usage{
rowwiseDT(...)
}
\arguments{
  \item{...}{ Arguments that defines the structure of a \code{data.table}. The column names must be specified as \code{col=} and may only appear before the data. See the examples section for details. }
}
\details{
If the length of any value is not one (i.e., not scalar), the whole column will be converted to a list column.
}
\value{
A \code{data.table} object.
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
