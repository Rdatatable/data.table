\name{patterns}
\alias{patterns}
\title{Regex patterns to extract columns from data.table}
\description{
  From \code{v1.9.6}, \code{\link{melt.data.table}} has a new enhanced functionality in which \code{measure.vars} argument can accept a \emph{list of column names} and melt them into separate columns. See the \code{Efficient reshaping using data.tables} vignette linked below to learn more.

  \code{patterns} is designed purely for convenience, to be used only within the \code{measure.vars} argument of \code{melt.data.table}. Column names corresponding to each pattern from the \code{data.table} is melted into a separate column.
}
\usage{
patterns(...)
}
\arguments{
  \item{...}{ A set of patterns. See example. }
}
\seealso{ 
  \code{\link{melt}}, \url{https://github.com/Rdatatable/data.table/wiki/Getting-started}
}
\examples{
# makes sense only in the context of melt at the moment
dt = data.table(x1 = 1:5, x2 = 6:10, y1 = letters[1:5], y2 = letters[6:10])
# melt all columns that begin with 'x' & 'y', respectively, into separate columns
melt(dt, measure.vars = patterns("^x", "^y"))
}
\keyword{ data }
