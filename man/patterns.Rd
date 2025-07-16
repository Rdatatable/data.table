\name{patterns}
\alias{patterns}
\title{Obtain matching indices corresponding to patterns}
\description{
\code{patterns} returns the elements of \code{cols}
that match the regular expression patterns, which must be
supported by \code{\link[base]{grep}}.

From \code{v1.9.6}, \code{\link{melt.data.table}} has an enhanced functionality
in which \code{measure.vars} argument can accept a \emph{list of column names}
and melt them into separate columns. See the \code{Efficient reshaping using
data.tables} vignette linked below to learn more.
}
\usage{
patterns(
  \dots, cols=character(0),
  ignore.case=FALSE, perl=FALSE,
  fixed=FALSE, useBytes=FALSE)
}
\arguments{
  \item{\dots}{A set of regular expression patterns.}
  \item{cols}{A character vector of names to which each pattern is matched.}
  \item{ignore.case, perl, fixed, useBytes}{Passed to \code{\link{grep}}.}
}
\seealso{
  \code{\link{melt}},
  \url{https://github.com/Rdatatable/data.table/wiki/Getting-started}
}
\examples{
DT = data.table(x1 = 1:5, x2 = 6:10, y1 = letters[1:5], y2 = letters[6:10])
# melt all columns that begin with 'x' & 'y', respectively, into separate columns
melt(DT, measure.vars = patterns("^x", "^y", cols=names(DT)))
# when used with melt, 'cols' is implicitly assumed to be names of input
# data.table, if not provided.
melt(DT, measure.vars = patterns("^x", "^y"))
}
\keyword{data}
