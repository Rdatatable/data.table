\name{patterns}
\alias{patterns}
\title{Obtain matching indices corresponding to patterns}
\description{
\code{patterns} returns the matching indices in the argument \code{cols}
corresponding to the regular expression patterns provided. The patterns must be
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
  \item{cols}{A character vector of column names to match with
    patterns. When used in the context of
    \code{melt(measure.vars=patterns())} or
    \code{DT[, .SDcols=patterns()]}, the
    user should not provide the \code{cols} argument, which is
    automatically set to all of the column names of the input data
    table, using non-standard evaluation.
  }
  \item{ignore.case, perl, fixed, useBytes}{Passed to \code{\link{grep}}.}
}
\seealso{
  \code{\link{melt}}, \code{\link{.SD}},
  \url{https://github.com/Rdatatable/data.table/wiki/Getting-started},
  [`vignette("datatable-reshape")`](https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html),
  [`vignette("datatable-sd-usage")`](https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-sd-usage.html)
}
\examples{
DT = data.table(x1 = 1:5, x2 = 6:10, y1 = letters[1:5], y2 = letters[6:10])
# melt all columns that begin with 'x' & 'y', respectively, into separate columns
melt(DT, measure.vars = patterns(x="^x", y="^y"))
# summarize all columns that contain x
DT[, lapply(.SD, sum), .SDcols=patterns("x")]
}
\keyword{data}
