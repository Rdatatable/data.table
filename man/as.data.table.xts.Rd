\name{as.data.table.xts}
\alias{as.data.table.xts}
\title{Efficient xts to as.data.table conversion}
\description{
  Efficient conversion xts to data.table.
}
\usage{
\method{as.data.table}{xts}(x, keep.rownames = TRUE, key=NULL, \dots)
}
\arguments{
\item{x}{xts to convert to data.table}
\item{keep.rownames}{Default is \code{TRUE}. If \code{TRUE}, adds the xts input's index as a separate column named \code{"index"}. \code{keep.rownames = "id"} names the index column \code{"id"} instead.}
\item{key}{ Character vector of one or more column names which is passed to \code{\link{setkeyv}}. }
\item{\dots}{ignored, just for consistency with \code{as.data.table}}
}
\seealso{ \code{\link{as.xts.data.table}} }
\examples{
if (requireNamespace("xts", quietly = TRUE)) {
  data(sample_matrix, package = "xts")
  sample.xts <- xts::as.xts(sample_matrix) # xts might not be attached on search path
  # print head of xts
  print(head(sample.xts))
  # print data.table
  print(as.data.table(sample.xts))
}
}
