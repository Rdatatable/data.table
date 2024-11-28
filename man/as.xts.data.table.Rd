\name{as.xts.data.table}
\alias{as.xts.data.table}
\title{Efficient data.table to xts conversion}
\description{
  Efficient conversion of data.table to xts, data.table must have a time based type in first column.  See ?xts::timeBased for supported types
}
\usage{
as.xts.data.table(x, numeric.only = TRUE, \dots)
}
\arguments{
\item{x}{data.table to convert to xts, must have a time based first column. As xts objects are indexed matrixes, all columns must be of the same type. If columns of multiple types are selected, standard as.matrix rules are applied during the conversion. }
\item{numeric.only}{If TRUE, only include numeric columns in the conversion and all non-numeric columns will be omitted with warning}
\item{\dots}{ignored, just for consistency with generic method.}
}
\seealso{ \code{\link{as.data.table.xts}} }
\examples{
if (requireNamespace("xts", quietly = TRUE)) {
  sample.dt <- data.table(date = as.Date((Sys.Date()-999):Sys.Date(),origin="1970-01-01"),
                          quantity = sample(10:50,1000,TRUE),
                          value = sample(100:1000,1000,TRUE))
  # print data.table
  print(sample.dt)
  # print head of xts
  print(head(as.xts.data.table(sample.dt))) # xts might not be attached on search path
}
}
