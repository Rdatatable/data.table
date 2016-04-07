\name{fwrite}
\alias{fwrite}
\title{Fast CSV writer}
\description{
  Similar to \code{write.table} but faster and more limited in features.
}
\usage{
fwrite(x, file.path, append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "",
       col.names = TRUE, qmethod = "double", block.size = 10000)
}
\arguments{
  \item{x}{The \code{data.table} or \code{data.frame} to write}
  \item{file.path}{Output file name}
  \item{append}{If \code{TRUE}, the file is opened in append mode and column names (header row) are not written.}
  \item{quote}{If \code{TRUE}, all columns of character and factor types, as well as all column names, will be surrounded by double quotes. If \code{FALSE}, nothing is quoted, even if this would break the CSV (the column contents are not checked for separator characters).}
  \item{sep}{The separator between columns}
  \item{eol}{Line separator}
  \item{na}{The string to use for missing values in the data}
  \item{col.names}{A logical value indicating if the column names (header row) should be written}
  \item{qmethod}{A character string specifying how to deal with embedded double quote characters when quoting strings. Must be one of "escape", in which case the quote character (as well as the backslash character) is escaped in C style by a backslash, or "double" (default), in which case it is doubled.}
  \item{block.size}{The output is written in blocks, each of which contains at most this number of rows. This is to avoid making large copies in memory. Can be used to tweak performance and memory usage.}
}
\details{
The speed-up compared to \code{write.csv} depends on the parameters and column types.
}
\seealso{ \code{\link[utils]{write.csv}} }
\examples{
\dontrun{

fwrite(data.table(first=c(1,2), second=c(NA, 'foo"bar')), "table.csv")

# table.csv contains:

# "first","second"
# "1",""
# "2","foo""bar"
}
}
\keyword{ data }

