\name{setencoding}
\alias{setencoding}
\title{A "data.table-way" of changing the encoding}
\usage{
setencoding(DT, ..., enc = localeToCharset(), src.enc = localeToCharset())
}
\arguments{
  \item{DT}{A data.table}

  \item{...}{The columns to be reencoded}

  \item{enc}{A character string with the desired encoding
  (only 1!). Something iconv can understand ("UTF-8",
  "latin1", etc.)}

  \item{src.enc}{The current encoding of entries with
  Encoding() "unknown". If the text displays correctly it
  is in the current locale, the default value.}
}
\value{
Not needed. The column is modified in place.
}
\description{
This function allows you to change the encoding of columns
of a data.table to a specified encoding even if they
currently use a mix of different encodings (even within a
single column).
}
\details{
It modifies the input data.table, you don't need the return
value. This takes the columns as object names, use
setencodingv to pass them as character vector.
}
\examples{
DT<-data.table(col1=c("ä","a"),col2=c("ß","b"))
setencoding(DT,col1,col2,enc="UTF-8",src.enc="latin1")
setencoding(DT,enc="latin1")
}
\author{Stefan Fritsch \email{fritsch@stat-up.com}}
\seealso{
\code{\link{setencodingv}}, \code{\link{setkey}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{setorder}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setDT}}, \code{\link{setDF}}, \code{\link{copy}}
}


