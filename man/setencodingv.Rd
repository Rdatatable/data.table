\name{setencodingv}
\alias{setencodingv}
\title{A "data.table-way" of changing the encoding}
\usage{
setencodingv(DT, cols = NULL, enc = localeToCharset(),
  src.enc = localeToCharset())
}
\arguments{
  \item{DT}{A data.table}

  \item{cols}{A character vector with the columns to be
  modified. If not set all character columns will be
  reencoded}

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
value. This takes a character vector of columns, use
setencoding to pass them as objects.
}
\examples{
DT<-data.table(col1=c("ä","a"),col2=c("ß","b"))
setencodingv(DT,c("col1","col2"),enc="latin1",src.enc="UTF-8")
}
\seealso{
\code{\link{setencoding}}
}

