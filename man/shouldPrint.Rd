\name{shouldPrint}
\alias{shouldPrint}
\title{ For use by packages that mimic/divert auto printing e.g. IRkernel and knitr }
\description{
  Not for use by users. Exported only for use by IRkernel (Jupyter) and knitr.
}
\usage{
  shouldPrint(x)
}
\arguments{
  \item{x}{ A \code{data.table}. }
}
\details{
  Should IRkernel/Jupyter print a data.table returned invisibly by DT[,:=] ?
  This is a read-once function since it resets an internal flag. If you need the value more than once in your logic, store the value from the first call.
}
\value{
  TRUE or FALSE.
}
\references{
  \url{https://github.com/IRkernel/IRkernel/issues/127}\cr
  \url{https://github.com/Rdatatable/data.table/issues/933}\cr
}
\examples{
# dummy example section to pass release check that all .Rd files have examples
}

