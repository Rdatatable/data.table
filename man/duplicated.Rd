\name{duplicated}
\alias{duplicated.data.table}
\alias{unique.data.table}
\title{ Determine Duplicate Rows }
\description{
     \code{duplicated} returns a logical vector indicating which rows of a \code{data.table}
     have duplicate rows (by key).

     \code{unique} returns a data table with duplicated rows (by key) removed, or
     (when no key) duplicated rows by all columns removed.
}
\usage{
\method{duplicated}{data.table}(x, ...)

\method{unique}{data.table}(x, ...)
}
\arguments{
  \item{x}{ A data.table. }
  \item{\dots}{ Not used at this time. }
}
\details{
  Because data.tables are usually sorted by key, tests for duplication are
  relatively quick.

  When \code{x} has a key, only key columns are checked for duplication; non-key
  columns are not checked. 
}
\value{
     \code{duplicated} returns a logical vector of length \code{nrow(x)}
  indicating which rows are duplicates by key.

     \code{unique} returns a data table with duplicated rows removed.
}
\seealso{ \code{\link{data.table}}, \code{\link{duplicated}}, \code{\link{unique}}}
\examples{
    (dt <- data.table(A = rep(1:3, each=4), B = rep(1:4, each=3), C = rep(1:2, 6), key = "A,B"))
    duplicated(dt)
    unique(dt)
    
    dt = data.table(a=c(2,1,2),b=c(1,2,1))   # no key
    unique(dt)       
}
\keyword{ data }


