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
\method{duplicated}{data.table}(x, incomparables=FALSE, tolerance=.Machine$double.eps ^ 0.5, ...)

\method{unique}{data.table}(x, incomparables=FALSE, tolerance=.Machine$double.eps ^ 0.5, ...)
}
\arguments{
  \item{x}{ A data.table. }
  \item{\dots}{ Not used at this time. }
  \item{incomparables}{ Not used. Here for S3 method consistency. }
  \item{tolerance}{ Double precision values are considered equal if they are within this tolerance. Same default as \code{\link{all.equal}}.}
}
\details{
  Because data.tables are usually sorted by key, tests for duplication are
  especially quick. Unlike \code{\link[base]{unique.data.frame}}, \code{paste} is not
  used to ensure equality of floating point data. This is done directly (for speed)
  whilst still respecting tolerance in the same spirit as \code{\link{all.equal}}.

  When \code{x} has a key, only key columns are checked for duplication; non-key
  columns are not checked. When \code{x} has no key, all columns are checked.
}
\value{
     \code{duplicated} returns a logical vector of length \code{nrow(x)}
  indicating which rows are duplicates.

     \code{unique} returns a data table with duplicated rows removed.
}
\seealso{ \code{\link{data.table}}, \code{\link{duplicated}}, \code{\link{unique}}, \code{\link{all.equal}}}
\examples{
    DT <- data.table(A = rep(1:3, each=4), B = rep(1:4, each=3), C = rep(1:2, 6), key = "A,B")
    duplicated(DT)
    unique(DT)
    
    DT = data.table(a=c(2L,1L,2L), b=c(1L,2L,1L))   # no key
    unique(DT)                   # rows 1 and 2 (row 3 is a duplicate of row 1)
    
    DT = data.table(a=c(3.142, 4.2, 4.2, 3.142, 1.223, 1.223), b=rep(1,6))
    unique(DT)                   # rows 1,2 and 5
    
    DT = data.table(a=tan(pi*(1/4 + 1:10)), b=rep(1,10))   # example from ?all.equal
    length(unique(DT$a))         # 10 strictly unique floating point values
    all.equal(DT$a,rep(1,10))    # TRUE, all within tolerance of 1.0
    DT[,which.min(a)]            # row 10, the strictly smallest floating point value
    identical(unique(DT),DT[1])  # TRUE, stable within tolerance
    identical(unique(DT),DT[10]) # FALSE
}
\keyword{ data }


