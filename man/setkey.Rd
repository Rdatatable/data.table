\name{setkey}
\alias{setkey}
\alias{key}
\alias{key<-}
\title{ Create key on a data table }
\description{
  Sorts a data.table and marks it as sorted. The sorted columns are the key. The key can be any columns in any order. The columns are sorted in ascending order always.
}
\usage{
setkey(x, ..., loc=parent.frame())
key(x)
key(x) <- value
}
\arguments{
  \item{x}{ An unquoted name of a data.table. }
  \item{\dots}{ The columns to sort by. Do not quote the column names. If \dots is missing all the columns are used. }
  \item{value}{ A character vector of column names. }
  \item{loc}{ The data.table must already exist in this frame, and it is sorted by reference in this frame. loc=.GlobalEnv is often useful within functions. }
}
\details{
  The sort is attempted with the very fast radix method in
  \code{sort.list}. If that fails, the sort reverts to the default
  method in \code{order}. That logic is repeated column by column.
  
  If value is NULL, the key is removed.
  }
\value{
    No value is returned. The data.table is modified by reference. If you require a copy, take a copy first. A working copy is currently taken internally.
}
% \references{ }
\author{ Matthew Dowle }
% \note{ }
\seealso{ \code{\link{data.table}}, \code{\link{tables}}, \code{\link{[.data.table}}, \code{\link{J}} }
\examples{
    DT = data.table(A=5:1,B=letters[5:1])
    DT # before
    setkey(DT,B)  # re-orders table and marks it sorted.
    DT # after
    tables()      # KEY column reports the key'd columns
    key(DT)
    key(DT) = "A"
}
\keyword{ data }

