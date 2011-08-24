\name{setkey}
\alias{setkey}
\alias{key}
\alias{key<-}
\alias{haskey}
\alias{copy}
\title{ Create key on a data table }
\description{
  Sorts a data.table and marks it as sorted. The sorted columns are the key. The key can be any columns in any order. The columns are sorted in ascending order always.
}
\usage{
setkey(x, ..., loc=parent.frame(),verbose=getOption("datatable.verbose",FALSE))
key(x)
key(x) <- value
haskey(x)
copy(x)
}
\arguments{
  \item{x}{ An unquoted name of a \code{data.table}. }
  \item{\dots}{ The columns to sort by. Do not quote the column names. If \code{\dots} is missing all the columns are used. }
  \item{value}{ A character vector of column names. }
  \item{loc}{ The \code{data.table} must already exist in this frame and is sorted by reference in this frame. \code{loc=.GlobalEnv} is often useful within functions. }
  \item{verbose}{ Output status and information. }
}
\details{
  The sort is attempted with the very fast \code{"radix"} method in
  \code{\link[base]{sort.list}}. If that fails, the sort reverts to the default
  method in \code{\link[base]{order}}. That logic is repeated column by column.
  
  The sort is \emph{stable}; i.e., the order of ties (if any) is preserved.
  
  If \code{value=NULL}, the key is removed.
  }
\value{
    No value is returned. The data.table is modified by reference. If you require a copy, take a copy first (using \code{DT2=copy(DT)}). \code{copy()} may also sometimes be useful before \code{:=} is used to subassign to a column by reference.
}
\references{
\url{http://en.wikipedia.org/wiki/Radix_sort}\cr
\url{http://en.wikipedia.org/wiki/Counting_sort}
}
\note{ \code{base::sort.list(x,method="radix")} actually invokes a \emph{counting sort}, not a radix sort. See do_radixsort in src/main/sort.c. A counting sort is particularly suitable for sorting integers and factors, however, and we like it. Anyway, this is one reason data.table 'likes' integers and factors.
}
\seealso{ \code{\link{data.table}}, \code{\link{tables}}, \code{\link{J}}, \code{\link[base]{sort.list}} 
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{
    DT = data.table(A=5:1,B=letters[5:1])
    DT # before
    setkey(DT,B)  # re-orders table and marks it sorted.
    DT # after
    tables()      # KEY column reports the key'd columns
    key(DT)
    key(DT) = "A"
    
    DT = data.table(A=5:1,B=letters[5:1])
    DT2 = DT              # not enough to copy
    setkey(DT2,B)         # does not copy on write to DT2
    identical(DT,DT2)     # TRUE. DT and DT2 are two names for the same keyed table
    
    DT = data.table(A=5:1,B=letters[5:1])
    DT2 = copy(DT)        # explicit copy is required for data.table
    setkey(DT2,B)         # just changes DT2
    identical(DT,DT2)     # FALSE. DT and DT2 are now different tables
}
\keyword{ data }

