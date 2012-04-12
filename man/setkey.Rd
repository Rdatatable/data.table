\name{setkey}
\alias{setkey}
\alias{setkeyv}
\alias{key}
\alias{key<-}
\alias{haskey}
\alias{copy}
\alias{setattr}
\alias{setnames}
\alias{setcolorder}
\alias{set}
\title{ Create key on a data table }
\description{
  Sorts a data.table and marks it as sorted. The sorted columns are the key. The key can be any columns in any order. The columns are sorted in ascending order always. The table is changed \emph{by reference}. No copy is made at all, other than temporary working memory as large as one column.
}
\usage{
setkey(x, ..., verbose=getOption("datatable.verbose"))
setkeyv(x, cols, verbose=getOption("datatable.verbose"))
key(x)
haskey(x)
copy(x)
setattr(x,name,value)
setnames(x,old,new)
setcolorder(x,neworder)
set(x,i=NULL,j,value)   # should be documented in ?":=", perhaps.
key(x) <- value   # deprecated, please use setkey or setkeyv instead.
}
\arguments{
  \item{x}{ A \code{data.table}. }
  \item{\dots}{ The columns to sort by. Do not quote the column names. If \code{\dots} is missing (i.e. \code{setkey(DT)}), all the columns are used. }
  \item{cols}{ A character vector (only) of column names. }
  \item{value}{ In (deprecated) \code{key<-}, a character vector (only) of column names. In \code{setattr}, the value to assign to the attribute or \code{NULL} removes the attribute, if present. }
  \item{name}{ The character attribute name. }
  \item{verbose}{ Output status and information. }
  \item{old}{ When \code{new} is provided, character names or numeric positions of column names to change. When \code{new} is not provided, the new column names, which must be the same length as the number of columns. See examples. }
  \item{new}{ Optional. New column names, the same length as \code{old}. } 
  \item{neworder}{ Character vector of the new column name ordering. May also be column numbers. }
  \item{i}{ Integer row numbers to be assigned \code{value}. }
  \item{j}{ Integer column number to be assigned \code{value}. }
}
\details{
  The sort is attempted with the very fast \code{"radix"} method in
  \code{\link[base]{sort.list}}. If that fails, the sort reverts to the default
  method in \code{\link[base]{order}}. That logic is repeated column by column.
  
  The sort is \emph{stable}; i.e., the order of ties (if any) is preserved.
  
  If \code{v=NULL}, the key is removed.
  
  In v1.7.8, the \code{key<-} syntax was deprecated. The \code{<-} method copies the whole table and we know of no way to avoid that copy without a change in \R itself. Please use the \code{set}* functions instead, which make no copy at all. \code{setkey} accepts unquoted column names for convenience, whilst \code{setkeyv} accepts one vector of column names.
  
  The problem (for \code{data.table}) with the copy by \code{key<-} (other than being slower) is that \R doesn't maintain the over allocated truelength, but it looks as though it has. Adding a column by reference using \code{:=} after a \code{key<-} was therefore a memory overwrite and eventually a seg fault; the over allocated memory wasn't really there after \code{key<-}'s copy. \code{data.table}s have a new attribute \code{.internal.selfref} to catch and warn about such copies in future. This attribute has been implemented in way that is friendly with \code{identical()} and \code{object.size()}. 

  For the same reason, please use \code{setattr()} rather than \code{attr(x,name)<-value}, \code{setnames()} rather than \code{names(x)<-value} or \code{colnames(x)<-value}, and \code{setcolorder()} rather than \code{DT<-DT[,neworder,with=FALSE]}.
  
  It isn't good programming practice, in general, to use column numbers rather than names. This is why \code{setkey} and \code{setkeyv} only accept column names, and why \code{old} in \code{setnames()} is recommended to be names. If you use column numbers then bugs (possibly silent) can more easily creep into your code as time progresses if changes are made elsewhere in your code; e.g., if you add, remove or reorder columns in a few months time, a \code{setkey} by column number will then refer to a different column, possibly returning incorrect results with no warning. (A similar concept exists in SQL, where \code{"select * from ..."} is considered poor programming style when a robust, maintainable system is required.)  If you wish to use column numbers, it's possible but a little harder; e.g., \code{setkeyv(DT,colnames(DT)[1:2])}.
  
  }
\value{
    The \code{data.table} is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setkey(DT,a)[J("foo")]}. If you require a copy, take a copy first (using \code{DT2=copy(DT)}). \code{copy()} may also sometimes be useful before \code{:=} is used to subassign to a column by reference. See \code{?copy}.
}
\references{
\url{http://en.wikipedia.org/wiki/Radix_sort}\cr
\url{http://en.wikipedia.org/wiki/Counting_sort}
}
\note{ \code{base::sort.list(x,method="radix")} actually invokes a \emph{counting sort}, not a radix sort. See do_radixsort in src/main/sort.c. A counting sort, however, is particularly suitable for sorting integers and factors, and we like it. Anyway, this is one reason data.table 'likes' integers and factors.
}
\seealso{ \code{\link{data.table}}, \code{\link{tables}}, \code{\link{J}}, \code{\link[base]{sort.list}}, \code{\link{copy}}, \code{\link{:=}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{
    # Type 'example(setkey)' to run these at prompt and browse output
    
    DT = data.table(A=5:1,B=letters[5:1])
    DT # before
    setkey(DT,B)          # re-orders table and marks it sorted.
    DT # after
    tables()              # KEY column reports the key'd columns
    key(DT)
    keycols = c("A","B")
    setkeyv(DT,keycols)    # rather than key(DT)<-keycols (which copies entire table)
    
    DT = data.table(A=5:1,B=letters[5:1])
    DT2 = DT              # does not copy
    setkey(DT2,B)         # does not copy-on-write to DT2
    identical(DT,DT2)     # TRUE. DT and DT2 are two names for the same keyed table
    
    DT = data.table(A=5:1,B=letters[5:1])
    DT2 = copy(DT)        # explicit copy() needed to copy a data.table
    setkey(DT2,B)         # now just changes DT2
    identical(DT,DT2)     # FALSE. DT and DT2 are now different tables
    
    DF = data.frame(a=1:2,b=3:4)       # base data.frame to demo copies
    try(tracemem(DF))                  # try() for non-Windows where R is faster without memory profiling
    colnames(DF)[1] <- "A"             # 4 copies of entire object
    names(DF)[1] <- "A"                # 3 copies of entire object
    `names<-`(DF,c("A","b"))           # 1 copy of entire object
    x=`names<-`(DF,c("A","b"))         # still 1 copy (so not print method)
    # What if DF is large, say 10GB in RAM. Copy 10GB just to change a column name?

    DT = data.table(a=1:2,b=3:4,c=5:6)
    try(tracemem(DT))
    setnames(DT,"b","B")               # by name; no match() needed
    setnames(DT,3,"C")                 # by position
    setnames(DT,2:3,c("D","E"))        # multiple
    setnames(DT,c("a","E"),c("A","F")) # multiple by name
    setnames(DT,c("X","Y","Z"))        # replace all
    # And, no copy of DT was made by setnames() at all

}
\keyword{ data }

