\name{setkey}
\alias{setkey}
\alias{setkeyv}
\alias{key}
\alias{key<-}
\alias{haskey}
\alias{copy}
\title{ Create key on a data table }
\description{
  Note that in \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column.. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.

  \code{setkey()} sorts a \code{data.table} and marks it as sorted (with an attribute \code{sorted}). The sorted columns are the key. The key can be any columns in any order. The columns are sorted in ascending order always. The table is changed \emph{by reference} and is therefore very memory efficient.
  
  \code{key()} returns the \code{data.table}'s key if it exists, and \code{NULL} if none exist.
  
  \code{haskey()} returns a logical \code{TRUE}/\code{FALSE} depending on whether the \code{data.table} has a key (or not).
  
  \code{copy()} copies an entire object.
}
\usage{
setkey(x, ..., verbose=getOption("datatable.verbose"))
setkeyv(x, cols, verbose=getOption("datatable.verbose"))
key(x)
haskey(x)
copy(x)
key(x) <- value   #  DEPRECATED, please use setkey or setkeyv instead.
}
\arguments{
  \item{x}{ A \code{data.table}. }
  \item{\dots}{ The columns to sort by. Do not quote the column names. If \code{\dots} is missing (i.e. \code{setkey(DT)}), all the columns are used. \code{NULL} removes the key. }
  \item{cols}{ A character vector (only) of column names. }
  \item{value}{ In (deprecated) \code{key<-}, a character vector (only) of column names.}
  \item{verbose}{ Output status and information. }
}
\details{
  In \code{data.table} versions \code{<= 1.8.10}, for columns of type \code{integer}, the sort is attempted with the very fast \code{"radix"} method in \code{\link[base]{sort.list}}. If that fails, the sort reverts to the default method in \code{\link[base]{order}}. For character vectors, \code{data.table} takes advantage of R's internal global string cache and implements a very efficient order, also exported as \code{\link{chorder}}.
  
  From \code{data.table} version \code{1.9}, for \code{integer} columns, a modified version of base's counting sort is implemented, which allows negative values as well. It is extremely fast, but is limited by the range of integer values being <= 1e5. If it fails, a 4-pass radix sort for integers, newly implemented based on Pierre Terdiman's and Michael Herf's code (see links below) is used. Similarly, a very fast 6-pass radix order for columns of type \code{double} is implemented (also adapted from Terdiman's and Herf's work). This gives a speed-up of about 5-8x compared to \code{1.8.10} on \code{setkey} and all internal \code{order}/\code{sort} operations. 

  Moreover, \code{data.table 1.9} also implements a better memory efficent way of ordering multiple columns, without compromising in speed.
  
  The sort is \emph{stable}; i.e., the order of ties (if any) is preserved, in both versions - \code{<=1.8.10} and \code{>= 1.9.0}.
  
  In v1.7.8, the \code{key<-} syntax was deprecated. The \code{<-} method copies the whole table and we know of no way to avoid that copy without a change in \R itself. Please use the \code{set}* functions instead, which make no copy at all. \code{setkey} accepts unquoted column names for convenience, whilst \code{setkeyv} accepts one vector of column names.
  
  The problem (for \code{data.table}) with the copy by \code{key<-} (other than being slower) is that \R doesn't maintain the over allocated truelength, but it looks as though it has. Adding a column by reference using \code{:=} after a \code{key<-} was therefore a memory overwrite and eventually a segfault; the over allocated memory wasn't really there after \code{key<-}'s copy. \code{data.table}s now have an attribute \code{.internal.selfref} to catch and warn about such copies. This attribute has been implemented in a way that is friendly with \code{identical()} and \code{object.size()}. 

  For the same reason, please use the other \code{set*} functions which modify objects by reference, rather than using the \code{<-} operator which results in copying the entire object. 
  
  It isn't good programming practice, in general, to use column numbers rather than names. This is why \code{setkey} and \code{setkeyv} only accept column names. If you use column numbers then bugs (possibly silent) can more easily creep into your code as time progresses if changes are made elsewhere in your code; e.g., if you add, remove or reorder columns in a few months time, a \code{setkey} by column number will then refer to a different column, possibly returning incorrect results with no warning. (A similar concept exists in SQL, where \code{"select * from ..."} is considered poor programming style when a robust, maintainable system is required.)  If you really wish to use column numbers, it's possible but deliberately a little harder; e.g., \code{setkeyv(DT,colnames(DT)[1:2])}.
  }
\value{
    The input is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setkey(DT,a)[J("foo")]}. If you require a copy, take a copy first (using \code{DT2=copy(DT)}). \code{copy()} may also sometimes be useful before \code{:=} is used to subassign to a column by reference. See \code{?copy}.
}
\references{
\url{http://en.wikipedia.org/wiki/Radix_sort}\cr
\url{http://en.wikipedia.org/wiki/Counting_sort}\cr
\url{http://cran.at.r-project.org/web/packages/bit/index.html}\cr
\url{http://stereopsis.com/radix.html}
}
\note{ Despite its name, \code{base::sort.list(x,method="radix")} actually invokes a \emph{counting sort} in R, not a radix sort. See do_radixsort in src/main/sort.c. A counting sort, however, is particularly suitable for sorting integers and factors, and we like it. In fact we like it so much that \code{data.table} contains a counting sort algorithm for character vectors using R's internal global string cache. This is particularly fast for character vectors containing many duplicates, such as grouped data in a key column. This means that character is often preferred to factor. Factors are still fully supported, in particular ordered factors (where the levels are not in alphabetic order).
}
\seealso{ \code{\link{data.table}}, \code{\link{tables}}, \code{\link{J}}, \code{\link[base]{sort.list}}, \code{\link{copy}}, \code{\link{setDT}}, \code{\link{set}} \code{\link{:=}}, \code{\link{setorder}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{chorder}}
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
    setkeyv(DT,keycols)   # rather than key(DT)<-keycols (which copies entire table)
    
    DT = data.table(A=5:1,B=letters[5:1])
    DT2 = DT              # does not copy
    setkey(DT2,B)         # does not copy-on-write to DT2
    identical(DT,DT2)     # TRUE. DT and DT2 are two names for the same keyed table
    
    DT = data.table(A=5:1,B=letters[5:1])
    DT2 = copy(DT)        # explicit copy() needed to copy a data.table
    setkey(DT2,B)         # now just changes DT2
    identical(DT,DT2)     # FALSE. DT and DT2 are now different tables
}
\keyword{ data }

