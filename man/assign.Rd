\name{:=}
\alias{:=}
\title{ Assignment by reference }
\description{
    Fast add, remove and modify subsets of columns, by reference.
}
\usage{
    LHS := RHS         # in j only i.e. DT[i,LHS:=RHS]
}
\arguments{
\item{LHS}{ A single column name. Or, when \code{with=FALSE}, a vector of column names or numeric positions (or a variable that evaluates as such). If the column doesn't exist, it is added, by reference. }
\item{RHS}{ A vector of replacement values. It is recycled in the usual way to fill the number of rows satisfying \code{i}, if any. Or, when \code{with=FALSE}, a \code{list} of replacement vectors which are applied (the \code{list} is recycled if necessary) to each column of \code{LHS} . To remove a column use \code{NULL}. }
}
\details{
\code{:=} is defined for use in \code{j} only. This syntax \emph{updates} the column(s) by reference. It makes no copies of any part of memory at all. Typical usages are :
\preformatted{
    DT[i,colname:=value]
    DT[i,"colname":=value,with=FALSE]
    DT[i,(3:6):=value,with=FALSE]
    DT[i,colnamevector:=value,with=FALSE]
}
The following all result in a friendly error (by design) :
\preformatted{
    x := 1L                                   # friendly error
    DT[i,colname] := value                    # friendly error
    DT[i]$colname := value                    # friendly error
}

\code{:=} in \code{j} can be combined with all types of \code{i}, such as binary search.\cr\cr  % for some reason in this .Rd file (but not the others), newlines seem to be ignored.

When the LHS is a factor column and the RHS is a character vector with items missing from the factor levels, the new level(s) are automatically added (by reference, efficiently), unlike base methods.\cr\cr

Unlike \code{<-} for \code{data.frame}, the (potentially large) LHS is not coerced to match the type of the (often small) RHS. Instead the RHS is coerced to match the type of the LHS, if necessary. Where this involves double precision values being coerced to an integer column, a warning is given (whether or not fractional data is truncated). The motivation for this is efficiency. It is best to get the column types correct up front and stick to them. Changing a column type is possible but deliberately harder: provide a whole column as the RHS. This RHS is then \emph{plonked} into that column slot and we call this \emph{plonk syntax}, or \emph{replace column syntax} if you prefer. By needing to construct a full length vector of a new type, you as the user are more aware of what is happening, and it's clearer to readers of your code that you really do intend to change the column type.\cr\cr

\code{data.table}s are \emph{not} copied-on-write by \code{setkey}, \code{key<-} or \code{:=}. See \code{\link{copy}}.\cr\cr

Additional resources: search for "\code{:=}" in the \href{../doc/datatable-faq.pdf}{FAQs vignette} (3 FAQs mention \code{:=}), search Stack Overflow's \href{http://stackoverflow.com/search?q=\%5Bdata.table\%5D+reference}{data.table tag for "reference"} (6 questions) and search \code{data.table}'s \href{http://rwiki.sciviews.org/doku.php?id=packages:cran:data.table}{wiki}.\cr\cr

Advanced (internals) : sub assigning to columns is easy to see how that is done internally. Removing columns by reference is also straightforward by modifying the vector of column pointers only (using memmove in C). Adding columns is more tricky to see how that can be grown by reference: the list vector of column pointers is over-allocated, see \code{\link{truelength}}. By defining \code{:=} in \code{j} we believe update synax is natural, and scales, but also it bypasses \code{[<-} dispatch via \code{*tmp*} and allows \code{:=} to update by reference with no copies of any part of memory at all.

}
\value{
    \code{DT} is modified by reference and the new value is returned. If you require a copy, take a copy first (using \code{DT2=copy(DT)}). Recall that this package is for large data (of mixed column types, with multi-column keys) where updates by reference can be many orders of magnitude faster than copying the entire table.   
}
\seealso{ \code{\link{data.table}}, \code{\link{copy}}, \code{\link{alloc.col}}, \code{\link{truelength}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{
    DT = data.table(a=LETTERS[1:3],b=4:6,key="a")
    DT[,c:=7]       # add a numeric column, 7 for all rows
    DT[,d:=8L]      # add an integer column, 8L for all rows
    DT[,c:=NULL]    # remove the c column
    DT[2,d:=9L]     # subassign by reference to column d
    DT              # DT changed by reference
    
    DT[a>"A",b:=d*2L]  # subassign to b using d where a>"A"
    DT["A",b:=0L]      # binary search for group "A" and set column b

\dontrun{
    DT[,newcol:=sum(v),by=group]  # like fast transform() by group (not yet implemented)}
   
    # Speed example ...
        
    m = matrix(1,nrow=100000,ncol=100)
    DF = as.data.frame(m)
    DT = as.data.table(m)    
\dontrun{
    system.time(for (i in 1:1000) DF[i,1] <- i)
    # 591 seconds }        
    system.time(for (i in 1:1000) DT[i,V1:=i])
    # 1.16 seconds  ( 509 times faster )
}
\keyword{ data }

