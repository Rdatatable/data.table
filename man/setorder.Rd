\name{setorder}
\alias{setorder}
\alias{setorderv}
\alias{setcolorder}
\title{Fast reordering of a data.table by reference}
\description{
  Note that in \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column.. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.

  \code{setcolorder} rearranges the columns of data.table, \emph{by reference}, to the new order provided.
  
  \code{setorder()} sorts or rearranges the rows of a \code{data.table} \emph{by reference}, based on the columns provided. It can sort in both ascending and descending order. The functionality is identical to using \code{?order} on a \code{data.frame}, except that \code{setorder} is much faster, is very memory efficient and is much more user-friendly.
  
}
\usage{
setcolorder(DT, neworder)
setorder(DT, ...)
setorderv(DT, cols, order)
}
\arguments{
  \item{DT}{ A \code{data.table}. }
  \item{neworder}{ Character vector of the new column name ordering. May also be column numbers. }
  \item{...}{ The columns to sort by. Do not quote column names. If \code{...} is missing (ex: \code{setorder(DT)}), DT is rearranged based on all columns in ascending order by default. To sort by a column in descending order prefix a \code{"-"}, i.e., \code{setorder(DT, x, -y, z)}. The \code{"-"} works on columns of type \code{character} as well. }
  \item{cols}{ A character vector of column names of \code{DT}, to which to order by. Do not add \code{"-"} here.}
  \item{order}{ An integer vector with only possible values of \code{1} and \code{-1}, corresponding to ascending and descending order. The length of \code{order} must be either \code{1} or equal to that of \code{cols}. If \code{length(order) == 1}, it's recycled to \code{length(cols)}. }
}
\details{
  When it's required to reorder the columns of a \code{data.table}, the idiomatic way is to use \code{setcolorder(DT, neworder)}, instead of doing \code{DT <- DT[, neworder, with=FALSE]}. This is because the latter makes an entire copy of the \code{data.table}, which maybe unnecessary in most situations. \code{setcolorder} also allows column numbers instead of column names for \code{neworder} argument, although it isn't good programming practice to use column numbers. We recommend using column names.
  
  \code{data.table} internally implements extremely fast radix based ordering, which will soon be exported and available as \code{forder}. However, in versions < 1.9.2, fast ordering was only capable of increasing order (ascending). In versions >=1.9.2, \code{data.table}'s internal fast order is also capable of sorting in decreasing order. Note that \code{\link{setkey}} still requires and will only sort in ascending order, and is not related to \code{setorder}. 
  
  By implementing \code{\link{forder}} to handle decreasing order as well, we now don't have to rely on \code{base:::order} anymore. It is now possible to reorder the rows of a \code{data.table} based on columns by reference, ex: \code{setorder(DT, x, -y, z)}. Note that, \code{-y} also works with columns of type \code{character}, unlike \code{base:::order} which requires \code{-xtfrm(y)} (and is slow) instead.
  
  Note that if the \code{data.table} is keyed, \code{setorder} will set it's key to \code{NULL}. 
  }
\value{
    The input is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setorder(DT,x,-y,z)[, cumsum(a), by=list(x,y,z)]}. If you require a copy, take a copy first (using \code{DT2=copy(DT)}). \code{copy()} may also sometimes be useful before \code{:=} is used to subassign to a column by reference. See \code{?copy}.
}
\seealso{ \code{\link{setkey}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setDT}}, \code{\link{copy}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{

set.seed(45L)
DT = data.table(A=sample(3, 10, TRUE), 
         B=sample(letters[1:3], 10, TRUE), C=sample(10))

# setorder
setorder(DT, A, -B)
# same as above but using 'setorderv'
# setorderv(DT, c("A", "B"), c(1,-1))

# setcolorder
setcolorder(DT, c("C", "A", "B"))
}
\keyword{ data }

