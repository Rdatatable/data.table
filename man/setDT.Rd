\name{setDT}
\alias{setDT}
\title{Convert lists and data.frames to data.table by reference}
\description{
  Note that in \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column.. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.

  \code{setDT} converts lists (both named and unnamed) and data.frames to data.tables \emph{by reference}. This feature was requested on \href{http://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy}{Stackoverflow}.
  
}
\usage{
setDT(x, giveNames=TRUE)
}
\arguments{
  \item{x}{ A named or unnamed \code{list}, \code{data.frame} or \code{data.table}. }
  \item{giveNames}{ For list input to \code{setDT}, \code{TRUE} automatically generates missing column names. \code{FALSE} sets all column names to \code{""}. }
}

\details{
  When working on large \code{lists} or \code{data.frames}, it might be both time and memory consuming to convert them to a \code{data.table} using \code{as.data.table(.)}, as this will make a complete copy of the input object before to convert it to a \code{data.table}. The \code{setDT} function takes care of this issue by allowing to convert \code{lists} - both named and unnamed lists and \code{data.frames} \emph{by reference} instead. That is, the input object is modified in place, no copy is being made.
}

\value{
    The input is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setDT(X)[, sum(b), by=a]}.
}

\seealso{ \code{\link{setkey}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setorder}}, \code{\link{copy}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{

set.seed(45L)
X = data.frame(A=sample(3, 10, TRUE), 
         B=sample(letters[1:3], 10, TRUE), 
         C=sample(10), stringsAsFactors=FALSE)

# get the frequency of each "A,B" combination
setDT(X)[, .N, by="A,B"][]

# convert list to data.table 
# autofill names
X = list(1:4, letters[1:4])
setDT(X)
# don't provide names
X = list(a=1:4, letters[1:4])
setDT(X, FALSE)
}
\keyword{ data }

