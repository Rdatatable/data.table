\name{setDT}
\alias{setDT}
\title{Convert lists and data.frames to data.table by reference}
\description{
  In \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column.. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.

  \code{setDT} converts lists (both named and unnamed) and data.frames to data.tables \emph{by reference}. This feature was requested on \href{http://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy}{Stackoverflow}.
  
}
\usage{
setDT(x, keep.rownames=FALSE)
}
\arguments{
  \item{x}{ A named or unnamed \code{list}, \code{data.frame} or \code{data.table}. }
  \item{keep.rownames}{ For \code{data.frame}s, \code{TRUE} retains the \code{data.frame}'s row names under a new column \code{rn}. }
}

\details{
  When working on large \code{lists} or \code{data.frames}, it might be both time and memory consuming to convert them to a \code{data.table} using \code{as.data.table(.)}, as this will make a complete copy of the input object before to convert it to a \code{data.table}. The \code{setDT} function takes care of this issue by allowing to convert \code{lists} - both named and unnamed lists and \code{data.frames} \emph{by reference} instead. That is, the input object is modified in place, no copy is being made.
}

\value{
    The input is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setDT(X)[, sum(B), by=A]}. If you require a copy, take a copy first (using \code{DT2 = copy(DT)}). See \code{?copy}.
}

\seealso{ \code{\link{setkey}}, \code{\link{setcolorder}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setorder}}, \code{\link{copy}}, \code{\link{setDF}}
}
\examples{

set.seed(45L)
X = data.frame(A=sample(3, 10, TRUE), 
         B=sample(letters[1:3], 10, TRUE), 
         C=sample(10), stringsAsFactors=FALSE)

# Convert X to data.table by reference and 
# get the frequency of each "A,B" combination
setDT(X)[, .N, by=.(A,B)]

# convert list to data.table 
# autofill names
X = list(1:4, letters[1:4])
setDT(X)
# don't provide names
X = list(a=1:4, letters[1:4])
setDT(X, FALSE)
}
\keyword{ data }

