\name{setDT}
\alias{setDT}
\title{Coerce lists and data.frames to data.table by reference}
\description{
  In \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column.. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.

  \code{setDT} converts lists (both named and unnamed) and data.frames to data.tables \emph{by reference}. This feature was requested on \href{https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy}{Stackoverflow}.

}
\usage{
setDT(x, keep.rownames=FALSE, key=NULL, check.names=FALSE)
}
\arguments{
  \item{x}{ A named or unnamed \code{list}, \code{data.frame} or \code{data.table}. }
  \item{keep.rownames}{ For \code{data.frame}s, \code{TRUE} retains the \code{data.frame}'s row names under a new column \code{rn}. \code{keep.rownames = "id"} names the column \code{"id"} instead. } 
  \item{key}{Character vector of one or more column names which is passed to \code{\link{setkeyv}}. It may be a single comma separated string such as \code{key="x,y,z"}, or a vector of names such as \code{key=c("x","y","z")}. }
  \item{check.names}{ Just as \code{check.names} in \code{\link{data.frame}}. }
}

\details{
  When working on large \code{lists} or \code{data.frames}, it might be both time and memory consuming to convert them to a \code{data.table} using \code{as.data.table(.)}, as this will make a complete copy of the input object before to convert it to a \code{data.table}. The \code{setDT} function takes care of this issue by allowing to convert \code{lists} - both named and unnamed lists and \code{data.frames} \emph{by reference} instead. That is, the input object is modified in place, no copy is being made.
}

\value{
    The input is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setDT(X)[, sum(B), by=A]}. If you require a copy, take a copy first (using \code{DT2 = copy(DT)}). See \code{?copy}.
}

\seealso{ \code{\link{data.table}}, \code{\link{as.data.table}}, \code{\link{setDF}}, \code{\link{copy}}, \code{\link{setkey}}, \code{\link{setcolorder}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setorder}}
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

# setkey directly
X = list(a = 4:1, b=runif(4))
setDT(X, key="a")[]

# check.names argument
X = list(a=1:5, a=6:10)
setDT(X, check.names=TRUE)[]

}
\keyword{ data }

