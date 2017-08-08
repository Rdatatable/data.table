\name{setorder}
\alias{setorder}
\alias{setorderv}
\alias{order}
\alias{fastorder}
\alias{forder}

\title{Fast row reordering of a data.table by reference}
\description{
In \code{data.table} parlance, all \code{set*} functions change their input 
\emph{by reference}. That is, no copy is made at all, other than temporary 
working memory, which is as large as one column.. The only other 
\code{data.table} operator that modifies input by reference is \code{\link{:=}}. 
Check out the \code{See Also} section below for other \code{set*} function 
\code{data.table} provides.

\code{setorder} (and \code{setorderv}) reorders the rows of a \code{data.table} 
based on the columns (and column order) provided. It reorders the table 
\emph{by reference} and is therefore very memory efficient.

Also \code{x[order(.)]} is now optimised internally to use data.table's fast 
order by default. data.table always reorders in C-locale. To sort by session 
locale, use \code{x[base::order(.)]} instead.

\code{bit64::integer64} type is also supported for reordering rows of a 
\code{data.table}.
}

\usage{
setorder(x, ..., na.last=FALSE)
setorderv(x, cols, order=1L, na.last=FALSE)
# optimised to use data.table's internal fast order
# x[order(., na.last=TRUE)]
}
\arguments{
\item{x}{ A \code{data.table}. }
\item{...}{ The columns to sort by. Do not quote column names. If \code{...} 
is missing (ex: \code{setorder(x)}), \code{x} is rearranged based on all 
columns in ascending order by default. To sort by a column in descending order 
prefix a \code{"-"}, i.e., \code{setorder(x, a, -b, c)}. The \code{-b} works 
when \code{b} is of type \code{character} as well. }
\item{cols}{ A character vector of column names of \code{x}, to which to order 
by. Do not add \code{"-"} here. Use \code{order} argument instead.}
\item{order}{ An integer vector with only possible values of \code{1} and 
\code{-1}, corresponding to ascending and descending order. The length of 
\code{order} must be either \code{1} or equal to that of \code{cols}. If 
\code{length(order) == 1}, it's recycled to \code{length(cols)}. }
\item{na.last}{logical. If \code{TRUE}, missing values in the data are placed 
last; if \code{FALSE}, they are placed first; if \code{NA} they are removed. 
\code{na.last=NA} is valid only for \code{x[order(., na.last)]} and it's 
default is \code{TRUE}. \code{setorder} and \code{setorderv} only accept 
TRUE/FALSE with default \code{FALSE}.}
}
\details{
\code{data.table} implements fast radix based ordering. In versions <= 1.9.2, 
it was only capable of increasing order (ascending). From 1.9.4 on, the 
functionality has been extended to decreasing order (descending) as well. 

\code{setorder} accepts unquoted column names (with names preceded with a 
\code{-} sign for descending order) and reorders data.table rows 
\emph{by reference}, for e.g., \code{setorder(x, a, -b, c)}. Note that 
\code{-b} also works with columns of type \code{character} unlike 
\code{base::order}, which requires \code{-xtfrm(y)} instead (which is slow). 
\code{setorderv} in turn accepts a  character vector of column names and an 
integer vector of column order separately.

Note that \code{\link{setkey}} still requires and will always sort only in 
ascending order, and is different from \code{setorder} in that it additionally 
sets the \code{sorted} attribute. 

\code{na.last} argument, by default, is \code{FALSE} for \code{setorder} and 
\code{setorderv} to be consistent with \code{data.table}'s \code{setkey} and 
is \code{TRUE} for \code{x[order(.)]} to be consistent with \code{base::order}. 
Only \code{x[order(.)]} can have \code{na.last = NA} as it's a subset operation 
as opposed to \code{setorder} or \code{setorderv} which reorders the data.table 
by reference.

If \code{setorder} results in reordering of the rows of a keyed \code{data.table}, 
then it's key will be set to \code{NULL}.
}
\value{
The input is modified by reference, and returned (invisibly) so it can be used 
in compound statements; e.g., \code{setorder(DT,a,-b)[, cumsum(c), by=list(a,b)]}. 
If you require a copy, take a copy first (using \code{DT2 = copy(DT)}). See 
\code{?copy}.
}
\seealso{ \code{\link{setkey}}, \code{\link{setcolorder}}, \code{\link{setattr}}, 
\code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setDT}}, 
\code{\link{setDF}}, \code{\link{copy}}, \code{\link{setNumericRounding}}
}
\examples{

set.seed(45L)
DT = data.table(A=sample(3, 10, TRUE), 
         B=sample(letters[1:3], 10, TRUE), C=sample(10))

# setorder
setorder(DT, A, -B)

# same as above, but using setorderv
setorderv(DT, c("A", "B"), c(1, -1))
}
\keyword{ data }

