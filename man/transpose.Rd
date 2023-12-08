\name{transpose}
\alias{transpose}
\title{Efficient transpose of list}
\description{
  \code{transpose} is an efficient way to transpose \code{lists}, \code{data frames} or \code{data tables}.
}

\usage{
transpose(l, fill=NA, ignore.empty=FALSE, keep.names=NULL, make.names=NULL)
}
\arguments{
  \item{l}{ A list, data.frame or data.table. }
  \item{fill}{ Default is \code{NA}. It is used to fill shorter list elements so as to return each element of the transposed result of equal lengths. }
  \item{ignore.empty}{Default is \code{FALSE}. \code{TRUE} will ignore length-0 list elements.}
  \item{keep.names}{The name of the first column in the result containing the names of the input; e.g. \code{keep.names="rn"}. By default \code{NULL} and the names of the input are discarded.}
  \item{make.names}{The name or number of a column in the input to use as names of the output; e.g. \code{make.names="rn"}. By default \code{NULL} and default names are given to the output columns.}
}
\details{
  The list elements (or columns of \code{data.frame}/\code{data.table}) should be all \code{atomic}. If list elements are of unequal lengths, the value provided in \code{fill} will be used so that the resulting list always has all elements of identical lengths. The class of input object is also preserved in the transposed result.

  The \code{ignore.empty} argument can be used to skip or include length-0 elements.

  This is particularly useful in tasks that require splitting a character column and assigning each part to a separate column. This operation is quite common enough that a function \code{\link{tstrsplit}} is exported.

  \code{factor} columns are converted to \code{character} type. Attributes are not preserved at the moment. This may change in the future.
}
\value{
  A transposed \code{list}, \code{data.frame} or \code{data.table}.
}

\examples{
ll = list(1:5, 6:8)
transpose(ll)
setDT(transpose(ll, fill=0))[]

DT = data.table(x=1:5, y=6:10)
transpose(DT)
}
\seealso{
  \code{\link{data.table}}, \code{\link{tstrsplit}}
}
\keyword{ data }

