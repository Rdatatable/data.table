\name{rowid}
\alias{rowid}
\alias{rowidv}
\title{ Generate unique row ids within each group}
\description{
   Convenience functions for generating a unique row ids within each group. It accepts atomic vectors, lists, data.frames or data.tables as input.

   \code{rowid} is intended for interactive use, particularly along with the function \code{dcast} to generate unique ids directly in the formula.

   \code{rowidv(DT, cols=c("x", "y"))} is equivalent to column \code{N} in the code \code{DT[, N := seq_len(.N), by=c("x", "y")]}.

   See examples for more.
}
\usage{
rowid(\dots, prefix=NULL)
rowidv(x, cols=seq_along(x), prefix=NULL)
}
\arguments{
  \item{x}{ A vector, list, data.frame or data.table. }
  \item{\dots}{ A sequence of numeric, integer64, character or logical vectors, all of same length. For interactive use.}
  \item{cols}{ Only meaningful for lists, data.frames or data.tables. A character vector of column names (or numbers) of x. }
  \item{prefix}{ Either \code{NULL} (default) or a character vector of length=1 which is prefixed to the row ids, returning a character vector (instead of an integer vector).}
}
\value{
	When \code{prefix = NULL}, an integer vector with same length as \code{NROW(x)}, else a character vector with the value in \code{prefix} prefixed to the ids obtained.
}
\examples{
DT = data.table(x=c(20,10,10,30,30,20), y=c("a", "a", "a", "b", "b", "b"), z=1:6)

rowid(DT$x) # 1,1,2,1,2,2
rowidv(DT, cols="x") # same as above

rowid(DT$x, prefix="group") # prefixed with 'group'

rowid(DT$x, DT$y) # 1,1,2,1,2,1
rowidv(DT, cols=c("x","y")) # same as above
DT[, .(N=seq_len(.N)), by=.(x,y)]$N # same as above

# convenient usage with dcast
dcast(DT, x ~ rowid(x, prefix="group"), value.var="z")
#     x group1 group2
# 1: 10      2      3
# 2: 20      1      6
# 3: 30      4      5
}
\seealso{
  \code{\link{dcast.data.table}}, \code{\link{rleid}}
}
\keyword{ data }
