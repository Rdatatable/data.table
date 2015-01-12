\name{rleid}
\alias{rleid}
\alias{rleidv}
\title{ Generate run-length type group id}
\description{
   A convenience function for generating a \emph{run-length} type \emph{id} column to be used in grouping operations. It accepts atomic vectors, lists, data.frames or data.tables as input.
}
\usage{
rleid(...)
rleidv(x, cols=seq_along(x))
}
\arguments{
  \item{x}{ A vector, list, data.frame or data.table. }
  \item{...}{ A sequence of numeric, integer64, character or logical vectors, all of same length. For interactive use.}
  \item{cols}{ Only meaningful for lists, data.frames or data.tables. A character vector of column names (or numbers) of x. }
}
\details{
    At times aggregation (or grouping) operations need to be performed where consecutive runs of identical values should belong to the same group (See \code{\link[base]{rle}}). The use for such a function has come up repeatedly on StackOverflow, see the \code{See Also} section. This function allows to generate \emph{"run-length"} groups directly.

    \code{rleid} is designed for interactive use and accepts a sequence of vectors as arguments. For programming, \code{rleidv} might be more useful. 
}
\value{
	An integer vector with same length as \code{NROW(x)}.
}
\examples{
DT = data.table(grp=rep(c("A", "B", "C", "A", "B"), c(2,2,3,1,2)), value=1:10)
rleid(DT$grp) # get run-length ids
rleidv(DT, "grp") # same as above
# get sum of value over run-length groups
DT[, sum(value), by=.(grp, rleid(grp))]

}
\seealso{
  \code{\link{data.table}}, \url{http://stackoverflow.com/q/21421047/559784}
}
\keyword{ data }
