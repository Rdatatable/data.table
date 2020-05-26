\name{uniq}
\alias{uniq}
\alias{uniqlist}
\alias{funiq}
\title{ Consecutively unique rows }
\description{
  Finds the consecutively unique rows.
}
\usage{
uniq(x, order=integer())
}
\arguments{
  \item{x}{ data.table type object. }
  \item{order}{ integer vector order of \code{x}, must not contain duplicates. }
}
\details{
  Works like UNIX \emph{uniq} as referred to by \code{\link[base]{unique}}; i.e., it drops immediately repeated rows but doesn't drop duplicates of any previous row. Unless \code{order} is provided, then it also drops any previous row.
}
\note{
  It is an undefined behavior when \code{order} argument contains duplicates. It was designed to take what the \code{\link[base]{order}} function returns. We do not check for duplicates, although we still check for values to be in range \code{1:nrow(x)} and non-NA, to avoid \emph{segfault} exception.
}
\value{
  Integer vector corresponding to rows which are consecutively unique.
}
\seealso{ \code{\link{data.table}}, \code{\link{rleid}} }
\examples{
uniq(data.table())
uniq(data.table(x=integer()))
uniq(data.table(x=integer(), y=integer()))
uniq(data.table(x=1L))
uniq(data.table(x=1L, y=1L))
uniq(data.table(x=1:2))
uniq(data.table(x=1:2, y=1:2))
uniq(data.table(x=1:2)[c(1L,1:2)])
uniq(data.table(x=1:2, y=1:2)[c(1L,1:2)])

# 'order' argument
x = data.table(id = 1:8, v = rep(1:2, each=4))
uniq(x[,"v"])
x = x[c(1:2,7:8,3:4,5:6)]
uniq(x[,"v"])

o = order(x$id)
uniq(x[,"v"], order=o)
# or if we are not sure if 'o' has no duplicates
if (!anyDuplicated(o)) {
  uniq(x[,"v"], order=o)
}
}
\keyword{ data }
