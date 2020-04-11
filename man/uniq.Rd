\name{uniq}
\alias{uniq}
\alias{uniqlist}
\title{ Consecutively unique rows }
\description{
  Finds the consecutively unique rows.
}
\usage{
uniq(x, order, internal=FALSE)
}
\arguments{
  \item{x}{ data.table type object. }
  \item{order}{ integer vector order of \code{x}. }
  \item{internal}{ logical flag, escapes checks of \code{order} argument. }
}
\details{
  Works like UNIX \emph{uniq} as referred to by \code{\link[base]{unique}}; i.e., it drops immediately repeated rows but doesn't drop duplicates of any previous row. Unless \code{order} is provided, then it also drops any previous row.
  Function should be considered experimental. Argument \code{internal} should not be set to \code{FALSE} unless \code{order} is a \emph{shuffle} (strict permutation of \code{1:nrow(x)}, no duplicates, no NAs). Anyone setting \code{internal} to \code{TRUE} is obligated to check the \code{order} argument beforehand, otherwise \emph{segfault} exception is likely to happen and terminate R process abnormally.
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

if (!anyDuplicated(o) && !anyNA(o) && between(o, 1L, nrow(x))) {
  uniq(x[,"v"], order=o, internal=TRUE)
}
}
\keyword{ data }
