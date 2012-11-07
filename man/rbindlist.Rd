\name{rbindlist}
\alias{rbindlist}
\title{ Makes one data.table from a list of many }
\description{
  Same as \code{do.call("rbind",l)}, but much faster.
}
\usage{
rbindlist(l)
}
\arguments{
  \item{l}{ A list of \code{data.table}, \code{data.frame} or \code{list} objects.}
}
\details{
Each item of \code{l} may be either \code{NULL} (skipped), an empty object (0 rows) (skipped), or, have the same number of columns as the first non empty item. All items do not have to be the same type; e.g, a \code{data.table} may be bound with a \code{list}. The column types of the result are taken from the first non-empty item. If subsequent non-empty items have columns that mismatch in type, they are coerced to the first non-empty item's column types.
}
\value{
    An unkeyed \code{data.table} containing a concatenation of all the items passed in.
}
\seealso{ \code{\link{data.table}} }
\examples{
    DT1 = data.table(A=1:3,B=letters[1:3])
    DT2 = data.table(A=4:5,B=letters[4:5])
    l = list(DT1,DT2)
    rbindlist(l)
}
\keyword{ data }

