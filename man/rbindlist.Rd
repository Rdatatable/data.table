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
  \item{l}{ A list of \code{data.table}, \code{data.frame} or \code{list} objects. Each item must be either \code{NULL} or have the same number of columns of consistent type. All items do not have to be the same type; e.g, a \code{data.table} may be bound with a \code{list}.}
}
% \details{
% }
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

