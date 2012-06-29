\name{rbindlist}
\alias{rbindlist}
\title{ Makes one data.table from a list of many }
\description{
  Same as do.call("rbind",l), but much faster.
}
\usage{
rbindlist(l)
}
\arguments{
  \item{l}{ A list of data.table, data.frame, list (which may be mixed). Unless NULL, each item must have the same number of columns of consistent type. }
}
% \details{
% }
\value{
    An unkeyed data.table containing a concatenation of all the items passed in.
}
\seealso{ \code{\link{data.table}} }
\examples{
    DT1 = data.table(A=1:3,B=letters[1:3])
    DT2 = data.table(A=4:5,B=letters[4:5])
    l = list(DT1,DT2)
    rbindlist(l)
}
\keyword{ data }

