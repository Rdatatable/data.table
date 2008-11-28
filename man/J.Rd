\name{J}
\alias{J}
\alias{SJ}
\alias{CJ}
\title{ Creates a Join data table }
\description{
  Creates a data table to be passed in as the i to a data.table join.
}
\usage{
J(...,SORTFIRST=FALSE)
SJ(...)
CJ(...)
}
\arguments{
  \item{\dots}{ Each argument is a vector. Generally each vector is the same length but if they are not then usual silent repitition is applied. }
  \item{SORTFIRST}{ Internal argument. SJ sets this to TRUE }
}
\details{
   J, SJ and CJ are convenience functions for creating a data.table in the context of a data.table 'query' on x.
   x[data.table(id)] is the same as x[J(id)] but the latter is more readable.
   x must have a key when passing in a join table as the i. See \code{\link{[.data.table}}
}
\value{
    J : the same result as calling data.table. J is a direct alias for data.table but results in clearer more readable code.
    SJ : (S)orted (J)oin. The same value as J() but additionally setkey() is called on all the columns in the order they were passed in to SJ. For efficiency.
    CJ : (C)ross (J)oin. A data.table is formed from the cross product of the vectors. For example, 10 ids, and 100 dates, CJ returns a 1000 row table containing all the dates for all the ids.
}
% \references{ }
\author{ Matthew Dowle }
% \note{ }
\seealso{ \code{\link{data.table}}, \code{\link{[.data.table}}, \code{\link{test.data.table}} }
\examples{
    DT = data.table(A=5:1,B=letters[5:1])
    setkey(DT,B)  # re-orders table and marks it sorted.
    DT[J("B")]  # returns the 2nd row
}
\keyword{ data }
