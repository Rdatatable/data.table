\name{J}
\alias{J}
\alias{CJ}
\alias{SJ}
\title{ Creates a Join data table }
\description{
  Creates a data.table to be passed in as the i to a [.data.table join.
}
\usage{
# DT[J(...)]    # J() only for use inside DT[...].
SJ(...)         # DT[SJ(...)]
CJ(...)         # DT[CJ(...)]
}
\arguments{
  \item{\dots}{ Each argument is a vector. Generally each vector is the same length but if they are not then usual silent repitition is applied. }
}
\details{
   \code{SJ} and \code{CJ} are convenience functions for creating a data.table in the context of a data.table 'query' on \code{x}.
   \code{x[data.table(id)]} is the same as \code{x[J(id)]} but the latter is more readable. Identical alternatives are \code{x[list(id)]} and \code{x[.(id)]}.
   \code{x} must have a key when passing in a join table as the \code{i}. See \code{\link{[.data.table}}
}
\value{
    J : the same result as calling list. J is a direct alias for list but results in clearer more readable code.
    SJ : (S)orted (J)oin. The same value as J() but additionally setkey() is called on all the columns in the order they were passed in to SJ. For efficiency, to invoke a binary merge rather than a repeated binary full search for each row of \code{i}.
    CJ : (C)ross (J)oin. A data.table is formed from the cross product of the vectors. For example, 10 ids, and 100 dates, CJ returns a 1000 row table containing all the dates for all the ids.
}
\seealso{ \code{\link{data.table}}, \code{\link{test.data.table}} }
\examples{
    DT = data.table(A=5:1,B=letters[5:1])
    setkey(DT,B)  # re-orders table and marks it sorted.
    DT[J("b")]      # returns the 2nd row
    DT[.("b")]      # same. Style of package plyr.
    DT[list("b")]   # same
}
\keyword{ data }


