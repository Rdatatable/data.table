\name{J}
\alias{J}
\alias{CJ}
\alias{SJ}
\title{ Creates a Join data table }
\description{
  Creates a \code{data.table} to be passed in as the \code{i} to a \code{[.data.table} join.
}

\usage{
# DT[J(\dots)]                           # J() only for use inside DT[\dots].
SJ(\dots)                                # DT[SJ(\dots)]
CJ(\dots, sorted = TRUE, unique = FALSE)  # DT[CJ(\dots)]
}

\arguments{
  \item{\dots}{ Each argument is a vector. Generally each vector is the
  same length but if they are not then the usual silent repetition is applied. }
  \item{sorted}{ logical. Should the input be sorted (ascending order)? If \code{FALSE}, the input order is retained. }
  \item{unique}{ logical. When \code{TRUE}, only unique values of each vectors are used (automatically). }
}
\details{
    \code{SJ} and \code{CJ} are convenience functions for creating a data.table in the context of a data.table 'query' on \code{x}.

    \code{x[data.table(id)]} is the same as \code{x[J(id)]} but the latter is more readable. Identical alternatives are \code{x[list(id)]} and \code{x[.(id)]}.

    \code{x} must have a key when passing in a join table as the \code{i}. See \code{\link{[.data.table}}
}
\value{
  \itemize{
    \code{J} : the same result as calling list. \code{J} is a direct alias for list but results in clearer more readable code.

    \code{SJ} : (S)orted (J)oin. The same value as \code{J()} but additionally \code{setkey()} is called on all the columns in the order they were passed in to \code{SJ}. For efficiency, to invoke a binary merge rather than a repeated binary full search for each row of \code{i}.

    \code{CJ} : (C)ross (J)oin. A \code{data.table} is formed from the cross product of the vectors. For example, 10 ids, and 100 dates, \code{CJ} returns a 1000 row table containing all the dates for all the ids. It gains \code{sorted}, which by default is \code{TRUE} for backwards compatibility. \code{FALSE} retains input order.
  }
}
\seealso{ \code{\link{data.table}}, \code{\link{test.data.table}} }
\examples{
DT = data.table(A=5:1,B=letters[5:1])
setkey(DT,B)    # re-orders table and marks it sorted.
DT[J("b")]      # returns the 2nd row
DT[.("b")]      # same. Style of package plyr.
DT[list("b")]   # same

# CJ usage examples
CJ(c(5,NA,1), c(1,3,2)) # sorted and keyed data.table
do.call(CJ, list(c(5,NA,1), c(1,3,2))) # same as above
CJ(c(5,NA,1), c(1,3,2), sorted=FALSE) # same order as input, unkeyed
# use for 'unique=' argument
x = c(1,1,2)
y = c(4,6,4)
CJ(x, y) # output columns are automatically named 'x' and 'y'
CJ(x, y, unique=TRUE) # unique(x) and unique(y) are computed automatically

}
\keyword{ data }


