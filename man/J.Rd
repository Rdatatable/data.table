\name{J}
\alias{J}
\alias{CJ}
\alias{SJ}
\title{
  Creates a join \code{data.table}
}
\description{
  Creates a \code{data.table} for use in \code{i} in a \code{[.data.table} join.
}

\usage{
# DT[J(...)]                          # J() only for use inside DT[...]
# DT[.(...)]                          # .() only for use inside DT[...]
# DT[list(...)]                       # same; .(), list() and J() are identical
SJ(\dots)                             # DT[SJ(...)]
CJ(\dots, sorted=TRUE, unique=FALSE)  # DT[CJ(...)]
}

\arguments{
  \item{\dots}{ Each argument is a vector. Generally each vector is the
  same length, but if they are not then the usual silent recycling is applied. }
  \item{sorted}{ logical. Should \code{setkey()} be called on all the columns in the order they were passed to \code{CJ}? }
  \item{unique}{ logical. When \code{TRUE}, only unique values of each vectors are used (automatically). }
}
\details{
    \code{SJ} and \code{CJ} are convenience functions to create a \code{data.table} to be used in \code{i} when performing a \code{data.table} 'query' on \code{x}.

    \code{x[data.table(id)]} is the same as \code{x[J(id)]} but the latter is more readable. Identical alternatives are \code{x[list(id)]} and \code{x[.(id)]}.

    When using a join table in \code{i}, \code{x} must either be keyed or the \code{on} argument be used to indicate the columns in \code{x} and \code{i} which should be joined. See \code{\link{[.data.table}}.
}
\value{
  \itemize{
    \code{J} : the same result as calling \code{list}, for which \code{J} is a direct alias.

    \code{SJ} : \strong{S}orted \strong{J}oin. The same value as \code{J()} but additionally \code{setkey()} is called on all columns in the order they were passed to \code{SJ}. For efficiency, to invoke a binary merge rather than a repeated binary full search for each row of \code{i}.

    \code{CJ} : \strong{C}ross \strong{J}oin. A \code{data.table} is formed from the cross product of the vectors. For example, \code{CJ} on 10 ids and 100 dates, returns a 1000 row table containing all dates for all ids. If \code{sorted = TRUE} (default), \code{setkey()} is called on all columns in the order they were passed in to \code{CJ}. If \code{sorted = FALSE}, the result is unkeyed and input order is retained.
  }
}
\seealso{ \code{\link{data.table}}, \code{\link{test.data.table}} }
\examples{
DT = data.table(A=5:1, B=letters[5:1])
setkey(DT, B)   # reorders table and marks it sorted
DT[J("b")]      # returns the 2nd row
DT[list("b")]   # same
DT[.("b")]      # same using the dot alias for list

# CJ usage examples
CJ(c(5, NA, 1), c(1, 3, 2))                 # sorted and keyed data.table
do.call(CJ, list(c(5, NA, 1), c(1, 3, 2)))  # same as above
CJ(c(5, NA, 1), c(1, 3, 2), sorted=FALSE)   # same order as input, unkeyed
# use for 'unique=' argument
x = c(1, 1, 2)
y = c(4, 6, 4)
CJ(x, y)              # output columns are automatically named 'x' and 'y'
CJ(x, y, unique=TRUE) # unique(x) and unique(y) are computed automatically
CJ(x, y, sorted = FALSE) # retain input order for y
}
\keyword{ data }


