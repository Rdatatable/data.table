\name{as.matrix}
\alias{as.matrix}
\alias{as.matrix.data.table}
\title{Convert a data.table to a matrix}
\description{
Converts a \code{data.table} into a \code{matrix}, optionally using one
of the columns in the \code{data.table} as the \code{matrix} \code{rownames}.
}
\usage{
\method{as.matrix}{data.table}(x, rownames=NULL, rownames.value=NULL, \dots)}

\arguments{

\item{x}{a \code{data.table}}

\item{rownames}{optional, a single column name or column number to use as
the \code{rownames} in the returned \code{matrix}. If \code{TRUE} the
\code{\link{key}} of the \code{data.table} will be used if it is a
single column, otherwise the first column in the \code{data.table} will
be used.}

\item{rownames.value}{optional, a vector of values to be used as the
\code{rownames} in the returned \code{matrix}. It must be the same length
as \code{nrow(x)}.}

\item{\dots}{ Required to be present because the generic `as.matrix` generic has it. Arguments here are not currently used or passed on by this method. }

}

\details{
\code{\link{as.matrix}} is a generic function in base R. It dispatches to
\code{as.matrix.data.table} if its \code{x} argument is a \code{data.table}.

The method for \code{data.table}s will return a character matrix if there
are only atomic columns and any non-(numeric/logical/complex) column,
applying \code{\link{as.vector}} to factors and \code{\link{format}} to other
non-character columns. Otherwise, the usual coercion hierarchy (logical <
integer < double < complex) will be used, e.g., all-logical data frames
will be coerced to a logical matrix, mixed logical-integer will give an
integer matrix, etc.
}

\value{
A new \code{matrix} containing the contents of \code{x}.
}

\seealso{
\code{\link{data.table}}, \code{\link{as.matrix}}, \code{\link{data.matrix}}
\code{\link{array}}
}

\examples{
DT <- data.table(A = letters[1:10], X = 1:10, Y = 11:20)
as.matrix(DT) # character matrix
as.matrix(DT, rownames = "A")
as.matrix(DT, rownames = 1)
as.matrix(DT, rownames = TRUE)

setkey(DT, A)
as.matrix(DT, rownames = TRUE)
}

\keyword{ array }

