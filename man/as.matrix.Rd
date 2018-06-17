\name{as.matrix}
\alias{as.matrix}
\alias{as.matrix.data.table}
\title{Convert a data.table to a matrix}
\description{
Converts a \code{data.table} into a \code{matrix}, optionally using one
of the columns in the \code{data.table} as the \code{matrix} \code{rownames}.
}
\usage{
\method{as.matrix}{data.table}(x, rownames, rownames.literal, ...)}

\arguments{
\item{x}{a \code{data.table}}
\item{rownames}{optional, a single column name or column index to use as 
the \code{rownames} in the returned \code{matrix}. If \code{TRUE} the
\code{\link{key}} of the \code{data.table} will be used if it is a 
single column, otherwise the first column in the \code{data.table} will
be used. Alternative a vector of length \code{nrow(x)} to assign as the
row names of the returned \code{matrix}.}
\item{rownames.literal}{logical, optional, when \code{x} is a 
\code{data.table} that has only a single row, set \code{rownames.literal} 
to \code{TRUE} if you want to use the vector supplied to the \code{rownames} 
argument as the row names of the returned \code{matrix} instead of looking 
for \code{rownames} as a column of \code{x}.}
\item{\dots}{additional arguments to be passed to or from methods.}
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

An additional argument \code{rownames} is provided for \code{as.matrix.data.table}
to facilitate conversions to matrices where the \code{\link{rownames}} are stored
in a single column of \code{x}, e.g. the first column after using 
\code{\link{dcast.data.table}}.
}

\value{
A new \code{matrix} containing the contents of \code{x}.
}

\seealso{
\code{\link{data.table}}, \code{\link{as.matrix}}, \code{\link{data.matrix}}
\code{\link{array}}
}

\examples{
(dt1 <- data.table(A = letters[1:10], X = 1:10, Y = 11:20))
as.matrix(dt1) # character matrix
as.matrix(dt1, rownames = "A")
as.matrix(dt1, rownames = 1)
as.matrix(dt1, rownames = TRUE)

(dt1 <- data.table(A = letters[1:10], X = 1:10, Y = 11:20))
setkey(dt1, A)
as.matrix(dt1, rownames = TRUE)
}

\keyword{ array }

