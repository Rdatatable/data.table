\name{as.data.frame}
\alias{as.data.frame}
\alias{as.data.frame.data.table}
\title{Convert a data.table to a data.frame}
\description{
Converts a \code{data.table} into a \code{data.frame}, optionally using one
of the columns in the \code{data.table} as the \code{data.frame} \code{rownames}.

If you do not need to use one of the columns as the \code{rownames}, use
\code{\link{setDF}} to coerce a \code{data.table} to a \code{data.frame} 
by reference (i.e. without making another copy in memory).
}
\usage{
\method{as.data.frame}{data.table}(x, rownames, ...)}

\arguments{
\item{x}{a \code{data.table}}
\item{rownames}{optional, a single column name or column index to use as 
the \code{rownames} inthe returned \code{matrix}. If \code{TRUE} the
\code{\link{key}} of the \code{data.table} will be used if it is a 
single column, otherwise the first column in the \code{data.table} will
be used.}
\item{\dots}{additional arguments to be passed to or from methods.}
}

\details{
\code{\link{as.data.frame}} is a generic function in base R. It dispatches to
\code{as.data.frame.data.table} if its \code{x} argument is a \code{data.table}.

The method for \code{data.table}s will create a copy of \code{x} with
the \code{data.table} class attributes removed. An additional argument 
\code{rownames} is provided for \code{as.data.frame.data.table} that 
allows a single column to be removed from \code{x} to be used as the 
\code{\link{rownames}} of the resulting \code{data.frame}. An example
of where this is useful is where a function from another package expects
the data stored in the first column of \code{x} to be the \code{rownames}
of the data frame for the purposes of subsetting or matching rows in 
its internal workings.

If you do not wish to keep a copy of \code{x} as a \code{data.table} and 
do not need to use the \code{rownames} argument you should use the
\code{\link{setDF}} function instead, which will coerce \code{x} to a 
\code{data.frame} by reference (i.e. without making another copy in memory).
}

\value{
A new \code{data.frame} containing the contents of \code{x}.
}

\seealso{
\code{\link{data.table}}, \code{\link{as.data.frame}}, \code{\link{setDF}}
}

\examples{
(dt1 <- data.table(A = letters[1:10], X = 1:10, Y = 11:20))
as.data.frame(dt1)
as.data.frame(dt1, rownames = "A")
as.data.frame(dt1, rownames = 1)
as.data.frame(dt1, rownames = TRUE)

(dt1 <- data.table(A = letters[1:10], X = 1:10, Y = 11:20))
setkey(dt1, A)
as.data.frame(dt1, rownames = TRUE)

setDF(dt1) # dt1 is a data.frame now
}
