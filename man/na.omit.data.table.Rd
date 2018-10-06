\name{na.omit.data.table}
\alias{na.omit.data.table}
\alias{na.omit}
\title{ Remove rows with missing values on columns specified }
\description{
    This is a \code{data.table} method for the S3 generic \code{stats::na.omit}. The internals are written in C for speed. See examples for benchmark timings.

    \code{bit64::integer64} type is also supported.
}

\usage{
\method{na.omit}{data.table}(object, cols=seq_along(object), invert=FALSE, \dots)
}
\arguments{
  \item{object}{ A \code{data.table}. }
  \item{cols}{ A vector of column names (or numbers) on which to check for missing values. Default is all the columns. }
  \item{invert}{ logical. If \code{FALSE} omits all rows with any missing values (default). \code{TRUE} returns just those rows with missing values instead. }
  \item{\dots}{ Further arguments special methods could require. }
}
\details{
The \code{data.table} method consists of an additional argument \code{cols}, which when specified looks for missing values in just those columns specified. The default value for \code{cols} is all the columns, to be consistent with the default behaviour of \code{stats::na.omit}.

It does not add the attribute \code{na.action} as \code{stats::na.omit} does.
}
\value{
A data.table with just the rows where the specified columns have no missing value in any of them.
}
\seealso{
\code{\link{data.table}}
}
\examples{
DT = data.table(x=c(1,NaN,NA,3), y=c(NA_integer_, 1:3), z=c("a", NA_character_, "b", "c"))
# default behaviour
na.omit(DT)
# omit rows where 'x' has a missing value
na.omit(DT, cols="x")
# omit rows where either 'x' or 'y' have missing values
na.omit(DT, cols=c("x", "y"))

\dontrun{
# Timings on relatively large data
set.seed(1L)
DT = data.table(x = sample(c(1:100, NA_integer_), 5e7L, TRUE),
                y = sample(c(rnorm(100), NA), 5e7L, TRUE))
system.time(ans1 <- na.omit(DT)) ## 2.6 seconds
system.time(ans2 <- stats:::na.omit.data.frame(DT)) ## 29 seconds
# identical? check each column separately, as ans2 will have additional attribute
all(sapply(1:2, function(i) identical(ans1[[i]], ans2[[i]]))) ## TRUE
}
}
\keyword{ data }

