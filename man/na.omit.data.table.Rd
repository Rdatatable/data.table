\name{na.omit.data.table}
\alias{na.omit.data.table}
\alias{na.omit}
\alias{is_na}
\alias{any_na}
\title{Find missing values per row and remove rows with missing values on columns specified}
\description{
  \code{is_na()} checks a \code{data.table} for \code{NA} values in row-wise fashion, returning \code{TRUE} if a given row contains a \code{NA} entry and \code{FALSE} otherwise. This corresponds to \code{rowSums(is.na(.))}, but implemented in \code{C} for speed. \code{any_na()} returns the equivalent of \code{any(is_na(.))}, but again is implemented in \code{C} for speed. In order to remove rows containing \code{NA} entries, an S3 generic function to \code{stats::na.omit} if provided for \code{data.table} objects. This internally calls \code{is_na()}. See examples for benchmark timings. \code{bit64::integer64} type is also supported.
}

\usage{
is_na(x, by = seq_along(x))

any_na(x, by = seq_along(x))

\method{na.omit}{data.table}(object, cols=seq_along(object), invert=FALSE, \dots)
}
\arguments{
  \item{x, object}{A \code{data.table}.}
  \item{by, cols}{A vector of column names, column indices or a logical vector, specifying the columns on which to check for missing values. Default is all the columns.}
  \item{invert}{logical. If \code{FALSE} omits all rows with any missing values (default). \code{TRUE} returns just those rows with missing values instead.}
  \item{\dots}{Further arguments special methods could require.}
}
\details{
The \code{data.table} method consists of an additional argument \code{cols}, which when specified looks for missing values in just those columns specified. The default value for \code{cols} is all the columns, to be consistent with the default behaviour of \code{stats::na.omit}.

It does not add the attribute \code{na.action} as \code{stats::na.omit} does.
}
\value{
For \code{is_na()} a logical vector of the same length as the number of rows, for \code{any_na()}, a single logical flag and for \code{na.omit()}, a data.table with just the rows where the specified columns have no missing value in any of them.
}
\seealso{
\code{\link{data.table}}
}
\examples{
DT <- data.table(
  a = c(1,NaN,NA,3),
  b = c(NA_integer_, 1:3),
  c = c("a", NA_character_, "b", "c"),
  d = LETTERS[1:4]
)
# all columns
is_na(DT)
# first and last columns only
identical(is_na(DT, c(1, 4)), is_na(DT, c("a", "b")))
identical(is_na(DT, c(1, 4)), is_na(DT, c(TRUE, FALSE, FALSE, TRUE)))
# check for any NA's
any_na(DT)
# there are none if restricted to the last column
any_na(DT, "d")
# default behaviour
na.omit(DT)
# omit rows where 'a' has a missing value
na.omit(DT, cols="a")
# omit rows where either 'a' or 'b' have missing values
na.omit(DT, cols=c("a", "b"))

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

