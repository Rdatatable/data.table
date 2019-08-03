\name{fifelse}
\alias{fifelse}
\alias{ifelse}
\title{ Fast ifelse }
\description{
\code{fifelse} is a faster and more robust replacement of \code{\link[base]{ifelse}}. It returns a value with the same length as \code{test} filled with corresponding values from \code{yes}, \code{no} or eventually \code{NA}, depending on \code{test}. Supports \code{bit64}'s \code{integer64} and \code{nanotime} classes.
}
\usage{
  fifelse(test, yes, no)
}
\arguments{
  \item{test}{ A logical vector }
  \item{yes, no}{  Values to return depending on \code{TRUE}/\code{FALSE} element of \code{test}. They must be the same type and be either length \code{1} or the same length of \code{test}.}
}
\details{
In contrast to \code{\link[base]{ifelse}} attributes are copied from \code{yes} to the output. This is useful when returning \code{Date}, \code{factor} or other classes.
}
\value{
A vector of the same length as \code{test} and attributes as \code{yes}. Data values are taken from the values of \code{yes} and \code{no}, eventually \code{NA}.
}
\examples{
x = c(1:4, 3:2, 1:4)
fifelse(x > 2L, x, x - 1L)

# unlike ifelse, fifelse preserves attributes, taken from the "yes" argument
dates = as.Date(c("2011-01-01","2011-01-02","2011-01-03","2011-01-04","2011-01-05"))
ifelse(dates == "2011-01-01", dates - 1, dates)
fifelse(dates == "2011-01-01", dates - 1, dates)
yes = factor(c("a","b","c"))
no = yes[1L]
ifelse(c(TRUE,FALSE,TRUE), yes, no)
fifelse(c(TRUE,FALSE,TRUE), yes, no)

# allow singleton replacements to be en-listed ( i.e. wrapped correctly in list() )
ifelse(c(TRUE, FALSE), 1L, list(0L))
fifelse(c(TRUE, FALSE), 1L, list(0L))
# the above is useful given that data.table allows list columns
DT = data.table(a = 0:5, b = list(1, list(2:3), list(4:6), list(6:4), list(3:2), 1))
DT[ , b := fifelse(a > 2, b, 0)][]
}
\keyword{ data }
