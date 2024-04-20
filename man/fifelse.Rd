\name{fifelse}
\alias{fifelse}
\alias{ifelse}
\title{ Fast ifelse }
\description{
\code{fifelse} is a faster and more robust replacement of \code{\link[base]{ifelse}}. It is comparable to \code{dplyr::if_else} and \code{hutils::if_else}. It returns a value with the same length as \code{test} filled with corresponding values from \code{yes}, \code{no} or eventually \code{na}, depending on \code{test}. Supports \code{bit64}'s \code{integer64} and \code{nanotime} classes.
}
\usage{
  fifelse(test, yes, no, na=NA)
}
\arguments{
  \item{test}{ A logical vector. }
  \item{yes, no}{ Values to return depending on \code{TRUE}/\code{FALSE} element of \code{test}. They must be the same type and be either length \code{1} or the same length of \code{test}. }
  \item{na}{ Value to return if an element of \code{test} is \code{NA}. It must be the same type as \code{yes} and \code{no} and its length must be either \code{1} or the same length of \code{test}. Default value \code{NA}. \code{NULL} is treated as \code{NA}. }
}
\details{
In contrast to \code{\link[base]{ifelse}} attributes are copied from the first non-\code{NA} argument to the output. This is useful when returning \code{Date}, \code{factor} or other classes.
}
\value{
A vector of the same length as \code{test} and attributes as \code{yes}. Data values are taken from the values of \code{yes} and \code{no}, eventually \code{na}.
}
\seealso{
  \code{\link{fcoalesce}}
}
\examples{
x = c(1:4, 3:2, 1:4)
fifelse(x > 2L, x, x - 1L)

# unlike ifelse, fifelse preserves attributes, taken from the 'yes' argument
dates = as.Date(c("2011-01-01","2011-01-02","2011-01-03","2011-01-04","2011-01-05"))
ifelse(dates == "2011-01-01", dates - 1, dates)
fifelse(dates == "2011-01-01", dates - 1, dates)
yes = factor(c("a","b","c"))
no = yes[1L]
ifelse(c(TRUE,FALSE,TRUE), yes, no)
fifelse(c(TRUE,FALSE,TRUE), yes, no)

# Example of using the 'na' argument
fifelse(test = c(-5L:5L < 0L, NA), yes = 1L, no = 0L, na = 2L)
}
\keyword{ data }
