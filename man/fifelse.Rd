\name{fifelse}
\alias{fifelse}
\title{ Fast ifelse }
\description{
\code{data.table::fifelse} is comparable to \code{base::ifelse}, \code{dplyr::if_else} and  \code{hutils::if_else}.
Like these functions, it returns a value with the same length as \code{test} and filled with value from \code{yes} or \code{no}.
This function is mostly written in C for speed and unlike \code{base::ifelse} the output type is consistent with those of \code{yes} and \code{no}.
}
\usage{
  fifelse(test, yes, no)
}
\arguments{
  \item{test}{ A logical vector }
  \item{yes, no}{  Values to return depending on \code{TRUE}/\code{FALSE} element of \code{test}. They must be the same type and be either length \code{1} or the same length of \code{test}. Attributes are copied from \code{yes} to the output.}
}
\value{
A vector of the same length as \code{test} and attributes as \code{yes}. Data values are taken from the values of \code{yes} and \code{no}.
}
\examples{
### Example 1
dates <- as.Date(c("2011-01-01","2011-01-02","2011-01-03","2011-01-04","2011-01-05"))
ifelse(dates == "2011-01-01", dates - 1, dates)
# [1] 14974 14976 14977 14978 14979
data.table::fifelse(dates == "2011-01-01", dates - 1, dates)
# [1] "2010-12-31" "2011-01-02" "2011-01-03" "2011-01-04" "2011-01-05"

### Example 2
v <- factor(letters[1:5])
ifelse(v \%in\% c("a","b","c"), v, factor(NA))
# [1]  1  2  3 NA NA
data.table::fifelse(v \%in\% c("a","b","c"), v, factor(NA))
# [1] a    b    c    <NA> <NA>
# Levels: a b c d e
}
\keyword{ data }
