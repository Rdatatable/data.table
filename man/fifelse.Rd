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
# In the following 2 examples, one can see the contrast between ifelse and 
# fifelse. Unlike ifelse, fifelse preserves the 
# type and class of the inputs. Attributes are are taken from the "yes" argument. 

### Example 1 - Date class preserved
dates = as.Date(c("2011-01-01","2011-01-02","2011-01-03","2011-01-04","2011-01-05"))
ifelse(dates == "2011-01-01", dates - 1, dates)
# [1] 14974 14976 14977 14978 14979
fifelse(dates == "2011-01-01", dates - 1, dates)
# [1] "2010-12-31" "2011-01-02" "2011-01-03" "2011-01-04" "2011-01-05"

### Example 2 - Factor class preserved
v = factor(letters[1:5])
ifelse(v \%in\% c("a","b","c"), v, factor(NA))
# [1]  1  2  3 NA NA
fifelse(v \%in\% c("a","b","c"), v, factor(NA))
# [1] a    b    c    <NA> <NA>
# Levels: a b c d e

\dontrun{
# Example 3:
# Unlike dplyr::if_else and hutils::if_else, fifelse and ifelse
# allow singleton replacements to be en-listed ( i.e. wrapped correctly in list()).

ifelse(c(TRUE, FALSE), 1L, list(0L))
# [[1]]
# [1] 1
# 
# [[2]]
# [1] 0

data.table::fifelse(c(TRUE, FALSE), 1L, list(0L))
# [[1]]
# [1] 1
# 
# [[2]]
# [1] 0

dplyr::if_else(c(TRUE, FALSE), 1L, list(0L))
# Error: `false` must be an integer vector, not a list
# Call `rlang::last_error()` to see a backtrace

hutils::if_else(c(TRUE, FALSE), 1L, list(0L))
# Error in hutils::if_else(c(TRUE, FALSE), 1L, list(0L)) : 
#   typeof(false) == list, yet typeof(true) == integer. Both true and false must have the same type.

# The above is useful given that data.table allows list columns as shown below:
DT1 = data.table(a = 0:5, b = list(1, list(2:3), list(4:6), list(6:4), list(3:2), 1))
DT2 = data.table(a = 0:5, b = list(1, list(2:3), list(4:6), list(6:4), list(3:2), 1))
all.equal(DT1[ , b := fifelse(a > 2, b, 0)],DT2[ , b := ifelse(a > 2, b, 0)])
# [1] TRUE

# Example 4 :
# data.table:fifelse is fast...
# The below code has been run on a desktop with 32GB RAM and 8 logical CPU
x <- -3e8L:3e8 < 0  # 2.4GB
system.time(y1 <- fifelse(x, 1L, 0L))
system.time(y2 <- hutils::if_else(x, 1L, 0L))
system.time(y3 <- ifelse(x, 1L, 0L))
system.time(y4 <- dplyr::if_else(x, 1L, 0L))
identical(y1,y2) && identical(y1,y3) && identical(y1,y4)
#   user  system elapsed (seconds)
#   0.55    0.78    1.33  # data.table v1.12.3
#   2.52    1.44    3.95  # hutils v1.5.0
#   9.46    5.80   15.26  # base v3.6.0
#  11.11    9.25   20.38  # dplyr v0.8.3
}
}
\keyword{ data }
