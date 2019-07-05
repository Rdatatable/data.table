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
\dontrun{
### Example 1
dates <- as.Date(c("2011-01-01","2011-01-02","2011-01-03","2011-01-04","2011-01-05"))
ifelse(dates == "2011-01-01", dates - 1, dates) # does not display the desired output
# [1] 14974 14976 14977 14978 14979
data.table::fifelse(dates == "2011-01-01", dates - 1, dates)
# [1] "2010-12-31" "2011-01-02" "2011-01-03" "2011-01-04" "2011-01-05"

### Example 2
x   <- c(-5e4L:5e4L) < 0
microbenchmark::microbenchmark(
  data.table::fifelse(x, 1L, 0L),
  base::ifelse(x, 1L, 0L),
  dplyr::if_else(x, 1L, 0L),
  hutils::if_else(x, 1L, 0L),
  times = 100L
)
# Unit: microseconds
#                            expr      min        lq      mean    median        uq       max neval
#  data.table::fifelse(x, 1L, 0L)  151.380  190.0810  376.5134  264.4880  345.0960  7378.278   100
#         base::ifelse(x, 1L, 0L) 2271.984 2340.8315 3499.8378 3129.3760 3502.9080 18044.582   100
#       dplyr::if_else(x, 1L, 0L) 2422.935 2460.3535 4257.2123 3459.5040 4307.9165 25538.319   100
#      hutils::if_else(x, 1L, 0L)  610.652  643.3655  838.1112  729.7465  925.8135  1919.191   100
}
}
\keyword{ data }
