\name{fcase}
\alias{fcase}
\title{fcase}
\description{
\code{fcase} is a fast implementation of SQL \code{CASE WHEN} statement for R. Conceptually, \code{fcase} is a nested version of \code{\link{fifelse}} (with smarter implementation than manual nesting). It is comparable to \code{dplyr::case_when} and supports \code{bit64}'s \code{integer64} and \code{nanotime} classes.
}
\usage{
  fcase(..., default=NA)
}
\arguments{
\item{...}{ A sequence consisting of logical condition (\code{when})-resulting value (\code{value}) \emph{pairs} in the following order \code{when1, value1, when2, value2, ..., whenN, valueN}. Logical conditions \code{when1, when2, ..., whenN} must all have the same length, type and attributes. Each \code{value} may either share length with \code{when} or be length 1. Please see Examples section for further details.}
\item{default}{ Default return value, \code{NA} by default, for when all of the logical conditions \code{when1, when2, ..., whenN} are \code{FALSE} for some entries. }
}
\value{
  Vector with the same length as the logical conditions (\code{when}) in \code{...}, filled with corresponding resulting value from \code{...}, or eventually \code{default}. Attributes of output values \code{value1, value2, ...valueN} in \code{...} are preserved.
}
\seealso{
  \code{\link{fifelse}}
}
\examples{
x = 1:10 
data.table::fcase(
	x < 5L, 1L,
	x > 5L, 3L
)
# [1]  1  1  1  1 NA  3  3  3  3  3

data.table::fcase(
	x < 5L, 1L:10L,
	x > 5L, 3L:12L
)
# [1]  1  2  3  4 NA  8  9 10 11 12

# fcase preserves attributes, example with dates
data.table::fcase(
	x < 5L, as.Date("2019-10-11"),
	x > 5L, as.Date("2019-10-14")
)
# [1] "2019-10-11" "2019-10-11" "2019-10-11"
# [4] "2019-10-11" NA  "2019-10-14" "2019-10-14" 
# [8] "2019-10-14" "2019-10-14" "2019-10-14"

# fcase example with factor
data.table::fcase(
	x < 5L, factor("a", levels=letters[1:3]),
	x > 5L, factor("b", levels=letters[1:3])
)
# [1] a    a    a    a    <NA> b    b    b    b    b
# Levels: a b c

# Example of using the 'default' arguement
data.table::fcase(
	x < 5L, 1L,
	x > 5L, 3L,
	default = 5L
)
# [1] 1 1 1 1 5 3 3 3 3 3
}
\keyword{ data }
