\name{fcase}
\alias{fcase}
\title{fcase}
\description{
\code{fcase} is a fast implementation of SQL \code{CASE WHEN} statement for R. Conceptually, \code{fcase} is a nested version of \code{\link{fifelse}} (with smarter implementation than manual nesting). It is comparable to \code{dplyr::case_when} and supports \code{bit64}'s \code{integer64} and \code{nanotime} classes.
}
\usage{
  fcase(\dots, default=NA)
}
\arguments{
\item{...}{ A sequence consisting of logical condition (\code{when})-resulting value (\code{value}) \emph{pairs} in the following order \code{when1, value1, when2, value2, ..., whenN, valueN}. Logical conditions \code{when1, when2, ..., whenN} must all have the same length, type and attributes. Each \code{value} may either share length with \code{when} or be length 1. Please see Examples section for further details.}
\item{default}{ Default return value, \code{NA} by default, for when all of the logical conditions \code{when1, when2, ..., whenN} are \code{FALSE} or missing for some entries. }
}
\details{
\code{fcase} evaluates each when-value pair in order, until it finds a \code{when} that is \code{TRUE}. It then returns the corresponding \code{value}. During evaluation, \code{value} will be evaluated regardless of whether the corresponding \code{when} is \code{TRUE} or not, which means recursive calls should be placed in the last when-value pair, see \code{Examples}.

\code{default} is always evaluated, regardless of whether it is returned or not.
}
\value{
  Vector with the same length as the logical conditions (\code{when}) in \code{...}, filled with the corresponding values (\code{value}) from \code{...}, or eventually \code{default}. Attributes of output values \code{value1, value2, ...valueN} in \code{...} are preserved.
}
\seealso{
  \code{\link{fifelse}}
}
\examples{
x = 1:10
fcase(
	x < 5L, 1L,
	x > 5L, 3L
)

fcase(
	x < 5L, 1L:10L,
	x > 5L, 3L:12L
)

# Lazy evaluation example
fcase(
	x < 5L, 1L,
	x >= 5L, 3L,
	x == 5L, stop("provided value is an unexpected one!")
)

# fcase preserves attributes, example with dates
fcase(
	x < 5L, as.Date("2019-10-11"),
	x > 5L, as.Date("2019-10-14")
)

# fcase example with factor; note the matching levels
fcase(
	x < 5L, factor("a", levels=letters[1:3]),
	x > 5L, factor("b", levels=letters[1:3])
)

# Example of using the 'default' argument
fcase(
	x < 5L, 1L,
	x > 5L, 3L,
	default = 5L
)

# fcase can be used for recursion, unlike fifelse
# Recursive function to calculate the Greatest Common Divisor
gcd_dt = function(x,y) {
  r = x\%\%y
  fcase(!r, y, r, gcd_dt(x, y)) # Recursive call must be in the last when-value pair
}
gcd_dt(10L, 1L)
}
\keyword{ data }
