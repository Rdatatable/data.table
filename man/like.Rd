\name{like}
\alias{like}
\alias{\%like\%}
\alias{\%ilike\%}
\alias{\%flike\%}
\title{ Convenience function for calling grep. }
\description{
  Intended for use in \code{i} in \code{\link[=data.table]{[.data.table}}, i.e., for subsetting/filtering.
  
  Syntax should be familiar to SQL users, with interpretation as regex.
}
\usage{
like(vector, pattern, ignore.case = FALSE, fixed = FALSE)
vector \%like\% pattern
vector \%ilike\% pattern
vector \%flike\% pattern
}
\arguments{
   \item{vector}{ Either a \code{character} or a \code{factor} vector. }
   \item{pattern}{ Pattern to be matched }
   \item{ignore.case}{ \code{logical}; is \code{pattern} case-sensitive? }
   \item{fixed}{ \code{logical}; should \code{pattern} be interpreted as a literal string (i.e., ignoring regular expressions)? }
}
\details{
  Internally, \code{like} is essentially a wrapper around \code{\link[base:grep]{base::grepl}}, except that it is smarter about handling \code{factor} input (\code{base::grep} uses slow \code{as.character} conversion).
}
\value{
    Logical vector, \code{TRUE} for items that match \code{pattern}.
}
\note{ Current implementation does not make use of sorted keys. }
\seealso{ \code{\link[base:grep]{base::grepl}} }
\examples{
DT = data.table(Name=c("Mary","George","Martha"), Salary=c(2,3,4))
DT[Name \%like\% "^Mar"]
DT[Name \%ilike\% "mar"]
DT[Name \%flike\% "Mar"]
}
\keyword{ data }
