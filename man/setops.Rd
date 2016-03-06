\name{setops}
\alias{intersect}
\alias{fintersect}
\alias{setdiff}
\alias{fsetdiff}
\alias{except}
\alias{fexcept}
\alias{union}
\alias{funion}
\alias{setequal}
\alias{fsetequal}
\title{ Set operations for data tables }
\description{
  Similar to base's set functions, \code{union}, \code{intersect}, \code{setdiff} and \code{setequal} but for \code{data.table}s. Additional \code{all} argument controls if/how \code{duplicate} rows are returned. \code{bit64::integer64} is also supported.

  Unlike SQL, data.table functions will retain order of rows in result.
}
\usage{
fintersect(x, y, all = FALSE)
fsetdiff(x, y, all = FALSE)
funion(x, y, all = FALSE)
fsetequal(x, y)
}
\arguments{
  \item{x}{A data.table.}
  \item{y}{A data.table.}
  \item{all}{Logical. Default is \code{FALSE} and removes duplicate rows on the result. When \code{TRUE}, if there are \code{xn} copies of a particular row in \code{x} and \code{yn} copies of the same row in \code{y}, then:

      \code{fintersect} will return \code{min(xn, yn)} copies of that row.

      \code{fsetdiff} will return \code{max(0, xn-yn)} copies of that row.

      \code{funion} will return xn+yn copies of that row.}
}
\details{
  Columns of type \code{complex} and \code{list} are not supported except for \code{funion}.
}
\value{
    A data.table in case of \code{fintersect}, \code{funion} and \code{fsetdiff}. Logical \code{TRUE} or \code{FALSE} for \code{fsetequal}.
}
\seealso{ \code{\link{data.table}}, \code{\link{rbindlist}}, \code{\link{all.equal.data.table}} }
\references{
\url{https://db.apache.org/derby/papers/Intersect-design.html}
}
\examples{
x = data.table(c(1,2,2,2,3,4,4))
y = data.table(c(2,3,4,4,4,5))
# intersect
fintersect(x, y)
# intersect all
fintersect(x, y, all=TRUE)
# except
fsetdiff(x, y)
# except all
fsetdiff(x, y, all=TRUE)
# union
funion(x, y)
# union all
funion(x, y, all=TRUE)
# setequal
fsetequal(x, y)
}
\keyword{ data }
