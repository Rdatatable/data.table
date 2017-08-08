\name{setops}
\alias{setops}
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
	\item{x,y}{\code{data.table}s.}
	\item{all}{Logical. Default is \code{FALSE} and removes duplicate rows on the result. When \code{TRUE}, if there are \code{xn} copies of a particular row in \code{x} and \code{yn} copies of the same row in \code{y}, then:
		\itemize{

			\item{\code{fintersect} will return \code{min(xn, yn)} copies of that row.}

			\item{\code{fsetdiff} will return \code{max(0, xn-yn)} copies of that row.}

			\item{\code{funion} will return xn+yn copies of that row.}}
		}
}
\details{
  Columns of type \code{complex} and \code{list} are not supported except for \code{funion}.
}
\value{
    A data.table in case of \code{fintersect}, \code{funion} and \code{fsetdiff}. Logical \code{TRUE} or \code{FALSE} for \code{fsetequal}.
}
\seealso{ \code{\link{data.table}}, \code{\link{rbindlist}}, \code{\link{all.equal.data.table}}, \code{\link{unique}}, \code{\link{duplicated}}, \code{\link{uniqueN}}, \code{\link{anyDuplicated}}
}
\references{
\url{https://db.apache.org/derby/papers/Intersect-design.html}
}
\examples{
x = data.table(c(1,2,2,2,3,4,4))
y = data.table(c(2,3,4,4,4,5))
fintersect(x, y)            # intersect
fintersect(x, y, all=TRUE)  # intersect all
fsetdiff(x, y)              # except
fsetdiff(x, y, all=TRUE)    # except all
funion(x, y)                # union
funion(x, y, all=TRUE)      # union all
fsetequal(x, y)             # setequal
}
\keyword{ data }
