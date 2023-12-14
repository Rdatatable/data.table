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
  Similar to base R set functions, \code{union}, \code{intersect}, \code{setdiff} and \code{setequal} but for \code{data.table}s. Additional \code{all} argument controls how duplicated rows are handled. Functions \code{fintersect}, \code{setdiff} (\code{MINUS} or \code{EXCEPT} in SQL) and \code{funion} are meant to provide functionality of corresponding SQL operators. Unlike SQL, data.table functions will retain row order.
}
\usage{
fintersect(x, y, all = FALSE)
fsetdiff(x, y, all = FALSE)
funion(x, y, all = FALSE)
fsetequal(x, y, all = TRUE)
}
\arguments{
	\item{x, y}{\code{data.table}s.}
	\item{all}{Logical. Default is \code{FALSE} and removes duplicate rows on the result. When \code{TRUE}, if there are \code{xn} copies of a particular row in \code{x} and \code{yn} copies of the same row in \code{y}, then:
	  \itemize{
	    \item\code{fintersect} will return \code{min(xn, yn)} copies of that row.
	    \item\code{fsetdiff} will return \code{max(0, xn-yn)} copies of that row.
	    \item\code{funion} will return \code{xn+yn} copies of that row.
	    \item\code{fsetequal} will return \code{FALSE} unless \code{xn == yn}.
	  }
	}
}
\details{
  \code{bit64::integer64} columns are supported but not \code{complex} and \code{list}, except for \code{funion}.
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
x2 = data.table(c(1,2,3,4)) # same set of rows as x
y = data.table(c(2,3,4,4,4,5))
fintersect(x, y)            # intersect
fintersect(x, y, all=TRUE)  # intersect all
fsetdiff(x, y)              # except
fsetdiff(x, y, all=TRUE)    # except all
funion(x, y)                # union
funion(x, y, all=TRUE)      # union all
fsetequal(x, x2, all=FALSE) # setequal
fsetequal(x, x2)            # setequal all
}
\keyword{ data }
