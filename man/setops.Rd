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
\title{ Set operators for a pair of data tables }
\description{
  Fast set operators for dealing with datasets as relation algebra sets. Use \code{all = TRUE} to keep duplicate rows and process datasets unique rows quantities. Unlikely in SQL, data.table functions will retain order of rows.
}
\usage{
fintersect(x, y, all = FALSE)
fsetdiff(x, y, all = FALSE)
funion(x, y, all = FALSE)
fsetequal(x, y)
}
\arguments{
  \item{x}{data.table.}
  \item{y}{data.table.}
  \item{all}{logical, decides how to handle duplicate entries, maps to operators known as \emph{INTERSECT ALL}, \emph{EXCEPT ALL}, \emph{UNION ALL}, see details below.}
}
\details{
  Set operators are internally performing joins or forcing uniqueness. That means set operators will not work on datasets having columns not supported in join or unique, e.g. \emph{list} or \emph{complex} columns. The exception is \emph{UNION ALL} case, \code{funion(..., all=TRUE)}, will support \emph{list} type columns. The \emph{bigint} data type \code{\link[bit64]{integer64}} is supported for all set operators.
}
\section{Fast intersect}{
  \code{fintersect} will produce intersection between two datasets.
  If \code{all = FALSE} the distinct dataset will be returned, otherwise \code{min(x.N, y.N)} copies of each unique row. 
  Taking the most basic example:
  \preformatted{
    x = data.table(rep("a", 5))
    y = data.table(rep("a", 2))
    fintersect(x, y) # 1 row
    fintersect(x, y, all=TRUE) # 2 rows
  }
}
\section{Fast setdiff}{
  \code{fsetdiff} will produce difference between two datasets. 
  If \code{all = FALSE} the distinct dataset will be returned, otherwise \code{max(0, x.N - y.N)} copies of each unique row.
  Taking the most basic example:
  \preformatted{
    x = data.table(rep("a", 5))
    y = data.table(rep("a", 2))
    fsetdiff(x, y) # 0 rows
    fsetdiff(x, y, all=TRUE) # 3 rows
  }
}
\section{Fast union}{
  \code{funion} will union two datasets, it is a wrapper on \code{\link{rbindlist}}. 
  If \code{all = FALSE} (default) then it will return distinct dataset.
}
\section{Fast setequal}{
  \code{fsetequal} will tests if two datasets are equal in content, so ignoring row order and attributes. 
  It is a wrapper on \code{\link{all.equal.data.table}} to return logical scalar always.
}
\value{
    data.table, for \code{fsetequal} a logical scalar.
}
\seealso{ \code{\link{rbindlist}}, \code{\link{all.equal.data.table}} }
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
