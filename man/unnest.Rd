\name{funnest}
\alias{funnest}
\title{ Unnest/explode list columns }
\description{
For tables with non-rectangular columns (i.e., \code{list}), \code{funnest} "stretches" the table by creating a row for each list element, while also preserving the structure of rectangular columns. Akin to \code{EXPLODE} in U-SQL or HiveQL/SparkQL or \code{UNNEST} from Presto or BigQuery, and similar to \code{\link{melt}} -- both reshape "long", but \code{funnest} does so for "ragged" tables more naturally.
}
\usage{
  funnest(x, cols = which(vapply_1b(x, is.list)))
}
\arguments{
  \item{x}{ A \code{data.table} }
  \item{cols}{ An \code{integer} vector of column indices of which columns to unnest; defaults to all \code{list} columns. Can be useful for unnesting only a subset of a table's \code{list} columns. Note that non-\code{list} columns are skipped; if there are no \code{list} columns provided, a \code{\link{copy}} of the table is returned. }
}
\details{
  By default, when \code{length(cols) > 1L}, a \emph{cartesian unnest} is performed, that is, the cross-product (\emph{a la} \code{\link{CJ}}) of each row's list elements is returned. If there are two columns in \code{cols}, \code{A} and \code{B}, the output will thus have \code{sum(lengths(A) * lengths(B))} rows.
}
\value{
A \code{data.table}
}
\seealso{
  \code{\link{CJ}}, \code{\link{rbindlist}}
}
\examples{
x = setDT(list(V1 = 1:2, V2 = 3:4, V3 = list(1:3, 1:2), V4 = list(1L, 1:3)))
funnest(x)
}
\keyword{ data }
