\name{groupingsets}
\alias{rollup}
\alias{cube}
\alias{groupingsets}
\alias{rollup.data.table}
\alias{cube.data.table}
\alias{groupingsets.data.table}
\title{ Grouping Set aggregation for data tables }
\description{
  Calculate aggregates at various levels of groupings producing multiple (sub-)totals. Reflects SQLs \emph{GROUPING SETS} operations.
}
\usage{
rollup(x, \dots)
\method{rollup}{data.table}(x, j, by, .SDcols, id = FALSE, \dots)
cube(x, \dots)
\method{cube}{data.table}(x, j, by, .SDcols, id = FALSE, \dots)
groupingsets(x, \dots)
\method{groupingsets}{data.table}(x, j, by, sets, .SDcols, id = FALSE, jj, \dots)
}
\arguments{
	\item{x}{\code{data.table}.}
	\item{\dots}{argument passed to custom user methods. Ignored for \code{data.table} methods.}
	\item{j}{expression passed to data.table \code{j}.}
	\item{by}{character column names by which we are grouping.}
	\item{sets}{list of character vector reflecting grouping sets, used in \code{groupingsets} for flexibility.}
	\item{.SDcols}{columns to be used in \code{j} expression in \code{.SD} object.}
	\item{id}{logical default \code{FALSE}. If \code{TRUE} it will add leading column with bit mask of grouping sets.}
	\item{jj}{quoted version of \code{j} argument, for convenience. When provided function will ignore \code{j} argument.}
}
\details{
    All three functions \code{rollup, cube, groupingsets} are generic methods, \code{data.table} methods are provided.
}
\value{
    A data.table with various aggregates.
}
\seealso{ \code{\link{data.table}}, \code{\link{rbindlist}}
}
\references{
\url{https://www.postgresql.org/docs/9.5/static/queries-table-expressions.html#QUERIES-GROUPING-SETS}
\url{https://www.postgresql.org/docs/9.5/static/functions-aggregate.html#FUNCTIONS-GROUPING-TABLE}
}
\examples{
n = 24L
set.seed(25)
DT <- data.table(
    color = sample(c("green","yellow","red"), n, TRUE),
    year = as.Date(sample(paste0(2011:2015,"-01-01"), n, TRUE)),
    status = as.factor(sample(c("removed","active","inactive","archived"), n, TRUE)),
    amount = sample(1:5, n, TRUE),
    value = sample(c(3, 3.5, 2.5, 2), n, TRUE)
)

# rollup
rollup(DT, j = sum(value), by = c("color","year","status")) # default id=FALSE
rollup(DT, j = sum(value), by = c("color","year","status"), id=TRUE)
rollup(DT, j = lapply(.SD, sum), by = c("color","year","status"), id=TRUE, .SDcols="value")
rollup(DT, j = c(list(count=.N), lapply(.SD, sum)), by = c("color","year","status"), id=TRUE)

# cube
cube(DT, j = sum(value), by = c("color","year","status"), id=TRUE)
cube(DT, j = lapply(.SD, sum), by = c("color","year","status"), id=TRUE, .SDcols="value")
cube(DT, j = c(list(count=.N), lapply(.SD, sum)), by = c("color","year","status"), id=TRUE)

# groupingsets
groupingsets(DT, j = c(list(count=.N), lapply(.SD, sum)), by = c("color","year","status"),
             sets = list("color", c("year","status"), character()), id=TRUE)
}
\keyword{ data }
