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
\method{rollup}{data.table}(x, j, by, .SDcols, id = FALSE, label = NULL, \dots)
cube(x, \dots)
\method{cube}{data.table}(x, j, by, .SDcols, id = FALSE, label = NULL, \dots)
groupingsets(x, \dots)
\method{groupingsets}{data.table}(x, j, by, sets, .SDcols,
  id = FALSE, jj, label = NULL, enclos = parent.frame(), \dots)
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
  \item{label}{label(s) to be used in the 'total' rows in the grouping variable columns of the output, that is, in rows where the grouping variable has been aggregated. Can be a named list of scalars, or a scalar, or \code{NULL}. Defaults to \code{NULL}, which results in the grouping variables having \code{NA} in their 'total' rows. See Details.}
  \item{enclos}{the environment containing the symbols referenced by \code{jj}. When writing functions that accept a \code{j} environment for non-standard evaluation by \pkg{data.table}, \code{\link[base]{substitute}()} it and forward it to \code{groupingsets} using the \code{jj} argument, set this to the \code{\link[base]{parent.frame}()} of the function that captures \code{j}.}
}
\details{
  All three functions \code{rollup, cube, groupingsets} are generic methods, \code{data.table} methods are provided.

  The \code{label} argument can be a named list of scalars, or a scalar, or \code{NULL}. When \code{label} is a list, each element name must be (1) a variable name in \code{by}, or (2) the first element of the class in the data.table \code{x} of a variable in \code{by}, or (3) one of 'character', 'integer', 'numeric', 'factor', 'Date', 'IDate'. The order of the list elements is not important. A label specified by variable name will apply only to that variable, while a label specified by first element of a class will apply to all variables in \code{by} for which the first element of the class of the variable in \code{x} matches the \code{label} element name, except for variables that have a label specified by variable name (that is, specification by variable name takes precedence over specification by class). For \code{label} elements with name in \code{by}, the class of the label value must be the same as the class of the variable in \code{x}. For \code{label} elements with name not in \code{by}, the first element of the class of the label value must be the same as the \code{label} element name. For example, \code{label = list(integer = 999, IDate = as.Date("3000-01-01"))} would produce an error because \code{class(999)[1]} is not \code{"integer"} and \code{class(as.Date("3000-01-01"))[1]} is not \code{"IDate"}. A corrected specification would be \code{label = list(integer = 999L, IDate = as.IDate("3000-01-01"))}.

  The \code{label = <scalar>} option provides a shorter alternative in the case where only one class of grouping variable requires a label. For example, \code{label = list(character = "Total")} can be shortened to \code{label = "Total"}. When this option is used, the label will be applied to all variables in \code{by} for which the first element of the class of the variable in \code{x} matches the first element of the class of the scalar.
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
by_vars = c("color", "year", "status")
rollup(DT, j=sum(value), by=by_vars) # default id=FALSE
rollup(DT, j=sum(value), by=by_vars, id=TRUE)
rollup(DT, j=lapply(.SD, sum), by=by_vars, id=TRUE, .SDcols="value")
rollup(DT, j=c(list(count=.N), lapply(.SD, sum)), by=by_vars, id=TRUE)
rollup(DT, j=sum(value), by=by_vars,
       # specify label by variable name
       label=list(color="total", year=as.Date("3000-01-01"), status=factor("total")))
rollup(DT, j=sum(value), by=by_vars,
       # specify label by variable name and first element of class
       label=list(color="total", Date=as.Date("3000-01-01"), factor=factor("total")))
# label is character scalar so applies to color only
rollup(DT, j=sum(value), by=by_vars, label="total")
rollup(DT, j=.N, by=c("color", "year", "status", "value"),
       # label can be explicitly specified as NA or NaN
       label = list(color=NA_character_, year=as.Date(NA), status=factor(NA), value=NaN))

# cube
cube(DT, j = sum(value), by = c("color","year","status"), id=TRUE)
cube(DT, j = lapply(.SD, sum), by = c("color","year","status"), id=TRUE, .SDcols="value")
cube(DT, j = c(list(count=.N), lapply(.SD, sum)), by = c("color","year","status"), id=TRUE)

# groupingsets
groupingsets(DT, j = c(list(count=.N), lapply(.SD, sum)), by = c("color","year","status"),
             sets = list("color", c("year","status"), character()), id=TRUE)
}
\keyword{ data }
