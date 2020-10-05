\name{measure}
\alias{measure}
\title{Specify measure.vars via regex or separator}
\description{
\code{measure} computes an integer vector or list which can be passed as
the \code{measure.vars} argument to \code{melt}.
See the \code{Efficient reshaping using
data.tables} vignette linked below to learn more.
}
\usage{
measure(\dots, sep, pattern, cols, multiple.keyword="value.name")
}
\arguments{
  \item{\dots}{One or more (1) symbols (without argument name; symbol
    is used for output variable column name) or (2)
    functions (with argument name that is used for output variable
    column name). Must have same number of arguments as groups that are
    specified by either \code{sep} or \code{pattern} arguments.}
  \item{sep}{Separator to split \code{cols} into groups. Columns that
    result in the maximum number of groups are considered measure variables.}
  \item{pattern}{Perl-compatible regex with capture groups to match to
    \code{cols}. Columns that match the regex are considered measure variables.}
  \item{cols}{A character vector of column names.}
  \item{multiple.keyword}{A string, if used in \code{\dots}, then
    measure returns a list and melt returns multiple
    value columns (with names defined by the unique values in that
    group). Otherwise if the string not used in \code{\dots}, then
    measure returns a vector and melt returns a single value column.}
}
\seealso{
  \code{\link{melt}},
  \url{https://github.com/Rdatatable/data.table/wiki/Getting-started}
}
\examples{
(two.iris = data.table(datasets::iris)[c(1,150)])
# melt into a single value column.
melt(two.iris, measure.vars = measure(part, dim, sep="."))
# melt into two value columns, one for each part.
melt(two.iris, measure.vars = measure(value.name, dim, sep="."))
# melt into two value columns, one for each dim.
melt(two.iris, measure.vars = measure(part, value.name, sep="."))
# melt using either sep or pattern, converting child number to integer.
(two.families = data.table(sex_child1="M", sex_child2="F", age_child1=10, age_child2=20))
print(melt(two.families, measure.vars = measure(
  value.name, child=as.integer,
  sep="_child"
)), class=TRUE)
print(melt(two.families, measure.vars = measure(
  value.name, child=as.integer,
  pattern="(.*)_child(.)"
)), class=TRUE)
# inspired by data(who, package="tidyr")
(who <- data.table(id=1, new_sp_m5564=2, newrel_f65=3))
# melt to three variable columns, all character.
melt(who, measure.vars = measure(diagnosis, gender, ages, pattern="new_?(.*)_(.)(.*)"))
# melt to five variable columns, two numeric (with custom conversion).
print(melt(who, measure.vars = measure(
  diagnosis, gender, ages,
  ymin=as.numeric,
  ymax=function(y)ifelse(y=="", Inf, as.numeric(y)),
  pattern="new_?(.*)_(.)(([0-9]{2})([0-9]{0,2}))"
)), class=TRUE)
}
\keyword{data}
