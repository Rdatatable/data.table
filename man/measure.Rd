\name{measure}
\alias{measure}
\alias{measurev}
\title{Specify measure.vars via regex or separator}
\description{
  These functions compute an integer vector or list for use as
  the \code{measure.vars} argument to \code{melt}.
  Each measured variable name is converted into several groups that occupy
  different columns in the output melted data.
  \code{measure} allows specifying group names/conversions in R code
  (each group and conversion specified as an argument)
  whereas \code{measurev} allows specifying group names/conversions using
  data values
  (each group and conversion specified as a list element).
  See
  \href{../doc/datatable-reshape.html}{\code{vignette("datatable-reshape")}}
  for more info.
}
\usage{
measure(\dots, sep, pattern, cols, multiple.keyword="value.name")
measurev(fun.list, sep, pattern, cols, multiple.keyword="value.name",
  group.desc="elements of fun.list")
}
\arguments{
  \item{\dots}{One or more (1) symbols (without argument name; symbol
    is used for group name) or (2) functions to convert the groups
    (with argument name that is used for group name).
    Must have same number of arguments as groups that are
    specified by either \code{sep} or \code{pattern} arguments.}
  \item{fun.list}{Named list which must have the same number of
    elements as groups that are specified by either \code{sep} or
    \code{pattern} arguments. Each name used for a group
    name, and each value must be either a function
    (to convert the group from a character vector to an atomic vector of the
    same size) or NULL (no conversion).}
  \item{sep}{Separator to split each element of \code{cols} into
    groups. Columns that result in the maximum number of groups
    are considered measure variables.}
  \item{pattern}{Perl-compatible regex with capture groups to match to
    \code{cols}. Columns that match the regex are considered measure variables.}
  \item{cols}{A character vector of column names.}
  \item{multiple.keyword}{A string, if used as a group name, then
    measure returns a list and melt returns multiple
    value columns (with names defined by the unique values in that
    group). Otherwise if the string not used as a group name, then
    measure returns a vector and melt returns a single value column.}
  \item{group.desc}{Internal, used in error messages.}
}
\seealso{
  \code{\link{melt}},
  \url{https://github.com/Rdatatable/data.table/wiki/Getting-started}
}
\examples{
(two.iris = data.table(datasets::iris)[c(1,150)])
# melt into a single value column.
melt(two.iris, measure.vars = measure(part, dim, sep="."))
# do the same, programmatically with measurev
my.list = list(part=NULL, dim=NULL)
melt(two.iris, measure.vars=measurev(my.list, sep="."))
# melt into two value columns, one for each part.
melt(two.iris, measure.vars = measure(value.name, dim, sep="."))
# melt into two value columns, one for each dim.
melt(two.iris, measure.vars = measure(part, value.name, sep="."))
# melt using sep, converting child number to integer.
(two.families = data.table(sex_child1="M", sex_child2="F", age_child1=10, age_child2=20))
print(melt(two.families, measure.vars = measure(
  value.name, child=as.integer,
  sep="_child"
)), class=TRUE)
# same melt using pattern.
print(melt(two.families, measure.vars = measure(
  value.name, child=as.integer,
  pattern="(.*)_child(.)"
)), class=TRUE)
# same melt with pattern and measurev function list.
print(melt(two.families, measure.vars = measurev(
  list(value.name=NULL, child=as.integer),
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
