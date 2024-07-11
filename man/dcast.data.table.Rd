\name{dcast.data.table}
\alias{dcast.data.table}
\alias{dcast}
\title{Fast dcast for data.table}
\description{
  \code{dcast.data.table} is \code{data.table}'s long-to-wide reshaping tool. In the spirit of \code{data.table}, it is very fast and memory efficient, making it well-suited to handling large data sets in RAM.  More importantly, it is capable of handling very large data quite efficiently in terms of memory usage. \code{dcast.data.table} can also cast multiple \code{value.var} columns and accepts multiple functions to \code{fun.aggregate}. See Examples for more.
}

\usage{
\method{dcast}{data.table}(data, formula, fun.aggregate = NULL, sep = "_",
    \dots, margins = NULL, subset = NULL, fill = NULL,
    drop = TRUE, value.var = guess(data),
    verbose = getOption("datatable.verbose"),
    value.var.in.dots = FALSE, value.var.in.LHSdots = value.var.in.dots, 
    value.var.in.RHSdots = value.var.in.dots)
}
\arguments{
  \item{data}{ A \code{data.table}.}
  \item{formula}{A formula of the form LHS ~ RHS to cast, see Details.}
  \item{fun.aggregate}{Should the data be aggregated before casting? If the formula doesn't identify a single observation for each cell, then aggregation defaults to \code{length} with a warning of class 'dt_missing_fun_aggregate_warning'.

  To use multiple aggregation functions, pass a \code{list}; see Examples. }
  \item{sep}{Character vector of length 1, indicating the separating character in variable names generated during casting. Default is \code{_} for backwards compatibility. }
  \item{\dots}{Any other arguments that may be passed to the aggregating function.}
  \item{margins}{Not implemented yet. Should take variable names to compute margins on. A value of \code{TRUE} would compute all margins.}
  \item{subset}{Specified if casting should be done on a subset of the data. Ex: \code{subset = .(col1 <= 5)} or \code{subset = .(variable != "January")}.}
  \item{fill}{Value with which to fill missing cells. If \code{fill=NULL} and missing cells are present, then \code{fun.aggregate} is used on a 0-length vector to obtain a fill value.}
  \item{drop}{\code{FALSE} will cast by including all missing combinations.

  \code{c(FALSE, TRUE)} will only include all missing combinations of formula \code{LHS}; \code{c(TRUE, FALSE)} will only include all missing combinations of formula RHS. See Examples.}

  \item{value.var}{Name of the column whose values will be filled to cast. Function \code{guess()} tries to, well, guess this column automatically, if none is provided.

  Cast multiple \code{value.var} columns simultaneously by passing their names as a \code{character} vector. See Examples. }
  \item{verbose}{Not used yet. May be dropped in the future or used to provide informative messages through the console.}
  \item{value.var.in.dots}{logical; \code{value.var.in.dots = TRUE} is shorthand to save setting both \code{value.var.in.LHSdots = TRUE} and \code{value.var.in.RHSdots = TRUE}.}
  \item{value.var.in.LHSdots}{logical; if TRUE, \code{\dots} in LHS of the \code{formula} includes \code{value.var} variables. The default is \code{FALSE}, so that \code{...} represents all variables not otherwise mentioned in \code{formula} or \code{value.var} (including default/guessed \code{value.var}).}
  \item{value.var.in.RHSdots}{logical; analogous to \code{value.var.in.LHSdots} above, but with respect to RHS of the \code{formula}.}
}
\details{
The cast formula takes the form \code{LHS ~ RHS}, ex: \code{var1 + var2 ~ var3}. The order of entries in the formula is essential. There are two special variables: \code{.} represents no variable, while \code{\dots} represents all variables not otherwise mentioned in \code{formula}, and \code{value.var} depending on \code{value.var.in.LHSdots} and \code{value.var.in.RHSdots} arguments; see Examples.

When not all combinations of LHS & RHS values are present in the data, some or all (in accordance with \code{drop}) missing combinations will replaced with the value specified by \code{fill}. Note that \code{fill} will be converted to the class of \code{value.var}; see Examples.

\code{dcast} also allows \code{value.var} columns of type \code{list}.

When variable combinations in \code{formula} don't identify a unique value, \code{fun.aggregate} will have to be specified, which defaults to \code{length}. For the formula \code{var1 ~ var2}, this means there are some \code{(var1, var2)} combinations in the data corresponding to multiple rows (i.e. \code{x} is not unique by \code{(var1, var2)}.

The aggregating function should take a vector as input and return a single value (or a list of length one) as output. In cases where \code{value.var} is a list, the function should be able to handle a list input and provide a single value or list of length one as output.

If the formula's LHS contains the same column more than once, ex: \code{dcast(DT, x+x~ y)}, then the answer will have duplicate names. In those cases, the duplicate names are renamed using \code{make.unique} so that key can be set without issues.

Names for columns that are being cast are generated in the same order (separated by an underscore, \code{_}) from the (unique) values in each column mentioned in the formula RHS.

From \code{v1.9.4}, \code{dcast} tries to preserve attributes wherever possible.

From \code{v1.9.6}, it is possible to cast multiple \code{value.var} columns and also cast by providing multiple \code{fun.aggregate} functions. Multiple \code{fun.aggregate} functions should be provided as a \code{list}, for e.g., \code{list(mean, sum, function(x) paste(x, collapse="")}. \code{value.var} can be either a character vector or list of length one, or a list of length equal to \code{length(fun.aggregate)}. When \code{value.var} is a character vector or a list of length one, each function mentioned under \code{fun.aggregate} is applied to every column specified under \code{value.var} column. When \code{value.var} is a list of length equal to \code{length(fun.aggregate)} each element of \code{fun.aggregate} is applied to each element of \code{value.var} column.

Historical note: \code{dcast.data.table} was originally designed as an enhancement to \code{reshape2::dcast} in terms of computing and memory efficiency. \code{reshape2} has since been superseded in favour of \code{tidyr}, and \code{dcast} has had a generic defined within \code{data.table} since \code{v1.9.6} in 2015, at which point the dependency between the packages became more etymological than programmatic. We thank the \code{reshape2} authors for the inspiration.

}
\value{
    A keyed \code{data.table} that has been cast. The key columns are equal to the variables in the \code{formula} LHS in the same order.
}

\examples{
ChickWeight = as.data.table(ChickWeight)
setnames(ChickWeight, tolower(names(ChickWeight)))
DT <- melt(as.data.table(ChickWeight), id.vars=2:4) # calls melt.data.table

# dcast is an S3 method in data.table from v1.9.6
dcast(DT, time ~ variable, fun.aggregate=mean)
dcast(DT, diet ~ variable, fun.aggregate=mean)
dcast(DT, diet+chick ~ time, drop=FALSE)
dcast(DT, diet+chick ~ time, drop=FALSE, fill=0)

# using subset
dcast(DT, chick ~ time, fun.aggregate=mean, subset=.(time < 10 & chick < 20))

# drop argument, #1512
DT <- data.table(v1 = c(1.1, 1.1, 1.1, 2.2, 2.2, 2.2),
                 v2 = factor(c(1L, 1L, 1L, 3L, 3L, 3L), levels=1:3),
                 v3 = factor(c(2L, 3L, 5L, 1L, 2L, 6L), levels=1:6),
                 v4 = c(3L, 2L, 2L, 5L, 4L, 3L))
# drop=TRUE
dcast(DT, v1+v2~v3, value.var='v4')                      # default is drop=TRUE
dcast(DT, v1+v2~v3, value.var='v4', drop=FALSE)          # all missing combinations of LHS and RHS
dcast(DT, v1+v2~v3, value.var='v4', drop=c(FALSE, TRUE)) # all missing combinations of LHS only
dcast(DT, v1+v2~v3, value.var='v4', drop=c(TRUE, FALSE)) # all missing combinations of RHS only

# using . and ...
DT <- data.table(v1 = rep(1:2, each = 6),
                 v2 = rep(rep(1:3, 2), each = 2),
                 v3 = rep(1:2, 6),
                 v4 = rnorm(6))
dcast(DT, \dots ~ v3, value.var="v4") # same as v1+v2 ~ v3, value.var="v4"
dcast(DT, \dots ~ v3, value.var="v4", value.var.in.dots=TRUE) # same as v1+v2+v4~v3, value.var="v4"
dcast(DT, v1+v2+v3 ~ ., value.var="v4")

## for each combination of (v1, v2), add up all values of v4
dcast(DT, v1+v2 ~ ., value.var="v4", fun.aggregate=sum)

# fill and types
dcast(DT, v2~v3, value.var='v1', fun.aggregate=length, fill=0L)  #  0L --> 0
dcast(DT, v2~v3, value.var='v4', fun.aggregate=length, fill=1.1) # 1.1 --> 1L

# multiple value.var and multiple fun.aggregate
DT = data.table(x=sample(5,20,TRUE), y=sample(2,20,TRUE),
                z=sample(letters[1:2], 20,TRUE), d1=runif(20), d2=1L)
# multiple value.var
dcast(DT, x+y ~ z, fun.aggregate=sum, value.var=c("d1","d2"))
# multiple fun.aggregate
dcast(DT, x+y ~ z, fun.aggregate=list(sum, mean), value.var="d1")
# multiple fun.agg and value.var (all combinations)
dcast(DT, x+y ~ z, fun.aggregate=list(sum, mean), value.var=c("d1", "d2"))
# multiple fun.agg and value.var (one-to-one)
dcast(DT, x+y ~ z, fun.aggregate=list(sum, mean), value.var=list("d1", "d2"))
}
\seealso{
  \code{\link{melt.data.table}}, \code{\link{rowid}}, \url{https://cran.r-project.org/package=reshape}
}
\keyword{data}
