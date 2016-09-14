\name{dcast.data.table}
\alias{dcast.data.table}
\alias{dcast}
\title{Fast dcast for data.table}
\description{
  \code{dcast.data.table} is a much faster version of \code{reshape2::dcast}, but for \code{data.table}s. More importantly, it's capable of handling very large data quite efficiently in terms of memory usage in comparison to \code{reshape2::dcast}. 

  From 1.9.6, \code{dcast} is implemented as an S3 generic in \code{data.table}. To melt or cast \code{data.table}s, it is not necessary to load \code{reshape2} anymore. If you have load \code{reshape2}, do so before loading \code{data.table} to prevent unwanted masking. 

  \bold{NEW}: \code{dcast.data.table} can now cast multiple \code{value.var} columns and also accepts multiple functions to \code{fun.aggregate}. See Examples for more.
}

% \method{dcast}{data.table}
\usage{
\method{dcast}{data.table}(data, formula, fun.aggregate = NULL, sep = "_", 
    ..., margins = NULL, subset = NULL, fill = NULL, 
    drop = TRUE, value.var = guess(data),
    verbose = getOption("datatable.verbose"))
}
\arguments{
  \item{data}{ A \code{data.table}.}
  \item{formula}{A formula of the form LHS ~ RHS to cast, see Details.}
  \item{fun.aggregate}{Should the data be aggregated before casting? If the formula doesn't identify a single observation for each cell, then aggregation defaults to \code{length} with a message.

  \bold{NEW}: it is possible to provide a list of functions to \code{fun.aggregate}. See Examples. }
  \item{sep}{Character vector of length 1, indicating the separating character in variable names generated during casting. Default is \code{_} for backwards compatibility. }
  \item{...}{Any other arguments that may be passed to the aggregating function.}
  \item{margins}{Not implemented yet. Should take variable names to compute margins on. A value of \code{TRUE} would compute all margins.}
  \item{subset}{Specified if casting should be done on a subset of the data. Ex: \code{subset = .(col1 <= 5)} or \code{subset = .(variable != "January")}.}
  \item{fill}{Value with which to fill missing cells. If \code{fun.aggregate} is present, takes the value by applying the function on a 0-length vector.}
  \item{drop}{\code{FALSE} will cast by including all missing combinations. 

  \bold{NEW:} Following \href{https://github.com/Rdatatable/data.table/issues/1512}{#1512}, \code{c(FALSE, TRUE)} will only include all missing combinations of formula \code{LHS}. And \code{c(TRUE, FALSE)} will only include all missing combinations of formula RHS. See Examples.}
  
  \item{value.var}{Name of the column whose values will be filled to cast. Function `guess()` tries to, well, guess this column automatically, if none is provided.

  \bold{NEW}: it is now possible to cast multiple \code{value.var} columns simultaneously. See Examples. }
  \item{verbose}{Not used yet. May be dropped in the future or used to provide informative messages through the console.}
}
\details{
The cast formula takes the form \code{LHS ~ RHS}, ex: \code{var1 + var2 ~ var3}. The order of entries in the formula is essential. There are two special variables: \code{.} and \code{...}. \code{.} represents no variable; \code{...} represents all variables not otherwise mentioned in \code{formula}; see Examples. 

\code{dcast} also allows \code{value.var} columns of type \code{list}.

When variable combinations in \code{formula} doesn't identify a unique value in a cell, \code{fun.aggregate} will have to be specified, which defaults to \code{length} if unspecified. The aggregating function should take a vector as input and return a single value (or a list of length one) as output. In cases where \code{value.var} is a list, the function should be able to handle a list input and provide a single value or list of length one as output.

If the formula's LHS contains the same column more than once, ex: \code{dcast(DT, x+x~ y)}, then the answer will have duplicate names. In those cases, the duplicate names are renamed using \code{make.unique} so that key can be set without issues.

Names for columns that are being cast are generated in the same order (separated by an underscore, \code{_}) from the (unique) values in each column mentioned in the formula RHS.

From \code{v1.9.4}, \code{dcast} tries to preserve attributes whereever possible.

\bold{NEW}: From \code{v1.9.6}, it is possible to cast multiple \code{value.var} columns and also cast by providing multiple \code{fun.aggregate} functions. Multiple \code{fun.aggregate} functions should be provided as a \code{list}, for e.g., \code{list(mean, sum, function(x) paste(x, collapse="")}. \code{value.var} can be either a character vector or list of length=1, or a list of length equal to \code{length(fun.aggregate)}. When \code{value.var} is a character vector or a list of length 1, each function mentioned under \code{fun.aggregate} is applied to every column specified under \code{value.var} column. When \code{value.var} is a list of length equal to \code{length(fun.aggregate)} each element of \code{fun.aggregate} is appled to each element of \code{value.var} column.

}
\value{
    A keyed \code{data.table} that has been cast. The key columns are equal to the variables in the \code{formula} LHS in the same order.
}

\examples{
require(data.table)
names(ChickWeight) <- tolower(names(ChickWeight))
DT <- melt(as.data.table(ChickWeight), id=2:4) # calls melt.data.table

# dcast is a S3 method in data.table from v1.9.6
dcast(DT, time ~ variable, fun=mean)
dcast(DT, diet ~ variable, fun=mean)
dcast(DT, diet+chick ~ time, drop=FALSE)
dcast(DT, diet+chick ~ time, drop=FALSE, fill=0)

# using subset
dcast(DT, chick ~ time, fun=mean, subset=.(time < 10 & chick < 20))

# drop argument, #1512
DT <- data.table(v1 = c(1.1, 1.1, 1.1, 2.2, 2.2, 2.2),
                 v2 = factor(c(1L, 1L, 1L, 3L, 3L, 3L), levels=1:3), 
                 v3 = factor(c(2L, 3L, 5L, 1L, 2L, 6L), levels=1:6), 
                 v4 = c(3L, 2L, 2L, 5L, 4L, 3L)) 
# drop=TRUE
dcast(DT, v1 + v2 ~ v3)                      # default is drop=TRUE
dcast(DT, v1 + v2 ~ v3, drop=FALSE)          # all missing combinations of both LHS and RHS
dcast(DT, v1 + v2 ~ v3, drop=c(FALSE, TRUE)) # all missing combinations of only LHS
dcast(DT, v1 + v2 ~ v3, drop=c(TRUE, FALSE)) # all missing combinations of only RHS

# using . and ...
DT <- data.table(v1 = rep(1:2, each = 6),
                 v2 = rep(rep(1:3, 2), each = 2),
                 v3 = rep(1:2, 6),
                 v4 = rnorm(6))
dcast(DT, ... ~ v3, value.var = "v4") #same as v1 + v2 ~ v3, value.var = "v4"
dcast(DT, v1 + v2 + v3 ~ ., value.var = "v4")

## for each combination of (v1, v2), add up all values of v4
dcast(DT, v1 + v2 ~ ., value.var = "v4", fun.aggregate = sum)

\dontrun{
# benchmark against reshape2's dcast, minimum of 3 runs
set.seed(45)
DT <- data.table(aa=sample(1e4, 1e6, TRUE), 
      bb=sample(1e3, 1e6, TRUE), 
      cc = sample(letters, 1e6, TRUE), dd=runif(1e6))
system.time(dcast(DT, aa ~ cc, fun=sum)) # 0.12 seconds
system.time(dcast(DT, bb ~ cc, fun=mean)) # 0.04 seconds
# reshape2::dcast takes 31 seconds
system.time(dcast(DT, aa + bb ~ cc, fun=sum)) # 1.2 seconds
}

# NEW FEATURE - multiple value.var and multiple fun.aggregate
dt = data.table(x=sample(5,20,TRUE), y=sample(2,20,TRUE), 
                z=sample(letters[1:2], 20,TRUE), d1 = runif(20), d2=1L)
# multiple value.var
dcast(dt, x + y ~ z, fun=sum, value.var=c("d1","d2"))
# multiple fun.aggregate
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var="d1")
# multiple fun.agg and value.var (all combinations)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=c("d1", "d2"))
# multiple fun.agg and value.var (one-to-one)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=list("d1", "d2"))
}
\seealso{
  \code{\link{melt.data.table}}, \code{\link{rowid}}, \url{https://cran.r-project.org/package=reshape}
}
\keyword{data}

