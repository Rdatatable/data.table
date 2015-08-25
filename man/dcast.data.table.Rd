\name{dcast.data.table}
\alias{dcast.data.table}
\alias{dcast}
\title{Fast dcast for data.table}
\description{
  \code{dcast.data.table} is a much faster version of \code{reshape2::dcast}, but for \code{data.table}s. More importantly, it's capable of handling very large data quite efficiently in terms of memory usage in comparison to \code{reshape2::dcast}. 

  From 1.9.6, \code{dcast} is a implemented as a S3 generic in \code{data.table}. To melt or cast data.tables, it is not necessary to load \code{reshape2} anymore. If you have to, then load \code{reshape2} package before loading \code{data.table}. 

  \bold{NEW}: \code{dcast.data.table} can now cast multiple \code{value.var} columns and also accepts multiple functions under \code{fun.aggregate} argument. See \code{examples} for more.
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
  \item{formula}{A formula of the form LHS ~ RHS to cast, see details.}
  \item{fun.aggregate}{Should the data be aggregated before casting? If the formula doesn't identify single observation for each cell, then aggregation defaults to \code{length} with a message.

  \bold{NEW}: it is possible to provide a list of functions to \code{fun.aggregate} argument. See \code{examples}.}
  \item{sep}{Default is \code{_} for backwards compatibility. Character vector of length 1, indicating the separating character in variable names generated during casting.}
  \item{...}{Any other arguments that maybe passed to the aggregating function.}
  \item{margins}{Not implemented yet. Should take variable names to compute margins on. A value of \code{TRUE} would compute all margins.}
  \item{subset}{Specified if casting should be done on subset of the data. Ex: subset = .(col1 <= 5) or subset = .(variable != "January").}
  \item{fill}{Value to fill missing cells with. If \code{fun.aggregate} is present, takes the value by applying the function on 0-length vector.}
  \item{drop}{\code{FALSE} will cast by including all missing combinations.}
  \item{value.var}{Name of the column whose values will be filled to cast. Function `guess()` tries to, well, guess this column automatically, if none is provided.

  \bold{NEW}: it is possible to cast multiple \code{value.var} columns simultaneously now. See \code{examples}.}
  \item{verbose}{Not used yet. Maybe dropped in the future or used to provide information messages onto the console.}
}
\details{
The cast formula takes the form \code{LHS ~ RHS}	, ex: \code{var1 + var2 ~ var3}. The order of entries in the formula is essential. There are two special variables: \code{.} and \code{...}. Their functionality is identical to that of \code{reshape2::dcast}. 

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
  \code{\link{melt.data.table}}, \url{http://cran.r-project.org/package=reshape}
}
\keyword{data}

