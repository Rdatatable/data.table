\name{dcast.data.table}
\alias{dcast.data.table}
\alias{dcast}
\title{Fast dcast for data.table}
\description{
  \code{dcast.data.table} is a much faster version of \code{reshape2::dcast}, but for \code{data.table}s. More importantly, it's capable of handling very large data quite efficiently in terms of memory usage in comparison to \code{reshape2}. 
  
  This is still under development, meaning it's stable, but not all features are complete yet. We also plan to make it an S3 generic by making changes to \code{reshape2::dcast} which will allow the same syntax \code{dcast(...)} seamlessly on both \code{data.table}s and \code{data.frame}s.
}
% \method{dcast}{data.table}
\usage{
## fast dcast a data.table (not an S3 method yet)
dcast.data.table(data, formula, fun.aggregate = NULL, 
    ..., margins = NULL, subset = NULL, fill = NULL, 
    drop = TRUE, value.var = guess(data),
    verbose = getOption("datatable.verbose"))
}
\arguments{
  \item{data}{ A molten \code{data.table} object, see \code{\link{melt.data.table}}}
  \item{formula}{A formula of the form LHS ~ RHS to cast, see details.}
  \item{fun.aggregate}{Should the data be aggregated before casting? If the formula doesn't identify single observation for each cell, then aggregation defaults to \code{length} with a message.}
  \item{...}{Any other arguments that maybe passed to the aggregating function.}
  \item{margins}{Not implemented yet. Should take variable names to compute margins on. A value of \code{TRUE} would compute all margins.}
  \item{subset}{Specified if casting should be done on subset of the data. Ex: subset = .(col1 <= 5) or subset = .(variable != "January").}
  \item{fill}{Value to fill missing cells with. If \code{fun.aggregate} is present, takes the value by applying the function on 0-length vector.}
  \item{drop}{\code{FALSE} will cast by including all missing combinations.}
  \item{value.var}{Name of the column whose values will be filled to cast. Function `guess()` tries to, well, guess this column automatically, if none is provided.}
  \item{verbose}{Not used yet. Maybe dropped in the future or used to provide information messages onto the console.}
}
\details{
The cast formula takes the form \code{LHS ~ RHS}	, ex: \code{var1 + var2 ~ var3}. The order of entries in the formula is essential. There are two special variables: \code{.} and \code{...}. Their functionality is identical to that of \code{reshape2:::dcast}. 

\code{dcast.data.table} also allows \code{value.var} columns of type \code{list}.

When the combination of variables in \code{formula} doesn't identify a unique value in a cell, \code{fun.aggregate} will have to be used. The aggregating function should take a vector as input and return a single value (or a list of length one) as output. In cases where \code{value.var} is a list, the function should be able to handle a list input and provide a single value or list of length one as output.

If the formula's LHS contains the same column more than once, ex: \code{dcast.data.table(DT, x+x~ y)}, then the answer will have duplicate names. In those cases, the duplicate names are renamed using \code{make.unique} so that the key can be set without issues.

Names for columns that are being cast are generated in the same order (separated by a \code{_}) from the (unique) values in each column mentioned in the formula RHS.

From \code{v1.9.4}, \code{dcast.data.table} tries to preserve attributes whereever possible, except when \code{value.var} is a factor (or ordered factor). For \code{factor} types, the resulting casted columns will be coerced to character type thereby losing \code{levels} attribute.
}
\value{
    A keyed \code{data.table} that has been cast. The key columns are equal to the variables in the \code{formula} LHS in the same order.
}

\note{
\bold{Currently unimplemented features from \code{reshape2::dcast}:}
\enumerate{
    \item At the moment, only formula interface is supported. That the argument \code{formula} can accept a list of quoted expressions is yet to be implemented. 
    \item The argument \code{margins} is yet to be implemented. 
    }
}
\examples{
require(data.table)
names(ChickWeight) <- tolower(names(ChickWeight))
DT <- melt(as.data.table(ChickWeight), id=2:4) # calls melt.data.table

# no S3 method yet, have to use "dcast.data.table"
dcast.data.table(DT, time ~ variable, fun=mean)
dcast.data.table(DT, diet ~ variable, fun=mean)
dcast.data.table(DT, diet+chick ~ time, drop=FALSE)
dcast.data.table(DT, diet+chick ~ time, drop=FALSE, fill=0)

# using subset
dcast.data.table(DT, chick ~ time, fun=mean, subset=.(time < 10 & chick < 20))

\dontrun{
# on big data
set.seed(45)
DT <- data.table(aa=sample(1e4, 1e6, TRUE), 
      bb=sample(1e3, 1e6, TRUE), 
      cc = sample(letters, 1e6, TRUE), dd=runif(1e6))
system.time(dcast.data.table(DT, aa ~ cc, fun=sum)) # 0.28 seconds
system.time(dcast.data.table(DT, bb ~ cc, fun=mean)) # 0.08 seconds
# reshape2::dcast takes 99 seconds
system.time(dcast.data.table(DT, aa + bb ~ cc, fun=sum)) # 2.7 seconds
}
}
\seealso{
  \code{\link{melt.data.table}}, \url{https://github.com/Rdatatable/data.table}
}
\keyword{data}

