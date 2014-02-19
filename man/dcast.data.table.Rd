\name{dcast.data.table}
\alias{dcast.data.table}
\alias{dcast}
\title{Fast dcast for data.table}
\description{
  A \code{dcast.data.table} is a much faster version of \code{reshape2:::dcast}, but for data.table. More importantly, it's able to handle bigger data quite efficiently without compromising speed. This is still under development, meaning it's stable, but not all features are complete yet. Once complete, we plan to make it an S3 generic by making changes to \code{reshape2:::dcast}. Then by loading both \code{data.table} and \code{reshape2}, one could use \code{dcast} on \code{data.table} as one would on a \code{data.frame}.
  
  If you think of a particular feature that might be useful, then file a feature request (FR) at the datatable projects page (link at the bottom). 
}
% \method{dcast}{data.table}
\usage{
## fast dcast a data.table (not an S3 method yet)
dcast.data.table(data, formula, fun.aggregate = NULL, 
	..., margins = NULL, subset = NULL, fill = NULL, 
	drop = TRUE, value.var = guess(data), 
	tolerance = .Machine$double.eps^0.5, 
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
  \item{tolerance}{Machine tolerance, used for comparing numerics.}
  \item{verbose}{Not used yet. Maybe dropped in the future or used to provide information messages onto the console.}
}
\details{
The cast formula takes the form \code{LHS ~ RHS}	, ex: \code{var1 + var2 ~ var3}. The order of entries in the formula is essential. There are two special variables: \code{.} and \code{...}. Their functionality is identical to that of \code{reshape2:::dcast}. 

\code{dcast.data.table} also allows \code{value.var} columns of type \code{list}.

When the combination of variables in \code{formula} doesn't identify a unique value in a cell, \code{fun.aggregate} will have to be used. The aggregating function should take a vector as input and return a single value (or a list of length one) as output. In cases where \code{value.var} is a list, the function should be able to handle a list input and provide a single value or list of length one as output.

If the formula's LHS contains the same column more than once, ex: \code{dcast.data.table(DT, x+x~ y)}, then the answer will have duplicate names. In those cases, the duplicate names are renamed using \code{make.unique} so that the key can be set without issues.

The only feature that's not implemented from \code{reshape2:::dcast} *yet* is the argument \code{margins}. 
}
\value{
    A keyed \code{data.table} that has been cast. The key columns are equal to the variables in the \code{formula} LHS in the same order.
}

\note{
\bold{Currently unimplemented features from \code{reshape2:::melt}:}
\enumerate{
	\item At the moment, only formula interface is supported. That the argument \code{formula} can accept a list of quoted expressions is yet to be implemented. 
	\item The argument \code{margins} is yet to be implemented. 
}

\bold{New features and the ones likely to be added:}
\enumerate{
	\item Implemented: \code{dcast.data.table} allows \code{value.var} column to be of type \code{list}.
	\item To do: The \code{fun.aggregate} could accept more than one function. It could take a list of functions, all of which satisfy the criteria that they take a vector as input and provide a single value as output. This is very useful for scenarios where one needs to calculate multiple summary statistics on a data set and cast simultaneously.

}

% \bold{Speedup using \code{fdcast}:}
% Benchmarking on relatively big data set gives over 98% speedup as shown below.
% 
%     \preformatted{
%     n <- 1E7
%     DF <- data.frame( stringsAsFactors=FALSE,
%       x=sample(letters, n, TRUE),
%       y=sample(LETTERS, n, TRUE),
%       za=rnorm(n),
%       zb=rnorm(n),
%       zc=rnorm(n)
%     )
%     DT <- as.data.table(DF)
%     system.time(fdcast(DT, id=1:2)) # fdcast
%     #  user  system elapsed 
%     # 0.693   0.525   1.295
% 	system.time(dcast(DF, id=1:2))  # dcast from reshape2
%     #   user  system elapsed 
%     # 74.985   6.825  91.904
% 	}
}

% dcast(DT, id=c("i1", "f1"), measure=c("d1", "i2")) # i2 coerced to date
% dcast(DT, id=c("i1", "f1"), measure=c("i2", "d1")) # date is coerced to i2

\examples{
require(data.table)
require(reshape2)
names(ChickWeight) <- tolower(names(ChickWeight))
DT <- melt(as.data.table(ChickWeight), id=2:4) # calls melt.data.table

# no S3 method yet, have to use "dcast.data.table"
dcast.data.table(DT, time ~ variable, fun=mean)
dcast.data.table(DT, diet ~ variable, fun=mean)
dcast.data.table(DT, diet+chick ~ time, drop=FALSE)
dcast.data.table(DT, diet+chick ~ time, drop=FALSE, fill=0)

# using subset
dcast.data.table(DT, chick ~ time, fun=mean, subset=.(time < 10 & chick < 20))

# on big data
set.seed(45)
DT <- data.table(aa=sample(1e4, 1e6, TRUE), 
      bb=sample(1e3, 1e6, TRUE), 
      cc = sample(letters, 1e6, TRUE), dd=runif(1e6))
system.time(dcast.data.table(DT, aa ~ cc, fun=sum)) # 0.59 seconds
system.time(dcast.data.table(DT, bb ~ cc, fun=mean)) # 0.26 seconds
# reshape2:::dcast takes 192.1 seconds
system.time(dcast.data.table(DT, aa + bb ~ cc, fun=sum)) # 3.6 seconds
}
\seealso{
  \code{\link{melt.data.table}}, \url{https://r-forge.r-project.org/projects/datatable/}
}
\keyword{data}

