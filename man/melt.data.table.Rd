\name{melt.data.table}
\alias{melt.data.table}
\alias{melt}
\title{Fast melt for data.table}
\description{
  A \code{melt.data.table} S3 method extending \code{reshape2:::melt}, for melting a \code{data.table}. \code{reshape2} also has to be loaded for using \code{melt.data.table}. A lot similar to \code{reshape2:::melt.data.frame} , but much faster and with some additional features.
}
\usage{
## fast melt a data.table
\method{melt}{data.table}(data, id.vars = NULL, measure.vars = NULL, 
	variable.name = "variable", value.name = "value", 
	..., na.rm = FALSE, variable.factor = TRUE, 
	value.factor = FALSE, 
	verbose = getOption("datatable.verbose"))
}
\arguments{
  \item{data}{ A \code{data.table} object to melt.}
  \item{id.vars}{vector of id variables. Can be integer (corresponding id column numbers) or character (id column names) vector. If \code{NULL}, all non-measure columns will be assigned to it.}
  \item{measure.vars}{vector of measure variables. Can be integer (corresponding measue column numbers) or character (measure column names) vector. If \code{NULL}, all non-id columns will be assigned to it.}
  \item{variable.name}{name for the measured variable names column. The default name is 'variable'.}
  \item{value.name}{name for the molten data values column. The default name is 'value'.}
  \item{na.rm}{If \code{TRUE}, \code{NA} values will be removed from the molten data.}
  \item{variable.factor}{If \code{TRUE}, the \code{variable} column will be converted to \code{factor}, else it will be a \code{character} column.}
  \item{value.factor}{If \code{TRUE}, the \code{value} column will be converted to \code{factor}, else the molten value type is left unchanged.}
  \item{verbose}{\code{TRUE} turns on status and information messages to the console. Turn this on by default using \code{options(datatable.verbose=TRUE)}. The quantity and types of verbosity may be expanded in future.}
  \item{...}{any other arguments to be passed to/from other methods}
}
\details{
If \code{id.vars} and \code{measure.vars} are both \code{NULL}, all non-\code{numeric/integer/logical} columns are assigned as id variables and the rest of the columns are assigned as measure variables. If only one of \code{id.vars} or \code{measure.vars} is supplied, the rest of the columns will be assigned to the other. Both \code{id.vars} and \code{measure.vars} can have the same column more than once and same column can be as id and measure variables. 

\code{melt.data.table} also accepts \code{list} columns for both id and measure variables. When all \code{measure.vars} are not of the same type, they'll be coerced according to the hierarchy \code{list} > \code{character} > \code{numeric > integer > logical}. For example, any of the measure variables is a \code{list}, then entire value column will be coerced to a list. Note that, if the type of \code{value} column is a list, \code{na.rm = TRUE} will have no effect.

All class attributes on \code{value} column (example: \code{Date}) are dropped silently.
}
\value{
    An unkeyed \code{data.table} containing the molten data.
}

\note{
\bold{Differences between \code{melt.data.table} and \code{reshape2:::melt.data.frame}:}
\enumerate{
	\item There are two other arguments \code{variable.factor} and \code{value.factor} which for backwards compatibility with \code{reshape2:::melt.data.frame} is set to \code{TRUE} and \code{FALSE} respectively.
	\item \code{melt.data.table} can handle list columns in both \code{id} and \code{measure} vairables. The molten data retains list columns as such. As long as at least one \code{measure.vars} is a \code{list}, the \code{value} column of molten data will be a \code{list}.
	\item \code{melt(data, id=integer(0), measure=integer(0))} gives a data.table with 0 rows and 2 columns - \code{variable} and \code{value} (default names), as opposed to \code{reshape2:::melt.data.frame} which gives a \code{0} columns and \code{nrow(data)} rows.
}
% \bold{Speedup using \code{fmelt}:}
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
%     system.time(fmelt(DT, id=1:2)) # fmelt
%     #  user  system elapsed 
%     # 0.693   0.525   1.295
% 	system.time(melt(DF, id=1:2))  # melt from reshape2
%     #   user  system elapsed 
%     # 74.985   6.825  91.904
% 	}
}

% melt(DT, id=c("i1", "f1"), measure=c("d1", "i2")) # i2 coerced to date
% melt(DT, id=c("i1", "f1"), measure=c("i2", "d1")) # date is coerced to i2

\examples{
set.seed(45)
require(reshape2)
require(data.table)
DT <- data.table(
      i1 = c(1:5, NA), 
      i2 = c(NA,6,7,8,9,10), 
      f1 = factor(sample(c(letters[1:3], NA), 6, TRUE)), 
      c1 = sample(c(letters[1:3], NA), 6, TRUE), 
      d1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"), 
      d2 = as.Date(6:1, origin="2012-01-01"))
DT[, l1 := DT[, list(c=list(rep(i1, sample(5,1)))), by = i1]$c] # list cols
DT[, l2 := DT[, list(c=list(rep(c1, sample(5,1)))), by = i1]$c]

# basic examples
melt(DT, id=1:2, measure=3) 
melt(DT, id=c("i1", "i2"), measure="f1", value.factor=TRUE) # same as above, but value is factor

# on Date
melt(DT, id=c("i1", "f1"), measure=c("d1", "d2")) # date class attribute lost
melt(DT, id=c("i1", "f1"), measure=c("c1", "d1")) # value is char, date attribute lost

# on list
melt(DT, id=1, measure=c("l1", "l2")) # value is a list
melt(DT, id=1, measure=c("c1", "l1")) # c1 coerced to list

# on character
melt(DT, id=1, measure=c("c1", "f1")) # value is char
melt(DT, id=1, measure=c("c1", "i2")) # i2 coerced to char

# on na.rm=TRUE
melt(DT, id=1, measure=c("c1", "i2"), na.rm=TRUE) # remove NA
}
\seealso{
  \code{\link{dcast.data.table}}, \url{https://r-forge.r-project.org/projects/datatable/}
}
\keyword{ data }

