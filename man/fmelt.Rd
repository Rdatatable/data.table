\name{fmelt}
\alias{fmelt}
\alias{fastmelt}
\alias{melt.data.table}
\title{Fast melt of a data.table (or a data.frame) object}
\description{
  Converts a data.table or data.frame into a 'long' format. A lot similar to \code{melt.data.frame} from \code{reshape2} package, but much faster and with additional features. 
}
\usage{
## fast melt a data.table or a data.frame
fmelt(data, id.var = NULL, measure.var = NULL, 
	variable.name = "variable", value.name = "value", 
	..., na.rm = FALSE, variable.factor = TRUE, 
	value.factor = FALSE, 
	verbose = getOption("datatable.verbose"))
}
\arguments{
  \item{data}{ A \code{data.table} (or a \code{data.frame}).}
  \item{id.var}{id vector. Can be integer (corresponding id column numbers) or character (id column names). If \code{NULL}, all non-measure columns will be assigned to it.}
  \item{measure.var}{measure vector. Can be integer (corresponding measue column numbers) or character (measure column names). If \code{NULL}, all non-id columns will be assigned to it.}
  \item{variable.name}{name for the measured variable names column. The default name is 'variable'.}
  \item{value.name}{name for the molten data values column. The default name is 'value'.}
  \item{na.rm}{If \code{TRUE}, \code{NA} values will be removed from the molten data.}
  \item{variable.factor}{If \code{TRUE}, the \code{variable} column will be converted to \code{factor}, else it will be a \code{character} column.}
  \item{value.factor}{If \code{TRUE}, the \code{value} column will be converted to \code{factor}, else the molten value type is left unchanged.}
  \item{verbose}{\code{TRUE} turns on status and information messages to the console. Turn this on by default using \code{options(datatable.verbose=TRUE)}. The quantity and types of verbosity may be expanded in future.}
  \item{...}{any other arguments to be passed to/from other methods}
}
\details{
If \code{id.var} and \code{measure.var} are both \code{NULL}, all \code{factor} and \code{character} columns are assigned as measure variables and the rest of the columns are assigned as id variables. If only one of \code{id.var} or \code{measure.var} is supplied, the rest of the columns will be assigned to the other. Both \code{id.var} and \code{measure.var} can have the same column more than once and same column can be as id and measure variables. 

\code{fmelt} also accepts \code{list} columns for both id and measure variables. If any of the measure variables is a \code{list}, then entire value column will be coerced to a list. In that case, \code{na.rm = TRUE} will have no effect. That is, \code{list} has the highest priority. If any of the measure variable is a \code{factor} or \code{character}, the value column will be coerced to \code{character}. Similarly, following that, \code{numeric > integer > logical}. 

If all the measure variables are of a particular class, whose internal representations are different, for example, say, \code{Date}, the molten result from \code{fmelt} will attempt to preserve the (here: \code{Date}) class. Note that this is not the case in \code{melt} from \code{reshape2}. If a coercion were to happen here, that is, if your measure variables are both, say, integer (or numeric) and Date types, then, the type of the first measure var decides the coercion type. Note that, still, when the measure var, for example, is a character and Date, the Date format is lost and the result will be coerced to character (following the hierarchy).

Even if all \code{measure.var}s are factors, the resulting \code{value} column will by default be a character column. This is unless \code{value.factor = TRUE}.
}
\value{
    An unkeyed \code{data.table} containing the molten data.
}

\note{
\bold{Differences between \code{fmelt} and \code{reshape2:::melt}:}
\enumerate{
	\item There are two other arguments \code{variable.factor} and \code{value.factor} which for backwards compatibility with \code{reshape}'s \code{melt} is set to \code{TRUE} and \code{FALSE} respectively. 
	\item \code{fmelt} can handle list columns in both \code{id} and \code{measure} vairables. The molten data retains list columns as such. \code{list} has highest priority. That is, as long as at least one measure variable is of type list, the \code{value} column of molten data will be a \code{list}.
	\item \code{fmelt(data, id=integer(0), measure=integer(0))} gives a data.table with 0 rows and 2 columns - \code{variable} and \code{value} (default names), as opposed to \code{melt} from \code{reshape2} which gives a \code{0} columns and \code{nrow(data)} rows.
	\item \code{fmelt} retains the class of certain objects like those of class \code{Date} whereever possible (See DETAILS section).
	\item The molten data \code{value} column will not be \code{factor}, even if all \code{measure.var} are \code{factor}, unless \code{value.factor} is set to \code{TRUE}.
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


\seealso{ \code{\link{fcast}} }
\examples{
# generate some data
set.seed(45)
require(data.table)
DT <- data.table(
      i1 = c(1:5, NA), 
      i2 = c(NA,6,7,8,9,10), 
      f1 = factor(sample(c(letters[1:3], NA), 6, TRUE)), 
      c1 = sample(c(letters[1:3], NA), 6, TRUE), 
      d1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"), 
      d2 = as.Date(6:1, origin="2012-01-01"))
DT[, l1 := DT[, list(c=list(rep(i1, sample(5,1)))), by = i1]$c] # generate list cols
DT[, l2 := DT[, list(c=list(rep(c1, sample(5,1)))), by = i1]$c]

# basic examples
fmelt(DT, id=1:2, measure=3) 
fmelt(DT, id=c("i1", "i2"), measure="f1", value.factor=TRUE) # same as above, but value is factor

# on Date
fmelt(DT, id=c("i1", "f1"), measure=c("d1", "d2")) # date class preserved
fmelt(DT, id=c("i1", "f1"), measure=c("d1", "i2")) # i2 coerced to date
fmelt(DT, id=c("i1", "f1"), measure=c("i2", "d1")) # date is coerced to i2

# on list
fmelt(DT, id=1, measure=c("l1", "l2")) # value is a list
fmelt(DT, id=1, measure=c("c1", "l1")) # f1 coerced to list

# on character
fmelt(DT, id=1, measure=c("c1", "f1")) # value is char
fmelt(DT, id=1, measure=c("c1", "i2")) # i2 coerced to char

# on na.rm=TRUE
fmelt(DT, id=1, measure=c("c1", "i2"), na.rm=TRUE) # remove NA
}
\keyword{ data }

