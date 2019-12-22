\name{melt.data.table}
\alias{melt.data.table}
\alias{melt}
\title{Fast melt for data.table}
\description{
\code{melt} is \code{data.table}'s wide-to-long reshaping tool.
We provide an S3 method for melting \code{data.table}s. It is written in C for speed and memory
efficiency. Since \code{v1.9.6}, \code{melt.data.table} allows melting into
multiple columns simultaneously.
}
\usage{
## fast melt a data.table
\method{melt}{data.table}(data, id.vars, measure.vars,
    variable.name = "variable", value.name = "value",
    \dots, na.rm = FALSE, variable.factor = TRUE,
    value.factor = FALSE,
    verbose = getOption("datatable.verbose"))
}
\arguments{
\item{data}{ A \code{data.table} object to melt.}
\item{id.vars}{vector of id variables. Can be integer (corresponding id
column numbers) or character (id column names) vector. If missing, all
non-measure columns will be assigned to it. If integer, must be positive; see Details. }
\item{measure.vars}{Measure variables for \code{melt}ing. Can be missing, vector, list, or pattern-based.

  \itemize{
    \item{ When missing, \code{measure.vars} will become all columns outside \code{id.vars}. }
    \item{ Vector can be \code{integer} (implying column numbers) or \code{character} (column names). }
    \item{ \code{list} is a generalization of the vector version -- each element of the list (which should be \code{integer} or \code{character} as above) will become a \code{melt}ed column. }
    \item{ Pattern-based column matching can be achieved with the regular expression-based \code{\link{patterns}} syntax; multiple patterns will produce multiple columns. }
  }

    For convenience/clarity in the case of multiple \code{melt}ed columns, resulting column names can be supplied as names to the elements \code{measure.vars} (in the \code{list} and \code{patterns} usages). See also \code{Examples}. }
\item{variable.name}{name for the measured variable names column. The default name is \code{'variable'}.}
\item{value.name}{name for the molten data values column(s). The default name is \code{'value'}. Multiple names can be provided here for the case when \code{measure.vars} is a \code{list}, though note well that the names provided in \code{measure.vars} take precedence. }
\item{na.rm}{If \code{TRUE}, \code{NA} values will be removed from the molten
data.}
\item{variable.factor}{If \code{TRUE}, the \code{variable} column will be
converted to \code{factor}, else it will be a \code{character} column.}
\item{value.factor}{If \code{TRUE}, the \code{value} column will be converted
to \code{factor}, else the molten value type is left unchanged.}
\item{verbose}{\code{TRUE} turns on status and information messages to the
console. Turn this on by default using \code{options(datatable.verbose=TRUE)}.
The quantity and types of verbosity may be expanded in future.}
\item{\dots}{any other arguments to be passed to/from other methods.}
}
\details{
If \code{id.vars} and \code{measure.vars} are both missing, all
non-\code{numeric/integer/logical} columns are assigned as id variables and
the rest as measure variables. If only one of \code{id.vars} or
\code{measure.vars} is supplied, the rest of the columns will be assigned to
the other. Both \code{id.vars} and \code{measure.vars} can have the same column
more than once and the same column can be both as id and measure variables.

\code{melt.data.table} also accepts \code{list} columns for both id and measure
variables.

When all \code{measure.vars} are not of the same type, they'll be coerced
according to the hierarchy \code{list} > \code{character} > \code{numeric >
integer > logical}. For example, if any of the measure variables is a
\code{list}, then entire value column will be coerced to a list. Note that,
if the type of \code{value} column is a list, \code{na.rm = TRUE} will have no
effect.

From version \code{1.9.6}, \code{melt} gains a feature with \code{measure.vars}
accepting a list of \code{character} or \code{integer} vectors as well to melt
into multiple columns in a single function call efficiently. The function
\code{\link{patterns}} can be used to provide regular expression patterns. When
used along with \code{melt}, if \code{cols} argument is not provided, the
patterns will be matched against \code{names(data)}, for convenience.

Attributes are preserved if all \code{value} columns are of the same type. By
default, if any of the columns to be melted are of type \code{factor}, it'll
be coerced to \code{character} type. To get a \code{factor} column, set
\code{value.factor = TRUE}. \code{melt.data.table} also preserves
\code{ordered} factors.

Historical note: \code{melt.data.table} was originally designed as an enhancement to \code{reshape2::melt} in terms of computing and memory efficiency. \code{reshape2} has since been deprecated, and \code{melt} has had a generic defined within \code{data.table} since \code{v1.9.6} in 2015, at which point the dependency between the packages became more etymological than programmatic. We thank the \code{reshape2} authors for the inspiration.

}

\value{
An unkeyed \code{data.table} containing the molten data.
}

\examples{
set.seed(45)
require(data.table)
DT <- data.table(
      i_1 = c(1:5, NA),
      i_2 = c(NA,6,7,8,9,10),
      f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
      f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
      c_1 = sample(c(letters[1:3], NA), 6, TRUE),
      d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
      d_2 = as.Date(6:1, origin="2012-01-01"))
# add a couple of list cols
DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]

# id, measure as character/integer/numeric vectors
melt(DT, id=1:2, measure="f_1")
melt(DT, id=c("i_1", "i_2"), measure=3) # same as above
melt(DT, id=1:2, measure=3L, value.factor=TRUE) # same, but 'value' is factor
melt(DT, id=1:2, measure=3:4, value.factor=TRUE) # 'value' is *ordered* factor

# preserves attribute when types are identical, ex: Date
melt(DT, id=3:4, measure=c("d_1", "d_2"))
melt(DT, id=3:4, measure=c("i_1", "d_1")) # attribute not preserved

# on list
melt(DT, id=1, measure=c("l_1", "l_2")) # value is a list
melt(DT, id=1, measure=c("c_1", "l_1")) # c1 coerced to list

# on character
melt(DT, id=1, measure=c("c_1", "f_1")) # value is char
melt(DT, id=1, measure=c("c_1", "i_2")) # i2 coerced to char

# on na.rm=TRUE. NAs are removed efficiently, from within C
melt(DT, id=1, measure=c("c_1", "i_2"), na.rm=TRUE) # remove NA

# measure.vars can be also a list
# melt "f_1,f_2" and "d_1,d_2" simultaneously, retain 'factor' attribute
# convenient way using internal function patterns()
melt(DT, id=1:2, measure=patterns("^f_", "^d_"), value.factor=TRUE)
# same as above, but provide list of columns directly by column names or indices
melt(DT, id=1:2, measure=list(3:4, c("d_1", "d_2")), value.factor=TRUE)
# same as above, but provide names directly:
melt(DT, id=1:2, measure=patterns(f="^f_", d="^d_"), value.factor=TRUE)

# na.rm=TRUE removes rows with NAs in any 'value' columns
melt(DT, id=1:2, measure=patterns("f_", "d_"), value.factor=TRUE, na.rm=TRUE)

# return 'NA' for missing columns, 'na.rm=TRUE' ignored due to list column
melt(DT, id=1:2, measure=patterns("l_", "c_"), na.rm=TRUE)

}
\seealso{
  \code{\link{dcast}}, \url{https://cran.r-project.org/package=reshape}
}
\keyword{ data }

