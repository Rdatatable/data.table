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
\item{variable.name}{name (default \code{'variable'}) of output column containing information about which input column(s) were melted. If \code{measure.vars} is an integer/character vector, then each entry of this column contains the name of a melted column from \code{data}. If \code{measure.vars} is a list of integer/character vectors, then each entry of this column contains an integer indicating an index/position in each of those vectors. If \code{measure.vars} has attribute \code{variable_table} then it must be a data table with nrow = length of \code{measure.vars} vector(s), each row describing the corresponding measured variables(s), (typically created via \code{measure}) and its columns will be output instead of the \code{variable.name} column.}
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
\code{list}, then entire value column will be coerced to a list. 

From version \code{1.9.6}, \code{melt} gains a feature with \code{measure.vars}
accepting a list of \code{character} or \code{integer} vectors as well to melt
into multiple columns in a single function call efficiently.
If a vector in the list contains missing values, or is shorter than the
max length of the list elements, then the output will include runs of
missing values at the specified position, or at the end.
The function
\code{\link{patterns}} can be used to provide regular expression patterns. When
used along with \code{melt}, if \code{cols} argument is not provided, the
patterns will be matched against \code{names(data)}, for convenience.

Attributes are preserved if all \code{value} columns are of the same type. By
default, if any of the columns to be melted are of type \code{factor}, it'll
be coerced to \code{character} type. To get a \code{factor} column, set
\code{value.factor = TRUE}. \code{melt.data.table} also preserves
\code{ordered} factors.

Historical note: \code{melt.data.table} was originally designed as an enhancement to \code{reshape2::melt} in terms of computing and memory efficiency. \code{reshape2} has since been superseded in favour of \code{tidyr}, and \code{melt} has had a generic defined within \code{data.table} since \code{v1.9.6} in 2015, at which point the dependency between the packages became more etymological than programmatic. We thank the \code{reshape2} authors for the inspiration.

}

\value{
An unkeyed \code{data.table} containing the molten data.
}

\examples{
set.seed(45)
require(data.table)
DT <- data.table(
  i_1 = c(1:5, NA),
  n_1 = c(NA, 6, 7, 8, 9, 10),
  f_1 = factor(sample(c(letters[1:3], NA), 6L, TRUE)),
  f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
  c_1 = sample(c(letters[1:3], NA), 6L, TRUE),
  c_2 = sample(c(LETTERS[1:2], NA), 6L, TRUE),
  d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
  d_2 = as.Date(6:1, origin="2012-01-01")
)
# add a couple of list cols
DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5, 1L)))), by = i_1]$c]
DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5, 1L)))), by = i_1]$c]

# id.vars, measure.vars as character/integer/numeric vectors
melt(DT, id.vars=1:2, measure.vars="f_1")
melt(DT, id.vars=c("i_1", "n_1"), measure.vars=3) # same as above
melt(DT, id.vars=1:2, measure.vars=3L, value.factor=TRUE) # same, but 'value' is factor
melt(DT, id.vars=1:2, measure.vars=3:4, value.factor=TRUE) # 'value' is *ordered* factor

# preserves attribute when types are identical, ex: Date
melt(DT, id.vars=3:4, measure.vars=c("d_1", "d_2"))
melt(DT, id.vars=3:4, measure.vars=c("n_1", "d_1")) # attribute not preserved

# on list
melt(DT, id.vars=1, measure.vars=c("l_1", "l_2")) # value is a list
suppressWarnings(
  melt(DT, id.vars=1, measure.vars=c("c_1", "l_1")) # c1 coerced to list, with warning
)

# on character
melt(DT, id.vars=1, measure.vars=c("c_1", "f_1")) # value is char
suppressWarnings(
  melt(DT, id.vars=1, measure.vars=c("c_1", "n_1")) # n_1 coerced to char, with warning
)

# on na.rm=TRUE. NAs are removed efficiently, from within C
melt(DT, id.vars=1, measure.vars=c("c_1", "c_2"), na.rm=TRUE) # remove NA

# measure.vars can be also a list
# melt "f_1,f_2" and "d_1,d_2" simultaneously, retain 'factor' attribute
# convenient way using internal function patterns()
melt(DT, id.vars=1:2, measure.vars=patterns("^f_", "^d_"), value.factor=TRUE)
# same as above, but provide list of columns directly by column names or indices
melt(DT, id.vars=1:2, measure.vars=list(3:4, c("d_1", "d_2")), value.factor=TRUE)
# same as above, but provide names directly:
melt(DT, id.vars=1:2, measure.vars=patterns(f="^f_", d="^d_"), value.factor=TRUE)

# na.rm=TRUE removes rows with NAs in any 'value' columns
melt(DT, id.vars=1:2, measure.vars=patterns("f_", "d_"), value.factor=TRUE, na.rm=TRUE)

# 'na.rm=TRUE' also works with list column, but note that is.na only
# returns TRUE if the list element is a length=1 vector with an NA.
is.na(list(one.NA=NA, two.NA=c(NA,NA)))
melt(DT, id.vars=1:2, measure.vars=patterns("l_", "d_"), na.rm=FALSE)
melt(DT, id.vars=1:2, measure.vars=patterns("l_", "d_"), na.rm=TRUE)

# measure list with missing/short entries results in output with runs of NA
DT.missing.cols <- DT[, .(d_1, d_2, c_1, f_2)]
melt(DT.missing.cols, measure.vars=list(d=1:2, c="c_1", f=c(NA, "f_2")))

# specifying columns to melt via separator.
melt(DT.missing.cols, measure.vars=measure(value.name, number=as.integer, sep="_"))

# specifying columns to melt via regex.
melt(DT.missing.cols, measure.vars=measure(value.name, number=as.integer, pattern="(.)_(.)"))
}
\seealso{
  \code{\link{dcast}}, \url{https://cran.r-project.org/package=reshape}
}
\keyword{ data }

