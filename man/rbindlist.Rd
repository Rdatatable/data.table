\name{rbindlist}
\alias{rbindlist}
\alias{rbind.data.table}
\alias{rbind}
\title{ Makes one data.table from a list of many }
\description{
  Same as \code{do.call("rbind", l)} on \code{data.frame}s, but much faster. See \code{DETAILS} for more.
}
\usage{
rbindlist(l, use.names=fill, fill=FALSE, idcol=NULL)
# rbind(\dots, use.names=TRUE, fill=FALSE, idcol=NULL)
}
\arguments{
  \item{l}{ A list containing \code{data.table}, \code{data.frame} or \code{list} objects. At least one of the inputs should have column names set. \code{\dots} is the same but you pass the objects by name separately. }
  \item{use.names}{If \code{TRUE} items will be bound by matching column names. By default \code{FALSE} for \code{rbindlist} (for backwards compatibility) and \code{TRUE} for \code{rbind} (consistency with base). Columns with duplicate names are bound in the order of occurrence, similar to base. When TRUE, at least one item of the input list has to have non-null column names.}
  \item{fill}{If \code{TRUE} fills missing columns with NAs. By default \code{FALSE}. When \code{TRUE}, \code{use.names} has to be \code{TRUE}, and all items of the input list has to have non-null column names. }
  \item{idcol}{Generates an index column. Default (\code{NULL}) is not to. If \code{idcol=TRUE} then the column is auto named \code{.id}. Alternatively the column name can be directly provided, e.g., \code{idcol = "id"}.

  If input is a named list, ids are generated using them, else using integer vector from \code{1} to length of input list. See \code{examples}.}
}
\details{
Each item of \code{l} can be a \code{data.table}, \code{data.frame} or \code{list}, including \code{NULL} (skipped) or an empty object (0 rows). \code{rbindlist} is most useful when there are a variable number of (potentially many) objects to stack, such as returned by \code{lapply(fileNames, fread)}. \code{rbind} however is most useful to stack two or three objects which you know in advance. \code{\dots} should contain at least one \code{data.table} for \code{rbind(\dots)} to call the fast method and return a \code{data.table}, whereas \code{rbindlist(l)} always returns a \code{data.table} even when stacking a plain \code{list} with a \code{data.frame}, for example.

In versions \code{<= v1.9.2}, each item for \code{rbindlist} should have the same number of columns as the first non empty item. \code{rbind.data.table} gained a \code{fill} argument to fill missing columns with \code{NA} in \code{v1.9.2}, which allowed for \code{rbind(\dots)} binding unequal number of columns.

In version \code{> v1.9.2}, these functionalities were extended to \code{rbindlist} (and written entirely in C for speed). \code{rbindlist} has \code{use.names} argument, which is set to \code{FALSE} by default for backwards compatibility. It also contains \code{fill} argument as well and can bind unequal columns when set to \code{TRUE}.

With these changes, the only difference between \code{rbind(\dots)} and \code{rbindlist(l)} is their \emph{default argument} \code{use.names}.

If column \code{i} of input items do not all have the same type; e.g, a \code{data.table} may be bound with a \code{list} or a column is \code{factor} while others are \code{character} types, they are coerced to the highest type (SEXPTYPE).

Note that any additional attributes that might exist on individual items of the input list would not be preserved in the result.
}
\value{
    An unkeyed \code{data.table} containing a concatenation of all the items passed in.
}
\seealso{ \code{\link{data.table}}, \code{\link{split.data.table}} }
\examples{
# default case
DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(A=4:5,B=letters[4:5])
l = list(DT1,DT2)
rbindlist(l)

# bind correctly by names
DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(B=letters[4:5],A=4:5)
l = list(DT1,DT2)
rbindlist(l, use.names=TRUE)

# fill missing columns, and match by col names
DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(B=letters[4:5],C=factor(1:2))
l = list(DT1,DT2)
rbindlist(l, use.names=TRUE, fill=TRUE)

# generate index column, auto generates indices
rbindlist(l, use.names=TRUE, fill=TRUE, idcol=TRUE)
# let's name the list
setattr(l, 'names', c("a", "b"))
rbindlist(l, use.names=TRUE, fill=TRUE, idcol="ID")

}
\keyword{ data }

