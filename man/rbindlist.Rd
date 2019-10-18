\name{rbindlist}
\alias{rbindlist}
\alias{rbind.data.table}
\alias{rbind}
\title{ Makes one data.table from a list of many }
\description{
  Same as \code{do.call("rbind", l)} on \code{data.frame}s, but much faster.
}
\usage{
rbindlist(l, use.names="check", fill=FALSE, idcol=NULL)
# rbind(..., use.names=TRUE, fill=FALSE, idcol=NULL)
}
\arguments{
  \item{l}{ A list containing \code{data.table}, \code{data.frame} or \code{list} objects. \code{\dots} is the same but you pass the objects by name separately. }
  \item{use.names}{\code{TRUE} binds by matching column name, \code{FALSE} by position. `check` (default) warns if all items don't have the same names in the same order and then currently proceeds as if `use.names=FALSE` for backwards compatibility (\code{TRUE} in future); see news for v1.12.2.}
  \item{fill}{\code{TRUE} fills missing columns with NAs. By default \code{FALSE}. When \code{TRUE}, \code{use.names} is set to \code{TRUE}.}
  \item{idcol}{Creates a column in the result showing which list item those rows came from. \code{TRUE} names this column \code{".id"}. \code{idcol="file"} names this column \code{"file"}. If the input list has names, those names are the values placed in this id column, otherwise the values are an integer vector \code{1:length(l)}. See \code{examples}.}
}
\details{
Each item of \code{l} can be a \code{data.table}, \code{data.frame} or \code{list}, including \code{NULL} (skipped) or an empty object (0 rows). \code{rbindlist} is most useful when there are an unknown number of (potentially many) objects to stack, such as returned by \code{lapply(fileNames, fread)}. \code{rbind} is most useful to stack two or three objects which you know in advance. \code{\dots} should contain at least one \code{data.table} for \code{rbind(\dots)} to call the fast method and return a \code{data.table}, whereas \code{rbindlist(l)} always returns a \code{data.table} even when stacking a plain \code{list} with a \code{data.frame}, for example.

Columns with duplicate names are bound in the order of occurrence, similar to base. The position (column number) that each duplicate name occurs is also retained.

If column \code{i} does not have the same type in each of the list items; e.g, the column is \code{integer} in item 1 while others are \code{numeric}, they are coerced to the highest type.

If a column contains factors then a factor is created. If any of the factors are also ordered factors then the longest set of ordered levels are found (the first if this is tied). Then the ordered levels from each list item are checked to be an ordered subset of these longest levels. If any ambiguities are found (e.g. \code{blue<green} vs \code{green<blue}), or any ordered levels are missing from the longest, then a regular factor is created with warning. Any strings in regular factor and character columns which are missing from the longest ordered levels are added at the end.
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

