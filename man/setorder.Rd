\name{setorder}
\alias{setorder}
\alias{setorderv}
\title{Fast row reordering of a data.table by reference}
\description{
  \code{setorder} (and \code{setorderv}) reorders the rows of a \code{data.table} based on the columns (and column order) provided. It reorders the table \emph{by reference} and is therefore very memory efficient.
}
\usage{
  setorder(x, \dots, na.last=FALSE)
  setorderv(x, cols = colnames(x), order=1L, na.last=FALSE, neworder)
}
\arguments{
  \item{x}{ A \code{data.table}. }
  \item{\dots}{ The columns to sort by. Do not quote column names. If \code{\dots} is missing (ex: \code{setorder(x)}), \code{x} is rearranged based on all columns in ascending order by default. To sort by a column in descending order prefix the symbol \code{"-"} which means \emph{descending} (not \emph{negative}, in this context), i.e., \code{setorder(x, a, -b, c)}. The \code{-b} works when \code{b} is of type \code{character} as well. }
  \item{cols}{ A character vector of column names of \code{x} by which to order. By default, sorts over all columns; \code{cols = NULL} will return \code{x} untouched. Do not add \code{"-"} here. Use \code{order} argument instead. }
  \item{order}{ An integer vector with only possible values of \code{1} and \code{-1}, corresponding to ascending and descending order. The length of \code{order} must be either \code{1} or equal to that of \code{cols}. If \code{length(order) == 1}, it is recycled to \code{length(cols)}. }
  \item{na.last}{ \code{logical}. If \code{TRUE}, missing values in the data are placed last; if \code{FALSE} (default), they are placed first. }
  \item{neworder}{ Integer vector, use custom order rather than order based on columns. This has to be strict permutation of \code{1:nrow(x)}, no repeats, zeros, NAs, also known as a \emph{shuffle}. }
}
\details{
  \code{setorder} accepts unquoted column names (with names preceded with a \code{-} sign for descending order) and reorders \code{data.table} rows
\emph{by reference}, for e.g., \code{setorder(x, a, -b, c)}. We emphasize that this means \emph{descending} and not \emph{negative} because the implementation simply reverses the sort order, as opposed to sorting the opposite of the input (which would be inefficient).

  Note that \code{-b} also works with columns of type \code{character} unlike \code{\link[base]{order}}, which requires \code{-xtfrm(y)} instead (which is slow).

  \code{setorderv} in turn accepts a character vector of column names and an integer vector of column order separately.

  Note that \code{\link{setkey}} still requires and will always sort only in ascending order, and is different from \code{setorder} in that it additionally sets the \code{sorted} attribute.
}
\note{
  \code{data.table} always reorders in \emph{C-locale}, see \code{\link{forder}} for details. To reorder by session locale, use \code{setorderv(DT, neworder=base::order(.))}.
}
\value{
  The input is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setorder(DT,a,-b)[, cumsum(c), by=list(a,b)]}. If you require a copy, take a copy first (using \code{DT2 = copy(DT)}). See \code{\link{copy}}.
  If \code{setorder} results in reordering of the rows of a keyed \code{data.table}, then its \emph{key} will be set to \code{NULL}.
}
\seealso{
  \code{\link{forder}}, \code{\link{setkey}}, \code{\link{setcolorder}}, \code{\link{copy}}
}
\examples{
set.seed(45L)
DT = data.table(A=sample(3, 10, TRUE),
         B=sample(letters[1:3], 10, TRUE), C=sample(10))

# setorder
setorder(DT, A, -B)

# same as above, but using setorderv
setorderv(DT, c("A", "B"), c(1, -1))

# neworder
DT = data.table(id1 = c("a","b","c","d"), v1 = rnorm(4))

# move first row to the end
setorderv(DT, neworder = c(2:4,1L))

# random order
setorderv(DT, neworder = sample.int(nrow(DT)))
}
\keyword{ data }

