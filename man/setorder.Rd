\name{setorder}
\alias{setorder}
\alias{setorderv}
\alias{order}
\alias{fastorder}
\alias{forder}
\alias{forderv}
\alias{sort_by}

\title{Fast row reordering of a data.table by reference}
\description{
In \code{data.table} parlance, all \code{set*} functions change their input
\emph{by reference}. That is, no copy is made at all, other than temporary
working memory, which is as large as one column. The only other
\code{data.table} operator that modifies input by reference is \code{\link{:=}}.
Check out the \code{See Also} section below for other \code{set*} function
\code{data.table} provides.

\code{setorder} (and \code{setorderv}) reorders the rows of a \code{data.table}
based on the columns (and column order) provided. It reorders the table
\emph{by reference} and is therefore very memory efficient.

Note that queries like \code{x[order(.)]} are optimised internally to use \code{data.table}'s fast order.

Also note that \code{data.table} always reorders in "C-locale" (see Details). To sort by session locale, use \code{x[base::order(.)]}.

\code{bit64::integer64} type is also supported for reordering rows of a \code{data.table}.
}

\usage{
setorder(x, \dots, na.last=FALSE)
setorderv(x, cols = colnames(x), order=1L, na.last=FALSE)
# optimised to use data.table's internal fast order
# x[order(., na.last=TRUE)]
# x[order(., decreasing=TRUE)]
# sort_by(x, ., na.last=TRUE, decreasing=FALSE)    # R >= 4.4.0
}
\arguments{
\item{x}{ A \code{data.table}. }
\item{\dots}{ The columns to sort by. Do not quote column names. If \code{\dots}
is missing (ex: \code{setorder(x)}), \code{x} is rearranged based on all
columns in ascending order by default. To sort by a column in descending order
prefix the symbol \code{"-"} which means "descending" (\emph{not} "negative", in this context), i.e., \code{setorder(x, a, -b, c)}. The \code{-b} works
when \code{b} is of type \code{character} as well. }
\item{cols}{ A character vector of column names of \code{x} by which to order. By default, sorts over all columns; \code{cols = NULL} will return \code{x} untouched. Do not add \code{"-"} here. Use \code{order} argument instead. }
\item{order}{ An integer vector with only possible values of \code{1} and
\code{-1}, corresponding to ascending and descending order. The length of
\code{order} must be either \code{1} or equal to that of \code{cols}. If
\code{length(order) == 1}, it is recycled to \code{length(cols)}. }
\item{na.last}{ \code{logical}. If \code{TRUE}, missing values in the data are placed last; if \code{FALSE}, they are placed first; if \code{NA} they are removed.
\code{na.last=NA} is valid only for \code{x[order(., na.last)]} and related \code{sort_by(x, .)} (\R \ifelse{html}{\out{&ge;}}{\eqn{\ge}} 4.4.0) and its
default is \code{TRUE}. \code{setorder} and \code{setorderv} only accept
\code{TRUE}/\code{FALSE} with default \code{FALSE}. }
}
\details{
\code{data.table} implements its own fast radix-based ordering. See the references for some exposition on the concept of radix sort.

\code{setorder} accepts unquoted column names (with names preceded with a
\code{-} sign for descending order) and reorders \code{data.table} rows
\emph{by reference}, for e.g., \code{setorder(x, a, -b, c)}. We emphasize that
this means "descending" and not "negative" because the implementation simply
reverses the sort order, as opposed to sorting the opposite of the input
(which would be inefficient).

Note that \code{-b} also works with columns of type \code{character} unlike
\code{\link[base]{order}}, which requires \code{-xtfrm(y)} instead (which is slow).
\code{setorderv} in turn accepts a character vector of column names and an
integer vector of column order separately.

Note that \code{\link{setkey}} still requires and will always sort only in
ascending order, and is different from \code{setorder} in that it additionally
sets the \code{sorted} attribute.

\code{na.last} argument, by default, is \code{FALSE} for \code{setorder} and
\code{setorderv} to be consistent with \code{data.table}'s \code{setkey} and
is \code{TRUE} for \code{x[order(.)]} and \code{sort_by(x, .)} (\R \ifelse{html}{\out{&ge;}}{\eqn{\ge}} 4.4.0) to be consistent with \code{base::order}.
Only \code{x[order(.)]} (and related \code{sort_by(x, .)}) can have \code{na.last = NA} as it is a subset operation
as opposed to \code{setorder} or \code{setorderv} which reorders the data.table
by reference.

\code{data.table} always reorders in "C-locale".
As a consequence, the ordering may be different to that obtained by \code{base::order}.
In English locales, for example, sorting is case-sensitive in C-locale.
Thus, sorting \code{c("c", "a", "B")} returns \code{c("B", "a", "c")} in \code{data.table}
 but \code{c("a", "B", "c")} in \code{base::order}. Note this makes no difference in most cases
of data; both return identical results on ids where only upper-case or lower-case letters are present (\code{"AB123" < "AC234"}
is true in both), or on country names and other proper nouns which are consistently capitalized.
For example, neither \code{"America" < "Brazil"} nor
\code{"america" < "brazil"} are affected since the first letter is consistently
capitalized.

Using C-locale makes the behaviour of sorting in \code{data.table} more consistent across sessions and locales.
The behaviour of \code{base::order} depends on assumptions about the locale of the R session.
In English locales, \code{"america" < "BRAZIL"} is true by default
but false if you either type \code{Sys.setlocale(locale="C")} or the R session has been started in a C locale
for you -- which can happen on servers/services since the locale comes from the environment the R session
was started in. By contrast, \code{"america" < "BRAZIL"} is always \code{FALSE} in \code{data.table} regardless of the way your R session was started.

If \code{setorder} results in reordering of the rows of a keyed \code{data.table},
then its key will be set to \code{NULL}.

Starting from \R 4.4.0, \code{sort_by(x, y, \dots)} is the S3 method for the generic \code{sort_by} for \code{data.table}'s. 
It uses the same formula or list interfaces as data.frame's \code{sort_by} but internally uses \code{data.table}'s fast ordering, 
hence it behaves the same as \code{x[order(.)]} and takes the same optional named arguments and their defaults.

}
\value{
The input is modified by reference, and returned (invisibly) so it can be used
in compound statements; e.g., \code{setorder(DT,a,-b)[, cumsum(c), by=list(a,b)]}.
If you require a copy, take a copy first (using \code{DT2 = copy(DT)}). See
\code{\link{copy}}.
}
\references{
  \url{https://en.wikipedia.org/wiki/Radix_sort}\cr
  \url{https://en.wikipedia.org/wiki/Counting_sort}\cr
  \url{http://stereopsis.com/radix.html}\cr
  \url{https://codercorner.com/RadixSortRevisited.htm}\cr
  \url{https://medium.com/basecs/getting-to-the-root-of-sorting-with-radix-sort-f8e9240d4224}
}
\seealso{
  \code{\link{setkey}}, \code{\link{setcolorder}}, \code{\link{setattr}},
  \code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setDT}},
  \code{\link{setDF}}, \code{\link{copy}}, \code{\link{setNumericRounding}}
}
\examples{

set.seed(45L)
DT = data.table(A=sample(3, 10, TRUE),
         B=sample(letters[1:3], 10, TRUE), C=sample(10))

# setorder
setorder(DT, A, -B)

# same as above, but using setorderv
setorderv(DT, c("A", "B"), c(1, -1))
}
\keyword{ data }

