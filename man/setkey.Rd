\name{setkey}
\alias{setkey}
\alias{setkeyv}
\alias{key}
\alias{haskey}
\alias{setindex}
\alias{setindexv}
\alias{indices}
\title{ Create key on a data.table }
\description{
\code{setkey} sorts a \code{data.table} and marks it as sorted with an
attribute \code{sorted}. The sorted columns are the key. The key can be any
number of columns. The columns are always sorted in \emph{ascending} order. The table
is changed \emph{by reference} and \code{setkey} is very memory efficient.

There are three reasons \code{setkey} is desirable: i) binary search and joins are faster
when they detect they can use an existing key, ii) grouping by a leading subset of the key
columns is faster because the groups are already gathered contiguously in RAM, iii)
simpler shorter syntax; e.g. \code{DT["id",]} finds the group "id" in the first column
of DT's key using binary search. It may be helpful to think of a key as
super-charged rownames: multi-column and multi-type rownames.

In \code{data.table} parlance, all \code{set*} functions change their input
\emph{by reference}. That is, no copy is made at all other than for temporary
working memory, which is as large as one column. The only other \code{data.table}
operator that modifies input by reference is \code{\link{:=}}. Check out the
\code{See Also} section below for other \code{set*} functions \code{data.table}
provides.

\code{setindex} creates an index for the provided columns. This index is simply an
ordering vector of the dataset's rows according to the provided columns. This order vector
is stored as an attribute of the \code{data.table} and the dataset retains the original order
of rows in memory. See the \href{../doc/datatable-secondary-indices-and-auto-indexing.html}{\code{vignette("datatable-secondary-indices-and-auto-indexing")}} for more details.

\code{key} returns the \code{data.table}'s key if it exists; \code{NULL} if none exists.

\code{haskey} returns \code{TRUE}/\code{FALSE} if the \code{data.table} has a key.
}
\usage{
setkey(x, \dots, verbose=getOption("datatable.verbose"), physical = TRUE)
setkeyv(x, cols, verbose=getOption("datatable.verbose"), physical = TRUE)
setindex(\dots)
setindexv(x, cols, verbose=getOption("datatable.verbose"))
key(x)
indices(x, vectors = FALSE)
haskey(x)
}
\arguments{
\item{x}{ A \code{data.table}. }
\item{\dots}{ The columns to sort by. Do not quote the column names. If \code{\dots} is missing (i.e. \code{setkey(DT)}), all the columns are used. \code{NULL} removes the key. }
\item{cols}{ A character vector of column names. For \code{setindexv}, this can be a \code{list} of character vectors, in which case each element will be applied as an index in turn. }
\item{verbose}{ Output status and information. }
\item{physical}{ \code{TRUE} changes the order of the data in RAM. \code{FALSE} adds an index. }
\item{vectors}{ \code{logical} scalar, default \code{FALSE}; when set to \code{TRUE}, a \code{list} of character vectors is returned, each referring to one index. }
}
\details{
\code{setkey} reorders (i.e. sorts) the rows of a \code{data.table} by the columns
provided. The sort method used has developed over the years and we have contributed
to base R too; see \code{\link[base]{sort}}. Generally speaking we avoid any type
of comparison sort (other than insert sort for very small input) preferring instead
counting sort and forwards radix. We also avoid hash tables.

Note that \code{setkey} always uses "C-locale"; see the Details in the help for \code{\link{setorder}} for more on why.

The sort is \emph{stable}; i.e., the order of ties (if any) is preserved.

For character vectors, \code{data.table} takes advantage of R's internal global string cache, also exported as \code{\link{chorder}}.
}

\section{Good practice}{
In general, it's good practice to use column names rather than numbers. This is
why \code{setkey} and \code{setkeyv} only accept column names.
If you use column numbers then bugs (possibly silent) can more easily creep into
your code as time progresses if changes are made elsewhere in your code; e.g., if
you add, remove or reorder columns in a few months time, a \code{setkey} by column
number will then refer to a different column, possibly returning incorrect results
with no warning. (A similar concept exists in SQL, where \code{"select * from ..."} is considered poor programming style when a robust, maintainable system is
required.)

If you really wish to use column numbers, it is possible but
deliberately a little harder; e.g., \code{setkeyv(DT,names(DT)[1:2])}.

If you wanted to use \code{\link[base]{grep}} to select key columns according to
a pattern, note that you can just set \code{value = TRUE} to return a character vector instead of the default integer indices.
}
\value{
The input is modified by reference and returned (invisibly) so it can be used
in compound statements; e.g., \code{setkey(DT,a)[.("foo")]}. If you require a
copy, take a copy first (using \code{DT2=copy(DT)}). \code{\link{copy}} may also
sometimes be useful before \code{:=} is used to subassign to a column by
reference.
}
\references{
  \url{https://en.wikipedia.org/wiki/Radix_sort}\cr
  \url{https://en.wikipedia.org/wiki/Counting_sort}\cr
  \url{http://stereopsis.com/radix.html}\cr
  \url{https://codercorner.com/RadixSortRevisited.htm}\cr
  \url{https://cran.r-project.org/package=bit64}\cr
  \url{https://github.com/Rdatatable/data.table/wiki/Presentations}
}
\seealso{ \code{\link{data.table}}, \code{\link{tables}}, \code{\link{J}},
\code{\link[base:order]{sort.list}}, \code{\link{copy}}, \code{\link{setDT}},
\code{\link{setDF}}, \code{\link{set}} \code{\link{:=}}, \code{\link{setorder}},
\code{\link{setcolorder}}, \code{\link{setattr}}, \code{\link{setnames}},
\code{\link{chorder}}, \code{\link{setNumericRounding}}
}
\examples{
# Type 'example(setkey)' to run these at the prompt and browse output

DT = data.table(A=5:1,B=letters[5:1])
DT # before
setkey(DT,B)          # re-orders table and marks it sorted.
DT # after
tables()              # KEY column reports the key'd columns
key(DT)
keycols = c("A","B")
setkeyv(DT,keycols)

DT = data.table(A=5:1,B=letters[5:1])
DT2 = DT              # does not copy
setkey(DT2,B)         # does not copy-on-write to DT2
identical(DT,DT2)     # TRUE. DT and DT2 are two names for the same keyed table

DT = data.table(A=5:1,B=letters[5:1])
DT2 = copy(DT)        # explicit copy() needed to copy a data.table
setkey(DT2,B)         # now just changes DT2
identical(DT,DT2)     # FALSE. DT and DT2 are now different tables

DT = data.table(A=5:1,B=letters[5:1])
setindex(DT)          # set indices
setindex(DT, A)
setindex(DT, B)
indices(DT)           # get indices single vector
indices(DT, vectors = TRUE) # get indices list
}
\keyword{ data }
