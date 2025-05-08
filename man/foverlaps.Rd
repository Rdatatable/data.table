\name{foverlaps}
\alias{foverlaps}
\title{Fast overlap joins}
\description{
A \emph{fast} binary-search based \emph{overlap join} of two \code{data.table}s.
This is very much inspired by \code{findOverlaps} function from the Bioconductor
package \code{IRanges} (see link below under \code{See Also}).

Usually, \code{x} is a very large data.table with small interval ranges, and
\code{y} is much smaller \emph{keyed} \code{data.table} with relatively larger
interval spans. For a usage in \code{genomics}, see the examples section.

NOTE: This is still under development, meaning it is stable, but some features
are yet to be implemented. Also, some arguments and/or the function name itself
could be changed.
}

\usage{
foverlaps(x, y, by.x = key(x) \%||\% key(y),
    by.y = key(y), maxgap = 0L, minoverlap = 1L,
    type = c("any", "within", "start", "end", "equal"),
    mult = c("all", "first", "last"),
    nomatch = NA,
    which = FALSE, verbose = getOption("datatable.verbose"))
}
\arguments{
\item{x, y}{ \code{data.table}s. \code{y} needs to be keyed, but not necessarily
\code{x}. See examples. }
\item{by.x, by.y}{A vector of column names (or numbers) to compute the overlap
joins. The last two columns in both \code{by.x} and \code{by.y} should each
correspond to the \code{start} and \code{end} interval columns in \code{x} and
\code{y} respectively. We should always have \code{start <= end}.
If \code{x} is keyed,  \code{by.x} is equal to \code{key(x)}, else
\code{key(y)}. \code{by.y} defaults to \code{key(y)}. }
\item{maxgap}{Non-negative integer, i.e., \code{maxgap >= 0}. Default is 0 (no
gap). For intervals \code{[a,b]} and \code{[c,d]}, where \code{a<=b} and
\code{c<=d}, when \code{c > b} or \code{d < a}, the two intervals don't overlap.
If the gap between these two intervals is \code{<= maxgap}, these two intervals
are considered as overlapping. Note: This is not yet implemented.}
\item{minoverlap}{Positive integer, i.e., \code{minoverlap > 0}. Default is 1. For
intervals \code{[a,b]} and \code{[c,d]}, where \code{a<=b} and \code{c<=d}, when
\code{c<=b} and \code{d>=a}, the two intervals overlap. If the length of overlap
between these two intervals is \code{>= minoverlap}, then these two intervals are
considered to be overlapping. Note: This is not yet implemented.}
\item{type}{ Default value is \code{any}. Allowed values are \code{any},
\code{within}, \code{start}, \code{end} and \code{equal}.

The types shown here are identical in functionality to the function
\code{findOverlaps} in the bioconductor package \code{IRanges}. Let \code{[a,b]}
and \code{[c,d]} be intervals in \code{x} and \code{y} with \code{a<=b} and
\code{c<=d}. For \code{type="start"}, the intervals overlap iff \code{a == c}.
For \code{type="end"}, the intervals overlap iff \code{b == d}. For
\code{type="within"}, the intervals overlap iff \code{a>=c and b<=d}. For
\code{type="equal"}, the intervals overlap iff \code{a==c and b==d}. For
\code{type="any"}, as long as \code{c<=b and d>=a}, they overlap. In addition
to these requirements, they also have to satisfy the \code{minoverlap} argument
as explained above.

NB: \code{maxgap} argument, when > 0, is to be interpreted according to the type
of the overlap. This will be updated once \code{maxgap} is implemented.}

\item{mult}{ When multiple rows in \code{y} match to the row in \code{x},
\code{mult=.} controls which values are returned - \code{"all"} (default),
\code{"first"} or \code{"last"}.}
\item{nomatch}{ When a row (with interval say, \code{[a,b]}) in \code{x} has no
match in \code{y}, \code{nomatch=NA} (default) means \code{NA} is returned for
\code{y}'s non-\code{by.y} columns for that row of \code{x}. \code{nomatch=NULL}
(or \code{0} for backward compatibility) means no rows will be returned for that
row of \code{x}. }
\item{which}{ When \code{TRUE}, if \code{mult="all"} returns a two column
\code{data.table} with the first column corresponding to \code{x}'s row number
and the second corresponding to \code{y}'s. When \code{nomatch=NA}, no matches
return \code{NA} for \code{y}, and if \code{nomatch=NULL}, those rows where no
match is found will be skipped; if \code{mult="first" or "last"}, a vector of
length equal to the number of rows in \code{x} is returned, with no-match entries
filled with \code{NA} or \code{0} corresponding to the \code{nomatch} argument.
Default is \code{FALSE}, which returns a join with the rows in \code{y}.}
\item{verbose}{ \code{TRUE} turns on status and information messages to the
console. Turn this on by default using \code{options(datatable.verbose=TRUE)}.
The quantity and types of verbosity may be expanded in future.}
}
\details{
Very briefly, \code{foverlaps()} collapses the two-column interval in \code{y}
to one-column of \emph{unique} values to generate a \code{lookup} table, and
then performs the join depending on the type of \code{overlap}, using the
already available \code{binary search} feature of \code{data.table}. The time
(and space) required to generate the \code{lookup} is therefore proportional
to the number of unique values present in the interval columns of \code{y}
when combined together.

Overlap joins takes advantage of the fact that \code{y} is sorted to speed-up
finding overlaps. Therefore \code{y} has to be keyed (see \code{?setkey})
prior to running \code{foverlaps()}. A key on \code{x} is not necessary,
although it \emph{might} speed things further. The columns in \code{by.x}
argument should correspond to the columns specified in \code{by.y}. The last
two columns should be the \emph{interval} columns in both \code{by.x} and
\code{by.y}. The first interval column in \code{by.x} should always be <= the
second interval column in \code{by.x}, and likewise for \code{by.y}. The
\code{\link{storage.mode}} of the interval columns must be either \code{double}
or \code{integer}. It therefore works with \code{bit64::integer64} type as well.

The \code{lookup} generation step could be quite time consuming if the number
of unique values in \code{y} are too large (ex: in the order of tens of millions).
There might be improvements possible by constructing lookup using RLE, which is
a pending feature request. However most scenarios will not have too many unique
values for \code{y}.
}
\value{
A new \code{data.table} by joining over the interval columns (along with other
additional identifier columns) specified in \code{by.x} and \code{by.y}.

NB: When \code{which=TRUE}: \code{a)} \code{mult="first" or "last"} returns a
\code{vector} of matching row numbers in \code{y}, and \code{b)} when
\code{mult="all"} returns a data.table with two columns with the first
containing row numbers of \code{x} and the second column with corresponding
row numbers of \code{y}.

\code{nomatch=NA|NULL} also influences whether non-matching rows are returned
or not, as explained above.
}

\examples{
require(data.table)
## simple example:
x = data.table(start=c(5,31,22,16), end=c(8,50,25,18), val2 = 7:10)
y = data.table(start=c(10, 20, 30), end=c(15, 35, 45), val1 = 1:3)
setkey(y, start, end)
foverlaps(x, y, type="any", which=TRUE) ## return overlap indices
foverlaps(x, y, type="any") ## return overlap join
foverlaps(x, y, type="any", mult="first") ## returns only first match
foverlaps(x, y, type="within") ## matches iff 'x' is within 'y'

## with extra identifiers (ex: in genomics)
x = data.table(chr=c("Chr1", "Chr1", "Chr2", "Chr2", "Chr2"),
               start=c(5,10, 1, 25, 50), end=c(11,20,4,52,60))
y = data.table(chr=c("Chr1", "Chr1", "Chr2"), start=c(1, 15,1),
               end=c(4, 18, 55), geneid=letters[1:3])
setkey(y, chr, start, end)
foverlaps(x, y, type="any", which=TRUE)
foverlaps(x, y, type="any")
foverlaps(x, y, type="any", nomatch=NULL)
foverlaps(x, y, type="within", which=TRUE)
foverlaps(x, y, type="within")
foverlaps(x, y, type="start")

## x and y have different column names - specify by.x
x = data.table(seq=c("Chr1", "Chr1", "Chr2", "Chr2", "Chr2"),
               start=c(5,10, 1, 25, 50), end=c(11,20,4,52,60))
y = data.table(chr=c("Chr1", "Chr1", "Chr2"), start=c(1, 15,1),
               end=c(4, 18, 55), geneid=letters[1:3])
setkey(y, chr, start, end)
foverlaps(x, y, by.x=c("seq", "start", "end"),
            type="any", which=TRUE)
}
\seealso{
\code{\link{data.table}},
\url{https://www.bioconductor.org/packages/release/bioc/html/IRanges.html},
\code{\link{setNumericRounding}}
}
\keyword{ data }
