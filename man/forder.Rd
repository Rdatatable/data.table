\name{forder}
\alias{order}
\alias{fastorder}
\alias{forderv}
\title{Fast Ordering Permutation}
\description{
  Finds permutation that reorders its input into specified order.

  \code{forder} is faster equivalent of \code{\link[base]{order}}. Argument \code{method} other than \code{"radix"} will redirect computation to \code{base::order}.

  \code{forderv} has a different interface and is meant to be used on \code{data.table}. In case if input is already ordered it returns \code{integer()} rather than \code{seq_len(nrow(x))}. Default value of \code{na.last} is \code{FALSE}, unlike \code{forder}. When \code{na.last} is \code{NA}, then zeros are returned for missing observations, unlike \code{forder}.

  Both functions finds the order in \emph{C-locale}, for details see \emph{Note} section.
}
\usage{
  forder(\dots, na.last=TRUE, decreasing=FALSE, method=c("auto","shell","radix"))
  forderv(x, by=seq_along(x), retGrp=FALSE, sort=TRUE, order=1L, na.last=FALSE)
}
\arguments{
  \item{\dots}{ Vectors on which to find permutation, variable names must not be quoted. To find descending order prefix the symbol \code{"-"} which means \emph{descending} (not \emph{negative}, in this context), i.e., \code{forder(a, -b, c)}. The \code{-b} works when \code{b} is of type \code{character} as well. }
  \item{na.last}{ \code{logical}. If \code{TRUE}, missing values in the data are placed last (default for \code{forder}); if \code{FALSE} they are placed first (default for \code{forderv}); if \code{NA} they are removed. }
  \item{decreasing}{ \code{logical} defaults to \code{FALSE}. When \code{TRUE} then it finds descending order in data, or when combined with \code{"-"} prefixes it will invert the order. Scalar will be recycled to match the number of arguments passed to \dots. }
  \item{method}{ \code{character} defaults to \code{"radix"}, any other value will redirect computation to \code{base::order}. }
  \item{x}{ A \code{data.table}. }
  \item{by}{ A \code{character} vector of column names of \code{x} by which to order. By default, find order over all columns; Do not add \code{"-"} prefixes here, use \code{order} argument instead. }
  \item{retGrp}{ \code{logical} defaults to \code{FALSE}. When \code{TRUE} then resulting object will carry extra attributes \code{starts} and \code{maxgrpn}.  }
  \item{sort}{ \code{logical} defaults to \code{TRUE}. When \code{FALSE} then results will be only identifying the groups and not returning the order. }
  \item{order}{ An \code{integer} vector with only possible values of \code{1} and \code{-1}, corresponding to ascending and descending order. The length of \code{order} must be either \code{1} or equal to that of \code{cols}. If \code{length(order)==1}, it is recycled to \code{length(by)}. }
}
\details{
  \code{data.table} implements its own fast \emph{radix}-based ordering. See the references for some exposition on the concept of \emph{radix} sort.

  Note that \code{data.table}'s fast order has been contributed to base R 3.3.0 in 2016. Since then \code{data.table}'s fast order evolved and is now parallel, uses multiple CPU threads.

  \code{bit64::integer64} type is also supported for finding order.

  Queries like \code{x[order(...)]} are optimised internally to use \code{data.table}'s fast order. Moreover queries \code{x[order(..., na.last=FALSE)]} can re-use existing indices.
}
\note{
  Using \emph{C-locale} makes the behaviour of ordering in \code{data.table} more consistent across sessions and locales. The behaviour of \code{base::order} depends on assumptions about the locale of the R session. In English locales, \code{"america" < "BRAZIL"} is \code{TRUE} by default but false if you either type \code{Sys.setlocale(locale="C")} or the R session has been started in a C locale for you -- which can happen on servers/services since the locale comes from the environment the R session was started in. By contrast, \code{"america" < "BRAZIL"} is always \code{FALSE} in \code{data.table} regardless of the way your R session was started.
}
\references{
  \url{https://en.wikipedia.org/wiki/Radix_sort}\cr
  \url{https://en.wikipedia.org/wiki/Counting_sort}\cr
  \url{http://stereopsis.com/radix.html}\cr
  \url{https://codercorner.com/RadixSortRevisited.htm}\cr
  \url{https://medium.com/basecs/getting-to-the-root-of-sorting-with-radix-sort-f8e9240d4224}
}
\value{
  Integer vector.

  \code{forder} will return vector of the same length as vectors in \dots. If \code{na.last} is \code{NA}, then returned values are limited to non-missing observations in input.

  \code{forderv} will return vector of the length equal to \code{nrow(x)}, or in case if \code{x} is already ordered by provided \code{by} argument, then 0 length integer vector, \code{integer()} is returned, rather than \code{seq_len(nrow(x))}. If \code{na.last} is \code{NA}, then zeros are returned as the order of missing observations.
}
\seealso{
  \code{\link{setkey}}, \code{\link{setorder}}, \code{\link{fsort}}
}
\examples{
x = 1:5
d = data.table(x)
forder(x)
forderv(d, by="x")

x[4L] = NA
set(d, 4L, "x", NA)
forder(x)
forderv(d, by="x")

forder(x, na.last=NA)
forderv(d, by="x", na.last=NA)
}
\keyword{ data }
