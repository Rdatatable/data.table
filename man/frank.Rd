\name{frank}
\alias{frank}
\alias{frankv}
\alias{rank}
\title{Fast rank}
\description{
  Similar to \code{base::rank} but \emph{much faster}. And it accepts vectors, lists, \code{data.frame}s or \code{data.table}s as input. In addition to the \code{ties.method} possibilities provided by \code{base::rank}, it also provides \code{ties.method="dense"}.

  Like \code{\link{forder}}, sorting is done in "C-locale"; in particular, this may affect how capital/lowercase letters are ranked. See Details on \code{forder} for more.

  \code{bit64::integer64} type is also supported.
}

\usage{
frank(x, \dots, na.last=TRUE, ties.method=c("average",
  "first", "last", "random", "max", "min", "dense"))

frankv(x, cols=seq_along(x), order=1L, na.last=TRUE,
      ties.method=c("average", "first", "last", "random",
        "max", "min", "dense"))

}
\arguments{
  \item{x}{ A vector, or list with all its elements identical in length or \code{data.frame} or \code{data.table}. }
  \item{\dots}{ Only for \code{list}s, \code{data.frame}s and \code{data.table}s. The columns to calculate ranks based on. Do not quote column names. If \code{\dots} is missing, all columns are considered by default. To sort by a column in descending order prefix \code{"-"}, e.g., \code{frank(x, a, -b, c)}. \code{-b} works when \code{b} is of type \code{character} as well.}
  \item{cols}{ A \code{character} vector of column names (or numbers) of \code{x}, for which to obtain ranks. }
  \item{order}{ An \code{integer} vector with only possible values of 1 and -1, corresponding to ascending and descending order. The length of \code{order} must be either 1 or equal to that of \code{cols}. If \code{length(order) == 1}, it is recycled to \code{length(cols)}. }
  \item{na.last}{ Control treatment of \code{NA}s. If \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they are put first; if \code{NA}, they are removed; if \code{"keep"} they are kept with rank \code{NA}. }
  \item{ties.method}{ A character string specifying how ties are treated, see \code{Details}. }
}
\details{
  To be consistent with other \code{data.table} operations, \code{NA}s are considered identical to other \code{NA}s (and \code{NaN}s to other \code{NaN}s), unlike \code{base::rank}. Therefore, for \code{na.last=TRUE} and \code{na.last=FALSE}, \code{NA}s (and \code{NaN}s) are given identical ranks, unlike \code{\link[base]{rank}}.

  \code{frank} is not limited to vectors. It accepts \code{data.table}s (and \code{list}s and \code{data.frame}s) as well. It accepts unquoted column names (with names preceded with a \code{-} sign for descending order, even on character vectors), for e.g., \code{frank(DT, a, -b, c, ties.method="first")} where \code{a,b,c} are columns in \code{DT}. The equivalent in \code{frankv} is the \code{order} argument.

  In addition to the \code{ties.method} values possible using base's \code{\link[base]{rank}}, it also provides another additional argument \code{"dense"} which returns the ranks without any gaps in the ranking. See examples.
}
\value{
  A numeric vector of length equal to \code{NROW(x)} (unless \code{na.last = NA}, when missing values are removed). The vector is of integer type unless \code{ties.method = "average"} when it is of double type (irrespective of ties).
}

\examples{
# on vectors
x = c(4, 1, 4, NA, 1, NA, 4)
# NAs are considered identical (unlike base R)
# default is average
frankv(x) # na.last=TRUE
frankv(x, na.last=FALSE)

# ties.method = min
frankv(x, ties.method="min")
# ties.method = dense
frankv(x, ties.method="dense")

# on data.table
DT = data.table(x, y=c(1, 1, 1, 0, NA, 0, 2))
frankv(DT, cols="x") # same as frankv(x) from before
frankv(DT, cols="x", na.last="keep")
frankv(DT, cols="x", ties.method="dense", na.last=NA)
frank(DT, x, ties.method="dense", na.last=NA) # equivalent of above using frank
# on both columns
frankv(DT, ties.method="first", na.last="keep")
frank(DT, ties.method="first", na.last="keep") # equivalent of above using frank

# order argument
frank(DT, x, -y, ties.method="first")
# equivalent of above using frankv
frankv(DT, order=c(1L, -1L), ties.method="first")
}
\seealso{
  \code{\link{data.table}}, \code{\link{setkey}}, \code{\link{setorder}}
}
\keyword{ data }
