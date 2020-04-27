\name{mergelist}
\alias{mergelist}
\alias{merge}
\alias{merge.data.table}
\title{Merge multiple data.tables}
\description{
  Faster merge of multiple \code{data.table}s.
}
\usage{
  mergelist(l, on, cols, how=c("inner","left","right","full"), mult=c("all","first","last","error"), copy=TRUE)
}
\arguments{
  \item{l}{ \code{list} of \code{data.table}s to merge. }
  \item{on}{ \code{character} ???or a list of characters??? of columns to merge on, when missing, then \emph{key} of \emph{join-to} table is used. }
  \item{cols}{ \code{list} of \code{character} vectors corresponding to tables in \code{l}. }
  \item{how}{ \code{character} ???scalar???, controls how to merge tables. Allowed values are \code{"inner"} (default), \code{"left"}, \code{"right"}, \code{"full"}. }
  \item{mult}{ \code{character} ???scalar???, controls how to proceed when multiple rows in \emph{join-to} table match to the row in \emph{join-from} table. Allowed values are \code{"all"} (default), \code{"first"}, \code{"last"}, \code{"error"}. }
  \item{copy}{ \code{logical}, behavior is described in \emph{Value} section below. Using \code{copy} other than \code{TRUE} (default) requires \code{how=="left" && mult!="all"}. }
}
\details{
  Merging is performed sequentially, for \code{l} of 3 tables, it will do something like \code{merge(merge(l[[1L]], l[[2L]]), l[[3L]])}.

  Terms \emph{join-to} and \emph{join-from} depends on \code{how} argument:
  \enumerate{
    \item{ \code{how="inner"}: treats \emph{LHS} and \emph{RHS} tables equally, terms applies to both tables. }
    \item{ \code{how="left"}: \emph{join-to} is \emph{RHS}, \emhp{join-from} is \emph{LHS}. }
    \item{ \code{how="right"}: \emph{join-to} is \emph{LHS}, \emhp{join-from} is \emph{RHS}. }
    \item{ \code{how="full"}: performs \emph{left} and \emph{right} joins and then binds results together, terms applies to both tables. }
  }
}
\value{
  A new \code{data.table} based on the merged objects. Eventually when \code{copy} is \code{FALSE}, then resulting object will share columns with \code{l[[1L]]} table. When \code{copy} is \code{NA}, then resulting object might additionally share columns with remaining tables of \code{l}. }
}
\note{
  Using \code{how="right"} is sub-efficient. Whenever possible, it is better to swap elements of \code{l} and use \code{how="left"} instead.

  Using \code{how="inner"} or \code{mult="full"} together with \code{mult!="all"} is sub-efficient. Unlike during join in \code{[.data.table}, it will apply \code{mult} on both tables. It is to ensure that join in symmetric, so \emph{LHS} and \emph{RHS} tables can be swapped, regardless of \code{mult} argument. It is always possible to apply \code{mult}-like filter manually and join using \code{mult="all"}.

  It is possible to use \code{mult="error"} to raise exception when multiple rows in \emph{join-to} table match to the row in \emph{join-from} table. It should not be used to just detect duplicates, as duplicates might not have matching row, and in such case exception will not be raised.
}
\seealso{
  \code{\link{[.data.table}}, \code{\link{merge.data.table}}
}
\examples{
}
\keyword{ data }