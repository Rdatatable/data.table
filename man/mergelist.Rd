\name{mergelist}
\alias{mergelist}
\title{Merge multiple data.tables}
\description{
  Faster merge of multiple \code{data.table}s.
}
\usage{
  mergelist(l, on, cols,
    how = c("left","inner","full","right"),
    mult = c("error","all","first","last"),
    copy = TRUE,
    join.many = getOption("datatable.join.many"))
}
\arguments{
  \item{l}{ \code{list} of \code{data.table}s to merge. }
  \item{on}{ \code{character} of columns to merge on, when missing, then \emph{key} of \emph{join-to} table is used. }
  \item{cols}{ \code{list} of \code{character} vectors corresponding to tables in \code{l}. }
  \item{how}{ \code{character} scalar, controls how to merge tables. Allowed values are \code{"left"} (default), \code{"inner"}, \code{"full"}, \code{"right"}. }
  \item{mult}{ \code{character} scalar, controls how to proceed when multiple rows in \emph{join-to} table match to the row in \emph{join-from} table. Allowed values are \code{"error"} (default), \code{"all"}, \code{"first"}, \code{"last"}. See examples on how to detect duplicated matches. Using \code{"all"} is recommended together with \code{join.many=FALSE}, unless rows explosion or cartesian product are intended. }
  \item{copy}{ \code{logical}, defaults to \code{TRUE}, when \code{FALSE}, then resulting object will share columns with \code{l[[1L]]} table. Using \code{FALSE} requires \code{how=="left" && mult!="all"}. }
  \item{join.many}{ \code{logical}, defaults to \code{TRUE}, when \code{FALSE} and \code{mult="all"}, then extra check is made to ensure no \emph{many-to-many} matches exists between tables, and if they are, then exception is raised. Works similarly to \code{allow.cartesian} option in \code{[.data.table} but is more strict. An option \code{"datatable.join.many"} can be used to control that globally, for \code{mergelist} and \code{[.data.table}. }
}
\details{
  Merging is performed sequentially, for \code{l} of 3 tables, it will do something like \code{merge(merge(l[[1L]], l[[2L]]), l[[3L]])}.

  \emph{Not yet implemented:} arguments \code{on}, \code{how}, \code{mult}, \code{join.many} could be lists as well, each of length \code{length(l)-1L}, to provide argument to be used for each single tables pair to merge.

  Terms \emph{join-to} and \emph{join-from} depends on \code{how} argument:
  \enumerate{
    \item{ \code{how="left"}: \emph{join-to} is \emph{RHS}, \emph{join-from} is \emph{LHS}. }
    \item{ \code{how="inner"}: treats \emph{LHS} and \emph{RHS} tables equally, terms applies to both tables. }
    \item{ \code{how="full"}: performs \emph{left} and \emph{right} joins and then binds results together, terms applies to both tables. }
    \item{ \code{how="right"}: \emph{join-to} is \emph{LHS}, \emph{join-from} is \emph{RHS}. }
  }

  Using \code{mult="error"} (default) will raise exception when multiple rows in \emph{join-to} table match to the row in \emph{join-from} table. It should not be used to just detect duplicates, as duplicates might not have matching row, and in such case exception will not be raised.

  When \code{on} argument is missing, then columns to join on will be decided based on \emph{key} depending on \code{how} argument:
  \enumerate{
    \item{ \code{how="left|right"}: key columns of \emph{join-to} table. }
    \item{ \code{how="inner|full"}: if only one table has key, then this key is used, if both tables have key, then \code{intersect(key(lhs), key(rhs))}, having its order aligned to shorter key. }
  }
}
\value{
  A new \code{data.table} based on the merged objects.
}
\note{
  Using \code{how="right"} is sub-efficient. Whenever possible, it is better to swap elements of \code{l} and use \code{how="left"} instead.

  Using \code{how="inner"} or \code{mult="full"} together with \code{mult!="all"} is sub-efficient. Unlike during join in \code{[.data.table}, it will apply \code{mult} on both tables. It is to ensure that join in symmetric, so \emph{LHS} and \emph{RHS} tables can be swapped, regardless of \code{mult} argument. It is always possible to apply \code{mult}-like filter manually and join using \code{mult="all"}.

  Using \code{join.many=FALSE} is sub-efficient. Note that it only takes effect when \code{mult="all"}. If input data are verified to not have duplicated matches, then this can safely use the default \code{TRUE}. Otherwise for \code{mult="all"} merges it is recommended to use \code{join.many=FALSE}, unless of course \emph{many-to-many} join, that duplicates rows, is intended.
}
\seealso{
  \code{\link{[.data.table}}, \code{\link{merge.data.table}}
}
\examples{
#X = lapply(1:4, function(i) as.data.table(setNames(list(i+0:1, i), c("id",letters[i]))))
#mergelist(X, on="id", how="left")

## detecting duplicates
#x = data.table()
#y = data.table()
#on = "id1"
### detecting duplicates for mult='error'
#mergelist(on=on, list(
#x,
#y[,.N,by=on][N>1L]
#))
### detecting duplicates for join.many=FALSE
#on = "id1"
#mergelist(on=on, list(
#x[,.N,by=on][N>1L],
#y[,.N,by=on][N>1L]
#))

## denormalize star schema

## denormalize snowflake schema

}
\keyword{ data }