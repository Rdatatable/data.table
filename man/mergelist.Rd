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
  \item{copy}{ \code{logical}, defaults to \code{TRUE}, when \code{FALSE}, then resulting object will share columns with tables in \code{l}. Using \code{FALSE} works only for \emph{left} or \emph{right} outer joins and \code{mult!="all"}. }
  \item{join.many}{ \code{logical}, defaults to \code{TRUE}, when \code{FALSE} and \code{mult="all"}, then extra check is made to ensure no \emph{many-to-many} matches exists between tables, and if they are, then exception is raised. Works similarly to \code{allow.cartesian} option in \code{[.data.table} but is more strict. An option \code{"datatable.join.many"} can be used to control that globally, for \code{mergelist} and \code{[.data.table}. }
}
\details{
  Function should be considered experimental.

  Merging is performed sequentially, for \code{l} of 3 tables, it will do something like \code{merge(merge(l[[1L]], l[[2L]]), l[[3L]])}. Merging supports only \emph{equi-join} and column names to join on must be common in both tables.

  Arguments \code{on}, \code{how} (not yet implemented), \code{mult} (not yet implemented), \code{join.many} could be lists as well, each of length \code{length(l)-1L}, to provide argument to be used for each single tables pair to merge.

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

  When joining tables that are not directly linked to single table, e.g. snowflake schema, \emph{right} outer join can be used to optimize the sequence of merges, see examples.
}
\value{
  A new \code{data.table} based on the merged objects.
}
\note{
  Using \code{how="inner"} or \code{mult="full"} together with \code{mult!="all"} is sub-efficient. Unlike during join in \code{[.data.table}, it will apply \code{mult} on both tables. It is to ensure that join in symmetric, so \emph{LHS} and \emph{RHS} tables can be swapped, regardless of \code{mult} argument. It is always possible to apply \code{mult}-like filter manually and join using \code{mult="all"}.

  Using \code{join.many=FALSE} is sub-efficient. Note that it only takes effect when \code{mult="all"}. If input data are verified to not have duplicated matches, then this can safely use the default \code{TRUE}. Otherwise for \code{mult="all"} merges it is recommended to use \code{join.many=FALSE}, unless of course \emph{many-to-many} join, that duplicates rows, is intended.
}
\seealso{
  \code{\link{[.data.table}}, \code{\link{merge.data.table}}
}
\examples{
l = list(
  data.table(id1 = c(1:4,2:5), v1 = 1:8),
  data.table(id1 = 2:3, v2 = 1:2),
  data.table(id1 = 3:5, v3 = 1:3)
)
mergelist(l, on="id1")

## using keys
l = list(
  data.table(id1 = c(1:4,2:5), v1 = 1:8),
  data.table(id1 = 3:5, id2 = 1:3, v2 = 1:3, key="id1"),
  data.table(id2 = 1:4, v3 = 4:1, key="id2")
)
mergelist(l)

## select columns
l = list(
  data.table(id1 = c(1:4,2:5), v1 = 1:8, v2 = 8:1),
  data.table(id1 = 3:5, v3 = 1:3, v4 = 3:1, v5 = 1L, key="id1")
)
mergelist(l, cols = list(NULL, c("v3","v5")))

## detecting duplicates matches
l = list(
  data.table(id1=c(1:4,2:5), v1=1:8), ## dups in LHS are fine
  data.table(id1=c(2:3,2L), v2=1:3),  ## dups in RHS
  data.table(id1=3:5, v3=1:3)
)
#mergelist(l, on="id1") ## mult='error' and multiple matches during merge
lapply(l[-1L], function(d) copy(d)[,cnt:=.N,by="id1"][cnt>1L])

## denormalize star schema

### make fact and two dimensions

gt = state.x77[,"Population"]
gt = data.table(state_id=seq_along(state.name), p=gt[state.name]/sum(gt), k=1L)
tt = as.IDate(paste0(as.integer(time(uspop)),"-01-01"))
tt = as.data.table(stats::approx(tt, c(uspop), tt[1L]:tt[length(tt)]))
tt = tt[, .(date=as.IDate(x), date_id=seq_along(x), pop=y, k=1L)]
fact = tt[gt, on="k", allow.cartesian=TRUE,
          .(state_id=i.state_id, date_id=x.date_id, population = x.pop * i.p)]
setkeyv(fact, c("state_id","date_id"))

time = data.table(key = "date_id",
  date_id = seq_along(tt$date), date = tt$date,
  month_id = month(tt$date), month = month.name[month(tt$date)],
  year_id = year(tt$date)-1789L, year = as.character(year(tt$date)),
  week_id = week(tt$date), week = as.character(week(tt$date)),
  weekday_id = wday(tt$date)-1L, weekday = weekdays(tt$date)
)[weekday_id==0L, weekday_id:=7L][]

geog = data.table(key = "state_id",
  state_id = seq_along(state.abb), state_abb = state.abb, state_name = state.name,
  division_id = as.integer(state.division),
  division_name = as.character(state.division),
  region_id = as.integer(state.region),
  region_name = as.character(state.region)
)

l = list(fact, time, geog)
ans = mergelist(l)

rm(gt, tt, l, ans)

## denormalize snowflake schema

make.lvl = function(x, cols) {
  stopifnot(is.data.table(x))
  lvl = x[, unique(.SD), .SDcols=cols]
  setkeyv(lvl, cols[1L])
  setindexv(lvl, as.list(cols))
}
time = list(
  date = make.lvl(time, c("date_id","date","year_id","month_id","week_id",
                          "weekday_id")),
  weekday = make.lvl(time, c("weekday_id","weekday")),
  week = make.lvl(time, c("week_id","week")),
  month = make.lvl(time, c("month_id","month")),
  year = make.lvl(time, c("year_id","year"))
)
geog = list(
  state = make.lvl(geog, c("state_id","state_abb","state_name","division_id")),
  division = make.lvl(geog, c("division_id","division_name","region_id")),
  region = make.lvl(geog, c("region_id","region_name"))
)

l = c(list(fact=fact), time, geog)
ans = mergelist(l) ## only join to first level of each hierarchy will use index

rm(ans)
ans = mergelist(list( ## use indices on every join
  fact,
  mergelist(time), ## date lvl has indices to all time hierarchies
  mergelist(rev(geog), how="right") ## right outer join will retain keys
))

rm(ans)
ans = mergelist(copy=FALSE, list( ## same but even faster, copy=FALSE
  fact, mergelist(copy=FALSE, time),
  mergelist(copy=FALSE, rev(geog), how="right")
))
}
\keyword{ data }
