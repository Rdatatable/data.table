\name{mergelist}
\alias{mergelist}
\title{Merge multiple data.tables}
\description{
  Faster merge of multiple \code{data.table}s.
}
\usage{
  mergelist(l, on, cols,
    how = c("left","inner","full","right","semi","anti","cross"),
    mult, copy = TRUE,
    join.many = getOption("datatable.join.many"))
}
\arguments{
  \item{l}{ \code{list} of \code{data.table}s to merge. }
  \item{on}{ \code{character} vector of column names to merge on; when missing, the \emph{key} of \emph{join-to} table is used. }
  \item{cols}{ \code{list} of \code{character} column names corresponding to tables in \code{l}, used to subset columns during merges. }
  \item{how}{ \code{character} scalar, controls how to merge tables. Allowed values are \code{"left"} (default), \code{"inner"}, \code{"full"}, \code{"right"}, \code{"semi"}, \code{"anti"}, \code{"cross"}. See Details. }
  \item{mult}{ \code{character} scalar, controls how to proceed when multiple rows in \emph{join-to} table match to the row in \emph{join-from} table. Allowed values are \code{"error"}, \code{"all"}, \code{"first"}, \code{"last"}. Default depends on \code{how}, described in \emph{details} below. See examples on how to detect duplicated matches. Using \code{"all"} is recommended together with \code{join.many=FALSE}, unless rows explosion or cartesian product are intended. }
  \item{copy}{ \code{logical}, defaults to \code{TRUE}, when \code{FALSE}, then resulting object may share columns with tables in \code{l}, depending on matches. }
  \item{join.many}{ \code{logical}, defaults to \code{getOption("datatable.join.many")}, which is \code{TRUE} by default; when \code{FALSE} and \code{mult="all"}, then extra check is made to ensure no \emph{many-to-many} matches exist between tables, and if they exist, then exception is raised. Works similarly to \code{allow.cartesian} option in \code{[.data.table} but is more strict. An option \code{"datatable.join.many"} controls that globally for \code{mergelist} and \code{[.data.table}. }
}
\details{
  Function should be considered experimental. Users are encouraged to provide feedback in our issue tracker.

  Merging is performed sequentially, for \code{l} of 3 tables, it will do something like \code{merge(merge(l[[1L]], l[[2L]]), l[[3L]])}. Merging does not support \emph{non-equi joins}, column names to merge on must be common in both tables on each merge.

  Arguments \code{on}, \code{how}, \code{mult}, \code{join.many} could be lists as well, each of length \code{length(l)-1L}, to provide argument to be used for each single tables pair to merge, see examples.

  Terms \emph{join-to} and \emph{join-from} depends on \code{how} argument:
  \enumerate{
    \item{ \code{how="left|semi|anti"}: \emph{join-to} is \emph{RHS}, \emph{join-from} is \emph{LHS}. }
    \item{ \code{how="inner|full|cross"}: treats \emph{LHS} and \emph{RHS} tables equally, terms applies to both tables. }
    \item{ \code{how="right"}: \emph{join-to} is \emph{LHS}, \emph{join-from} is \emph{RHS}. }
  }

  Using \code{mult="error"} will raise exception when multiple rows in \emph{join-to} table match to the row in \emph{join-from} table. It should not be used to just detect duplicates, as duplicates might not have matching row, and in such case exception will not be raised.

  Default value for argument \code{mult} depends on \code{how} argument:
  \enumerate{
    \item{ \code{how="left|inner|full|right"}: sets \code{mult="error"}. }
    \item{ \code{how="semi|anti"}: sets \code{mult="last"}, although works same as \code{mult="first"}. }
    \item{ \code{how="cross"}: sets \code{mult="all"}. }
  }

  When \code{on} argument is missing, then columns to join on will be decided based on \emph{key} depending on \code{how} argument:
  \enumerate{
    \item{ \code{how="left|right|semi|anti"}: key columns of \emph{join-to} table. }
    \item{ \code{how="inner|full"}: if only one table has key, then this key is used, if both tables have key, then \code{intersect(key(lhs), key(rhs))}, having its order aligned to shorter key. }
  }

  When joining tables that are not directly linked to single table, e.g. snowflake schema, \emph{right} outer join can be used to optimize the sequence of merges, see examples.
}
\value{
  A new \code{data.table} based on the merged objects.
}
\note{
  Using \code{how="inner|full"} together with \code{mult!="all"} is sub-efficient. Unlike during join in \code{[.data.table}, it will apply \code{mult} on both tables. It is to ensure that the join is symmetric so \emph{LHS} and \emph{RHS} tables can be swapped, regardless of \code{mult} argument. It is always possible to apply \code{mult}-like filter manually and join using \code{mult="all"}.

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

## different arguments for each merge pair
l = list(
  data.table(id1=1:4, id2=4:1),
  data.table(id1=c(1:3,1:2), v2=c(1L,1L,1:2,2L)),
  data.table(id2=4:5)
)
mergelist(l,
  on = list("id1", "id2"),     ## first merge on id1, second on id2
  how = list("inner", "anti"), ## first inner join, second anti join
  mult = list("last", NULL))   ## use default 'mult' in second join

## detecting duplicates matches
l = list(
  data.table(id1=c(1:4,2:5), v1=1:8), ## dups in LHS are fine
  data.table(id1=c(2:3,2L), v2=1:3),  ## dups in RHS
  data.table(id1=3:5, v3=1:3)
)
#mergelist(l, on="id1") # ERROR: mult='error' and multiple matches during merge
lapply(l[-1L], `[`, j = if (.N>1L) .SD, by = "id1") ## duplicated rows

## 'star schema' and 'snowflake schema' examples

### populate fact: US population by state and date

gt = state.x77[,"Population"]
gt = data.table(state_id=seq_along(state.name), p=gt[state.name]/sum(gt), k=1L)
tt = as.IDate(paste0(as.integer(time(uspop)),"-01-01"))
tt = as.data.table(stats::approx(tt, c(uspop), tt[1L]:tt[length(tt)]))
tt = tt[, .(date=as.IDate(x), date_id=seq_along(x), pop=y, k=1L)]
fact = tt[gt, on="k", allow.cartesian=TRUE,
          .(state_id=i.state_id, date_id=x.date_id, population = x.pop * i.p)]
setkeyv(fact, c("state_id","date_id"))

### populate dimensions: time and geography

time = data.table(key = "date_id",
  date_id = seq_along(tt$date), date = tt$date,
  month_id = month(tt$date), month = month.name[month(tt$date)],
  year_id = year(tt$date)-1789L, year = as.character(year(tt$date)),
  week_id = week(tt$date), week = as.character(week(tt$date)),
  weekday_id = wday(tt$date)-1L, weekday = weekdays(tt$date)
)[weekday_id==0L, weekday_id:=7L][]
geog = data.table(key = "state_id",
  state_id = seq_along(state.name), state_abb=state.abb, state_name=state.name,
  division_id = as.integer(state.division),
  division_name = as.character(state.division),
  region_id = as.integer(state.region),
  region_name = as.character(state.region)
)
rm(gt, tt)

### denormalize 'star schema'

l = list(fact, time, geog)
ans = mergelist(l)

rm(l, ans)

### turn 'star schema' into 'snowflake schema'

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

### denormalize 'snowflake schema'

#### left join all
l = c(list(fact=fact), time, geog)
ans = mergelist(l)

rm(ans)
#### merge hierarchies alone, reduce sizes in merges of geog dimension
ans = mergelist(list(
  fact,
  mergelist(time),
  mergelist(rev(geog), how="right")
))

rm(ans)
#### same but no unnecessary copies
ans = mergelist(list(
  fact,
  mergelist(time, copy=FALSE),
  mergelist(rev(geog), how="right", copy=FALSE)
))
}
\keyword{ data }
