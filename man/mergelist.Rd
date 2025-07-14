\name{mergelist}
\alias{mergelist}
\alias{setmergelist}
\title{Merge multiple data.tables}
\description{
  Faster merge of multiple \code{data.table}s.
}
\usage{
  mergelist(l, on, cols=NULL,
    how=c("left", "inner", "full", "right", "semi", "anti", "cross"),
    mult, join.many=getOption("datatable.join.many"))
  setmergelist(l, on, cols=NULL,
    how=c("left", "inner", "full", "right", "semi", "anti", "cross"),
    mult, join.many=getOption("datatable.join.many"))
}
\arguments{
  \item{l}{ \code{list} of \code{data.table}s to merge. }
  \item{on}{ \code{character} vector of column names to merge on; when missing, the \code{\link{key}} of the \emph{join-to} table is used (see Details). }
  \item{cols}{ Optional \code{list} of \code{character} column names corresponding to tables in \code{l}, used to subset columns during merges. \code{NULL} means all columns, all tables; \code{NULL} entries in a list means all columns for the corresponding table. }
  \item{how}{ Character string, controls how to merge tables. Allowed values are \code{"left"} (default), \code{"inner"}, \code{"full"}, \code{"right"}, \code{"semi"}, \code{"anti"}, and \code{"cross"}. See Details. }
  \item{mult}{ Character string, controls how to proceed when multiple rows in the \emph{join-to} table match to the row in the \emph{join-from} table. Allowed values are \code{"error"}, \code{"all"}, \code{"first"}, \code{"last"}. The default value depends on \code{how}; see Details. See Examples for how to detect duplicated matches. When using \code{"all"}, we recommend specifying \code{join.many=FALSE} as a precaution to prevent unintended explosion of rows. }
  \item{join.many}{ \code{logical}, defaulting to \code{getOption("datatable.join.many")}, which is \code{TRUE} by default; when \code{FALSE} and \code{mult="all"}, an error is thrown when any \emph{many-to-many} matches are detected between pairs of tables. This is essentially a stricter version of the \code{allow.cartesian} option in \code{\link{[.data.table}}. Note that the option \code{"datatable.join.many"} also controls the behavior of joins in \code{[.data.table}. }
}
\details{
  Note: these functions should be considered experimental. Users are encouraged to provide feedback in our issue tracker.

  Merging is performed sequentially from "left to right", so that for \code{l} of 3 tables, it will do something like \code{merge(merge(l[[1L]], l[[2L]]), l[[3L]])}. \emph{Non-equi joins} are not supported. Column names to merge on must be common in both tables on each merge.

  Arguments \code{on}, \code{how}, \code{mult}, \code{join.many} could be lists as well, each of length \code{length(l)-1L}, to provide argument to be used for each single tables pair to merge, see examples.

  The terms \emph{join-to} and \emph{join-from} indicate which in a pair of tables is the "baseline" or "authoritative" source -- this governs the ordering of rows and columns. 
  Whether each refers to the "left" or "right" table of a pair depends on the \code{how} argument:
  \enumerate{
    \item{ \code{how \%in\% c("left", "semi", "anti")}: \emph{join-to} is \emph{RHS}, \emph{join-from} is \emph{LHS}. }
    \item{ \code{how \%in\% c("inner", "full", "cross")}: \emph{LHS} and \emph{RHS} tables are treated equally, so that the terms are interchangeable. }
    \item{ \code{how == "right"}: \emph{join-to} is \emph{LHS}, \emph{join-from} is \emph{RHS}. }
  }

  Using \code{mult="error"} will throw an error when multiple rows in \emph{join-to} table match to the row in \emph{join-from} table. It should not be used just to detect duplicates, which might not have matching row, and thus would silently be missed.

  When not specified, \code{mult} takes its default depending on the \code{how} argument:
  \enumerate{
    \item{ When \code{how \%in\% c("left", "inner", "full", "right")}, \code{mult="error"}. }
    \item{ When \code{how \%in\% c("semi", "anti")}, \code{mult="last"}, although this is equivalent to \code{mult="first"}. }
    \item{ When \code{how == "cross"}, \code{mult="all"}. }
  }

  When the \code{on} argument is missing, it will be determined based \code{how} argument:
  \enumerate{
    \item{ When \code{how \%in\% c("left", right", "semi", "anti")}, \code{on} becomes the key column(s) of the \emph{join-to} table. }
    \item{ When \code{how \%in\% c("inner", full")}, if only one table has a key, then this key is used; if both tables have keys, then \code{on = intersect(key(lhs), key(rhs))}, having its order aligned to shorter key. }
  }

  When joining tables that are not directly linked to a single table, e.g. a snowflake schema (see References), a \emph{right} outer join can be used to optimize the sequence of merges, see Examples.
}
\value{
  A new \code{data.table} based on the merged objects.

  For \code{setmergelist}, if possible, a \code{\link{copy}} of the inputs is avoided.
}
\note{
  Using \code{how="inner"} or \code{how="full"} together with \code{mult!="all"} is sub-efficient. Unlike during joins in \code{[.data.table}, it will apply \code{mult} on both tables. This ensures that the join is symmetric so that the \emph{LHS} and \emph{RHS} tables can be swapped, regardless of \code{mult}. It is always possible to apply a \code{mult}-like filter manually and join using \code{mult="all"}.

  Using \code{join.many=FALSE} is also sub-efficient. Note that it only takes effect when \code{mult="all"}. If input data are verified not to have duplicate matches, then this can safely use the default \code{TRUE}. Otherwise, for \code{mult="all"} merges it is recommended to use \code{join.many=FALSE}, unless of course \emph{many-to-many} joins, duplicating rows, are intended.
}
\seealso{
  \code{\link{[.data.table}}, \code{\link{merge.data.table}}
}
\references{
  \url{https://en.wikipedia.org/wiki/Snowflake_schema}, \url{https://en.wikipedia.org/wiki/Star_schema}
}

\examples{
l = list(
  data.table(id1=c(1:4, 2:5), v1=1:8),
  data.table(id1=2:3, v2=1:2),
  data.table(id1=3:5, v3=1:3)
)
mergelist(l, on="id1")

## using keys
l = list(
  data.table(id1=c(1:4, 2:5), v1=1:8),
  data.table(id1=3:5, id2=1:3, v2=1:3, key="id1"),
  data.table(id2=1:4, v3=4:1, key="id2")
)
mergelist(l)

## select columns
l = list(
  data.table(id1=c(1:4, 2:5), v1=1:8, v2=8:1),
  data.table(id1=3:5, v3=1:3, v4=3:1, v5=1L, key="id1")
)
mergelist(l, cols=list(NULL, c("v3", "v5")))

## different arguments for each merge pair
l = list(
  data.table(id1=1:4, id2=4:1),
  data.table(id1=c(1:3, 1:2), v2=c(1L, 1L, 1:2, 2L)),
  data.table(id2=4:5)
)
mergelist(l,
  on = list("id1", "id2"),     ## first merge on id1, second on id2
  how = list("inner", "anti"), ## first inner join, second anti join
  mult = list("last", NULL))   ## use default 'mult' in second join

## detecting duplicates matches
l = list(
  data.table(id1=c(1:4, 2:5), v1=1:8), ## dups in LHS are fine
  data.table(id1=c(2:3, 2L), v2=1:3),  ## dups in RHS
  data.table(id1=3:5, v3=1:3)
)
lapply(l[-1L], `[`, j = if (.N>1L) .SD, by = "id1") ## duplicated rows
try(mergelist(l, on="id1"))

## 'star schema' and 'snowflake schema' examples (realistic data sizes)

### populate fact: US population by state and date

gt = state.x77[, "Population"]
gt = data.table(state_id=seq_along(state.name), p=gt[state.name] / sum(gt), k=1L)
tt = as.IDate(paste0(as.integer(time(uspop)), "-01-01"))
tt = as.data.table(stats::approx(tt, c(uspop), tt[1L]:tt[length(tt)]))
tt = tt[, .(date=as.IDate(x), date_id=seq_along(x), pop=y, k=1L)]
fact = tt[gt, on="k", allow.cartesian=TRUE,
          .(state_id=i.state_id, date_id=x.date_id, population=x.pop * i.p)]
setkeyv(fact, c("state_id", "date_id"))

### populate dimensions: time and geography

time = data.table(key="date_id",
  date_id= seq_along(tt$date), date=tt$date,
  month_id=month(tt$date), month=month.name[month(tt$date)],
  year_id=year(tt$date)-1789L, year=as.character(year(tt$date)),
  week_id=week(tt$date), week=as.character(week(tt$date)),
  weekday_id=wday(tt$date)-1L, weekday=weekdays(tt$date)
)
time[weekday_id == 0L, weekday_id := 7L][]
geog = data.table(key="state_id",
  state_id=seq_along(state.name), state_abb=state.abb, state_name=state.name,
  division_id=as.integer(state.division),
  division_name=as.character(state.division),
  region_id=as.integer(state.region),
  region_name=as.character(state.region)
)
rm(gt, tt)

### denormalize 'star schema'

l = list(fact, time, geog)
str(l)
mergelist(l)

rm(l)

### turn 'star schema' into 'snowflake schema'

make.lvl = function(x, cols) {
  stopifnot(is.data.table(x))
  lvl = x[, unique(.SD), .SDcols=cols]
  setkeyv(lvl, cols[1L])
  setindexv(lvl, as.list(cols))
  lvl
}
time = list(
  date = make.lvl(
    time, c("date_id", "date", "year_id", "month_id", "week_id", "weekday_id")),
  weekday = make.lvl(time, c("weekday_id", "weekday")),
  week = make.lvl(time, c("week_id", "week")),
  month = make.lvl(time, c("month_id", "month")),
  year = make.lvl(time, c("year_id", "year"))
)
geog = list(
  state = make.lvl(geog, c("state_id", "state_abb", "state_name", "division_id")),
  division = make.lvl(geog, c("division_id", "division_name", "region_id")),
  region = make.lvl(geog, c("region_id", "region_name"))
)

### denormalize 'snowflake schema'

#### left join all
l = c(list(fact=fact), time, geog)
str(l)
mergelist(l)

rm(l)
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
  setmergelist(time),
  setmergelist(rev(geog), how="right")
))
}
\keyword{ data }
