\name{Extract.data.table}
\alias{Extract.data.table}
\alias{[.data.table}
\title{ Query a data table }
\description{
  Like \code{[.data.frame} but i and j can be expressions of column names directly. i may also be a data.table and this invokes a fast table join using binary search in O(log n) time. Allowing i to be data.table is consistent with subsetting an n-dimension array by an n-column matrix in base \R.
}
\usage{
\method{[}{data.table}(x, i, j, by, \dots, with=TRUE, nomatch = NA, 
  mult = "first", roll = FALSE, rolltolast = FALSE, simplify = TRUE,
  which = FALSE, incbycols = !bysameorder, bysameorder = FALSE,
  verbose = FALSE)
}
\arguments{
  \item{x}{ A data.table }
  \item{i}{ Optional. i may be integer, logical, expression, data.table or character.
  
  integer and logical work in the same way as for data.frame row selection, see \code{with} below. However, i is generally either an expression or a data.table ...
  
  When i is an expression, it is evaluated within the frame of the data.table. The expression can 'see' column names as variables, objects in the calling frame and objects in frames above that up to and including .GlobalEnv in \R's usual way provided by \code{\link{eval}}. The expression should evaluate to integer or logical. The fact that i (and j) can be expressions of column names is the reason data.table's column names should obey \R's object name rules e.g. no spaces. There is nothing new here that subset() doesn't already provide in base \R, other than not needing to call it. i is analogous to the 'where' clause in SQL. i expressions (e.g. DT[Name=="JONES"]) will generally lead to a \emph{vector scan} i.e. every value in the column is read. Also a new logical vector has to be allocated by R, as long as the number of rows in the data.table, which is then populated before being used to subset the table. Although \code{[.data.table} allows vector scanning i expressions, \emph{joins} are preferred ...
  
  When i is a data.table, x must have a key, meaning \emph{join} i to x and return the rows in x that match. An equi-join is performed between each column in i to each column in x's key in order. This is similar to base R functionality of subsetting a matrix by a 2-column matrix, and in higher dimensions subsetting an n-dimensional array by an n-column matrix. ncol(i) columns are used in the join and i is subject to ncol(i)<=ncol(key(x)).  From \code{help(\"[\",package=\"base\")} :
    "When indexing arrays by \code{[} a single argument \code{i} can be a
    matrix with as many columns as there are dimensions of \code{x}; the
    result is then a vector with elements corresponding to the sets of
    indices in each row of \code{i}."  
  Furthermore, join criteria in SQL is placed in the 'where' clause (e.g. where tableA.id = tableB.id) so to place join criteria in the i argument is consistent. Joining is row subsetting via a match to another table. The match is a sorted match, or binary search, performed in compiled C with O(log n) search times. Using binary search not only reduces the number of evaluations of the criteria but also reduces the number of page fetches required from RAM to L2 cache.
  If a data.table is thought of as a flat sparse array (of dimension the number of columns in its key), allowing i to be a data.table in \R is natural.
  If i also has a key (as well as x) then i's key determines the columns from i to use in the sorted match. A \dfn{spliced binary search} is performed which avoids a full binary search on x for each row of i. This is marginally faster than a full binary search on x for each row of i.
  If not all the columns in x's key are joined to, then mult controls what is returned, by default "all" in this case, see mult.

  When i is character, x must have a key, and i will be matched to the first column of that key. The first column of x's key must therefore be factor in this case. data.table's \emph{never} have rownames. See \code{\link{data.table}}.
  }
  \item{j}{ Optional. j is normally an expression of column names or list of expressions enclosed in DT(). It may also be a single integer column position or single character column name. To specify a vector of integer column positions or vector of character column names you must specify with=FALSE otherwise the vector itself is returned. j is like 'select' in SQL and and is evaluated within the frame of the data.table, the column names are variables. This is standard base R functionality using \code{\link{with}}() on a data.frame. A data.frame is a frame as in evaluation frame, not just as in a (rectangular) picture frame. 
It is good practive to include "j=" or imagine it there before the 'j clause'. This avoids the mistake DT[,MySum=sum(v)] when attempting to create a data.table result containing a column named MySum. The MySum is treated as an argument name by [.data.table which doesn't exist. DT[,DT(MySum=sum(v))] was intended, or DT[,j=DT(MySum=sum(v))].
}
F
  \item{by}{ Optional. character vector length 1. Contains column names or expressions of column names seperated by comma to perform aggregation by. Apply the j expression by the by. Agreggation for a subset of known groups can be achieved more efficiently by passing those groups as an i data.table and using mult="all". This is analogous to 'having' clause in SQL. See simplify below and examples. 
   NOTE: \code{by} is currently implemented inefficiently. On large datasets is may appear to hang. This is a known problem to be resolved.
}

  \item{\dots}{ reserved for future use. At some point j may be allowed to be function where the \dots argument(s) are passed on to a j function which takes data.table input e.g. DT[,tail,by="ColA",n=3] where n=3 is passed on to tail.}

  \item{with}{ by default j is evaluated within the frame of x so the column names are seen as variables. See \code{\link{with}}. DT[,"ColA"] returns the column as expected, as does DT[,1]. However DT[,1:2], DT[,c(1,2)] and DT[,c("a","b")] all return the j vector itself without looking up the column contents. Setting with=FALSE resolves this. Note that DT[,DT(ColA,ColB)] is preferred to DT[,c("ColA","ColB"),with=FALSE]. If column integers or character names are held in a variable, with=FALSE is also required e.g. DT[,colsVar,with=FALSE].}

The remaining arguments below are only applicable for joins i.e. when i is a data.table.

  \item{nomatch}{ Same as nomatch in \code{\link{match}}. nomatch=NA is like an outer join in SQL i.e. rows containing NA are returned for rows in i not in x. nomatch=0 is like an inner join in SQL i.e. no rows are returned for rows in i not in x. }

  \item{mult}{ "first","last" or "all". Controls what to return when there is more than one row in x matching the row in i. "all" is analogous to inner join behaviour in SQL. SQL has no first and last concept since it's data is inherently unordered (note that select top x * can not reliably return the same rows over time in SQL). The reason the default for mult is not "all" is efficiency. When x has a unique key (normally true for time series) and all the columns in the key are joined to, then the 3 options return the same result. But "first" (or "last") is more efficient internally than "all" in that case because mult="all" calls "first" and then "last" and returns the rows inbetween. When not all the columns in x's key are joined to (i.e. ncol(i)<ncol(key(x))), mult is defaulted to "all". }

  \item{roll}{ By default an equi-join is performed on each column in turn. For example given x=DT(id,datetime), where the datetime is irregular, we look for the exact datetime in the i table occurring in the x table. In R we often roll data on through missing periods using a function such as fill.na so that we can then equi-join to the regular time series. However this takes programming time, compute time and storage space, sometimes very significantly. roll=TRUE applies to the last column of x's key. It returns the last available observation \emph{on or before} the datetime (or any numeric) in the last column of the i table. The datetimes in x that are joined \emph{to} are returned, as this is useful information. This may at first seem like an error (since duplicates are returned when roll=TRUE) but is correct. If the dates in the i table are required, these can be appended afterwards, but this is rarely needed in practice. See examples. }

  \item{rolltolast}{ Like roll but the time series data is not rolled past the last observation. In finance this is useful when a stock is delisted and we do not want to roll the last price forward creating a flat line. \code{nomatch} determines whether NA is returned, or no rows are returned for the period after the last price. roll and rolltolast may not both be TRUE. }

  \item{simplify}{ Control's the result when grouping j is achieved by passing i in with mult="all" rather than using the 'by' argument. See by above. TRUE collapses the result to a single data.table key'd by the groups. FALSE returns a raw list, one item per group, which is also the same length as the number of rows in i. }

  \item{which}{ By default the subset of rows in x matching i are returned. TRUE returns the row numbers only as a vector. These row numbers can be stored and passed in as the i directly in further data.table queries for efficiency, or used in further logic. }

  \item{incbycols}{ Advanced. By default the groups created by the by expression are returned as the first column(s) of the result. The alternative way to group is by passing groups in the i data.table if the grouping corresponds to x's key and setting mult="all". FALSE omits the i columns from the result for efficiency in the 2nd case when writing multiple queries with the same grouping. }

  \item{bysameorder}{ Advanced. The by expression creates a data.table internally which is then key'd and passed in as the i clause using SJ() ( possibly created a different order of the groups than they appear in the table) and mult set to "all". But when groups are passed into i directly \code{bysameorder=TRUE} saves the \code{setkey} for efficiency. }

  \item{verbose}{ TRUE turns on status and information mesages to the console in an attempt to aid debugging. }  
}
\details{
Builds on base R functionality to reduce 2 types of time :
\enumerate{
\item programming time (easier to write, read, debug and maintain)
\item compute time
}
when combining database like operations (\code{\link{subset}}, \code{\link{with}} and \code{\link{by}}) and provides similar joins that \code{\link{merge}} provides but faster. This is achieved by using R's column based ordered in-memory
data.frame, eval within the environment of a list (i.e. with), the [.data.table mechanism to condense the
features and compiled C to make certain operations fast.

The package can be used solely for development time benefits on small datasets. Main compute time benefits are on 64bit platforms with plentiful RAM, or by using the ff package, or both.

Like a data.frame, the comma is optional inside [] when j is missing. However unlike with a data.frame a single unnamed argument refers to i rather than j. For example DT[3] returns the 3rd row as a 1 row data.table rather than DF[3] which returns the 3rd column as a vector. DT[3] is identical to DT[3,] unlike data.frame's. The i argument of matrix, data.frame and data.table subsetting using '[' is analogous to the 'where' clause in SQL. In data.table's when long expressions of column names appear as the i, or a join expression, we do not have to remember the comma at the end of the line. In a data.table if the i is missing, thats when you have to remember the comma, but at the beginning, so that the argument aligns to the j (j is analogous to 'select' clause in SQL). data.table's can be treated as a list (since they are a list as are data.frame's) by column index using [[ just as a data.frame e.g. DT[[3]] is identical to DF[3] and DF[[3]].

As with a data.frame a 1 row subset returns a 1 row data.table. As with data.frame a 1 column subset (or in data.table an expression of column names returning a vector) returns a vector. However unlike data.frame there is no drop argument. The type of j's result determines the result e.g. DT[,b] returns a vector, DT[,DT(b)] returns a 1-column data.table. Note that DT the data.table object, and DT the function are different \R objects in these examples.  When no j clause is present, a data.table subset will always return a data.table even if only one row is returned (unlike matrix subsetting but like data.frame subsetting).

As with data.frame subsetting mutliple queries can be concatenated on one line e.g. DT[a>3][order(b)] is analogous to 'select * from DT where a>3 order by b' in SQL. Ordering is a select of all the rows, but in a different order. Another analogy is DT[,sum(b),by="c"][c=="foo"] compared to "select sum(b) from DT group by c having c=='foo'". However, as noted under 'by' above this is more efficiently implemented using \code{setkey(DT,c);DT["foo",sum(b),mult="all"]}.
  
The j expression does not have to return data. Data.tables 'queries' can be used for their side effects. For example DT[,plot(colB,colC),by="colA"] produces a set of plots, perhaps to a PDF file, and returns no data. If the j expression returns data and has side-effects (e.g. \link{hist}()) but only the side-effects are required, the j expression can be wrapped with \code{\link{invisible}}().

The j expression 'sees' variables in the calling frame and above including .GlobalEnv, see the examples. This is base R functionality from eval() and with().

When i is a logical expression e.g. DT[a==3], R must first create new memory to hold a logical vector as long as the rows in DT to hold the result of a==3. This is then used to subset DT. We call this a \dfn{vector scan} since every value of column a must be read. For large datasets (or repetitive algorithms on small datasets) vector scans have poor performance. Instead join to DT using setkey(DT,a) then DT[J(3),mult="all"].
}
\value{
A data.table when j is missing, even when this is one row. Unlike data.frame, if any columns are factor, unused factor levels in the subset are not retained in the result to save memory.
When j is provided, the type of j determines the type of the result.
When j returns a data.table with grouping either using \code{by} or passing groups into i with mult="all", then simplify controls whether the groups of data.tables are bound into one data.table.
When j is used for its side effects only, NULL is returned.
}

\references{ \url{http://en.wikipedia.org/wiki/Binary_search} }
\author{ Matthew Dowle }
\note{
\enumerate{
\item All arguments other than i,j and by, must be written in full since they appear after the \dots. \R does not allow partial argument names after \dots.
\item Some examples use dates stored as integer format yyyymmdd but this is not imposed. Nothing in data.table is specific to any particular datetime class, only that storage.mode is numeric in key columns.
\item Grouping is currently inefficient.
\item Whilst it is possible to create a data.table of ff objects, and subset it by integer row numbers (see examples), keys and joins are not currently implemented when the data.table contains ff objects.
}
}
\seealso{ \code{\link{data.table}}, \code{\link{setkey}}, \code{\link{J}}, \code{\link{test.data.table}}, \code{\link{like}}, \code{\link{between}} }
\examples{
DF = data.frame(a=1:5, b=6:10)
DT = data.table(a=1:5, b=6:10)

DT[2]             # select * from DT where row number = 2
DT[2:3,sum(b)]    # select sum(b) from DT where row number in (2,3)
DT[2:5,plot(a)]   # used for j's side effect only i.e. displaying the plot
DT[c(FALSE,TRUE)] # extract all even numbered rows via standard R recycling

flush.console()

tt = subset(DF,a==3)
ss = DT[a==3]
identical(as.data.table(tt), ss)

tt = subset(DF,a==3,b)[[1]]+1
ss = DT[a==3,b+1]
identical(tt, ss)

tt = with(subset(DF,a==3),a+b+1)
ss = DT[a==3,a+b+1]
identical(tt, ss)

Lkp=1:3
tt = DF[with(DF,a \%in\% Lkp),]
ss = DT[a \%in\% Lkp]
identical(as.data.table(tt), ss)

# Examples above all use vector scans.
# Examples below all use binary search.

DT = data.table(a=letters[1:5], b=6:10)
setkey(DT,a)
identical(DT["d"],DT[4])
identical(DT[J("d")], DT[4])
identical(DT[c("c","d")], DT[J(c("c","d"))])


DT = data.table(id=rep(c("A","B"),each=3), date=c(20080501L,20080502L,20080506L), v=1:6)
setkey(DT,id,date)
DT
DT["A"]                                    # all 3 rows for A
DT[J("A",20080502L)]                       # date matches exactly
DT[J("A",20080505L)]                       # NA since 5 May missing (outer join)
DT[J("A",20080505L),nomatch=0]             # inner join
dts = c(20080501L, 20080502L, 20080505L, 20080506L, 20080507L, 20080508L)
DT[J("A",dts)]                             # 3 dates match exactly
DT[J("A",dts),roll=TRUE]                   # roll previous data forward
DT[J("A",dts),rolltolast=TRUE]             # roll all but last observation forward
DT[J("A",dts),rolltolast=TRUE,nomatch=0]   # remove time series after last
DT(DT[J("A",dts),roll=TRUE],dts)           # joined to date from dts


dts = rev(seq(as.Date("2008-06-30"), by=-1, length=5000))
dts = as.integer(gsub("-","",dts))
ids = paste(rep(LETTERS,each=26),LETTERS,sep="")
DT = data.table(CJ(id=ids, date=dts), v=rnorm(length(ids)*length(dts)))
setkey(DT,id,date)
system.time(tt <<- DT[id=="FD"])  # vector scan.   user 1.16  system 0.11  elapsed 1.27 
system.time(ss <<- DT["FD"])      # binary search. user 0.02  system 0.00  elapsed 0.02
identical({setkey(tt,id,date);tt}, ss)
tables(mb=TRUE)


tt = DT[,mean(v),by="id"][c("FD","FE")]   # select mean(v) from DT group by id having id in ('FD','FE')
ss = DT[c("FD","FE"),mean(v)]             # more efficient way to group for known subgroups
identical(tt, ss)

tt = DT[c("FD","FE")][,mean(v),by="id,month=as.integer(date/100)"]

\dontrun{
# Ensure you have at least 2.7GB free disk space comfortably before running this
# See Notes section above regarding ff
require(ff)
n=180000000L
DT = data.table(id=ff(0L,length=n), date=ff(1L,length=n), val=ff(0,length=n))
DT$id[167000001L] = 20
DT$val[167000002L] = 3.14
DT$date[167000003L] = 42
DT[167000000:167000005]
physical(DT$id)
rm(DT)
gc()  # return memory to OS
}

# See over 90 further examples in \link{test.data.table}()

}
\keyword{ data }
