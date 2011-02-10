\name{data.table}
\alias{data.table}
\alias{as.data.table}
\alias{is.data.table}
\alias{Ops.data.table}
\alias{na.omit.data.table}
\alias{is.na.data.table}
\alias{na.omit.data.table}
\alias{[.data.table}
\title{ Enhanced data.frame }
\description{
   \code{data.table} \emph{inherits} from \code{data.frame}. It offers fast subset, fast grouping and fast ordered joins in a short and flexible syntax, for faster development. It was inspired by \code{A[B]} syntax in \R where \code{A} is a matrix and \code{B} is a 2-column matrix. Since a \code{data.table} \bold{is} a \code{data.frame} it is compatible with \R functions and packages that \emph{only} accept \code{data.frame}.\cr
   The 10 minute quick start guide to \code{data.table} may be a good place to start; type \code{vignette("datatable-intro")}.
}
\usage{
data.table(..., keep.rownames=FALSE, check.names=TRUE, key=NULL)

\method{[}{data.table}(x, i, j, by=NULL, with=TRUE, nomatch = NA,
  mult = "all", roll = FALSE, rolltolast = FALSE,
  which = FALSE, bysameorder = FALSE,
  verbose=getOption("datatable.verbose",FALSE), drop=NULL)
}
\arguments{
  \item{\dots}{ Just as \code{\dots} in \code{\link{data.frame}}. Usual recycling rules are applied to vectors of different lengths to create a list of equal length vectors.

}
  \item{keep.rownames}{ If \code{\dots} is a \code{matrix} or \code{data.frame}, \code{TRUE} will retain the rownames of that object in a column named \code{rn}.

}
  \item{check.names}{ Just as \code{check.names} in \code{\link{data.frame}}.

}
  \item{key}{ Character vector of length 1 containing one or more column names separated by comma which is passed to \code{\link{setkey}}.

}
  \item{x}{ A \code{data.table}.

}
  \item{i}{ Integer, logical, character, expression of column names, or \code{data.table}.

  integer and logical work the same way they do in \code{\link{[.data.frame}}.

  character is matched to the first column of \code{x}'s key.

  expression is evaluated within the frame of the \code{data.table} (i.e. it sees column names as if they are variables) and can evaluate to any of the other types.

  When \code{i} is a \code{data.table}, \code{x} must have a key. \code{i} is \emph{joined} to \code{x} using the key and the rows in \code{x} that match are returned. An equi-join is performed between each column in \code{i} to each column in \code{x}'s key. The match is a binary search in compiled C in O(log n) time. If \code{i} has less columns than \code{x}'s key then many rows of \code{x} may match to each row of \code{i}. If \code{i} has more columns than \code{x}'s key, the columns of \code{i} not involved in the join are included in the result. If \code{i} also has a key, it is \code{i}'s key columns that are used to match to \code{x}'s key columns, and a binary merge of the two tables is carried out.

  Advanced: When \code{i} is an expression of column names that evaluates to \code{data.table}, a join is performed. We call this a \emph{self join}.\cr\cr

}
  \item{j}{ \code{list()} of expressions of column names, an expression or function call that evaluates to \code{list} (including \code{data.frame} and \code{data.table} which are \code{list}s, too), or (when \code{with=FALSE}) same as \code{j} in \code{[.data.frame}.

  \code{j} is evaluated within the frame of the \code{data.table} (i.e. it sees column names as if they are variables). The result is returned. \code{list()} may be omitted when there is just one expression, for convenience.\cr\cr

}
  \item{by}{ \code{list()} of expressions of column names, or a single character string containing comma separated column names, or a character vector of column names.

  The \code{list()} of expressions is evaluated within the frame of the data.table (i.e. it sees column names as if they are variables). The data.table is then grouped by the \code{by} and \code{j} is evaluated within the frame of each group. \code{list()} may be omitted when there is just one expression, typically a single unquoted column name.

  Advanced: Agreggation for a subset of known groups is particularly efficient when passing those groups in \code{i}. When \code{i} is a \code{data.table}, \code{DT[i,j]} evaluates \code{j} for each row of \code{i}. We call this \emph{by without by} or \emph{grouping by i}. Hence the self join \code{DT[data.table(unique(colA)),j]} is identical to \code{DT[,j,by=colA]}.

  Advanced: An object \code{.SD} may be used in the \code{j} expression. \code{.SD} is a \code{data.table} containing the subset of \code{x}'s data for each group, excluding the group column(s). \code{.SD} can be used when grouping by \code{i} and when grouping by \code{by}.

  Advanced: In the \code{X[Y,j]} form of grouping, the \code{j} expression sees variables in \code{X} first, then \code{Y}. We call this \emph{join inherited scope}. If the variable is not in \code{X} or \code{Y} then the calling frame is searched, it's calling frame, and so on in the usual way up to and including the global environment.\cr\cr

}
  \item{with}{ By default \code{with=TRUE} and \code{j} is evaluated within the frame of \code{x}; the column names can be used as variables. When \code{with=FALSE}, \code{j} works as it does in \code{[.data.frame}.

}
  \item{nomatch}{ Same as \code{nomatch} in \code{\link{match}}. When a row in \code{i} has no match to \code{x}'s key, \code{nomatch=NA} means \code{NA} is returned for \code{x}'s non-join columns for that row of \code{i}. 0 means no rows will be returned for that row of \code{i}.

}
  \item{mult}{ When \emph{multiple} rows in x matches to the row in i, \code{mult} controls which are returned: \code{"all"} (default), \code{"first"} or \code{"last"}.

}
  \item{roll}{ Applies to the last column of \code{x}'s key, which is generally a date but can be any ordered variable, with gaps. When \code{roll=TRUE} if \code{i}'s row matches to all but the last column of \code{x}'s key, and the value of the last column falls in a gap (including after the last observation for that group), the prevailing value in \code{x} is \emph{rolled} forward. This operation is particular fast using a modified binary search. The operation is also known as last observation carried forward (LOCF).

}
  \item{rolltolast}{ Like \code{roll} but the data is not rolled forward past the \emph{last} observation. The value of \code{i} must fall in a gap in \code{x} but not after the end of the data for that group defined by the \code{length(key(x))-1} columns of \code{x}'s key. \code{roll} and \code{rolltolast} may not both be \code{TRUE}.

}
  \item{which}{ \code{TRUE} returns the integer row numbers of \code{x} that \code{i} matches to.

}
  \item{bysameorder}{ Advanced. \code{TRUE} tells \code{[.data.table} that the expressions in \code{by} are an order preserving map of \code{x}, allowing some efficiency gains. In most situations \code{bysameorder} is set to \code{TRUE} internally and there is no need to use this option. We hope to make this feature fully automatic in a future version and remove this argument.

}
  \item{verbose}{ \code{TRUE} turns on status and information mesages to the console. Turn this on by default using \code{options(datatable.verbose=TRUE)}. The quantity and types of verbosity may be expanded in future.

}  
  \item{drop}{ Never used by \code{data.table}. Do not use. It needs to be here because \code{data.table} inherits from \code{data.frame}. See \code{vignette("datatable-faq")}.

}
}
\details{
\code{data.table} builds on base \R functionality to reduce 2 types of time :
\enumerate{
\item programming time (easier to write, read, debug and maintain)
\item compute time
}

It combines database like operations such as \code{\link{subset}}, \code{\link{with}} and \code{\link{by}} and provides similar joins that \code{\link{merge}} provides but faster. This is achieved by using R's column based ordered in-memory \code{data.frame} structure, \code{eval} within the environment of a \code{list}, the \code{[.data.table} mechanism to condense the features, and compiled C to make certain operations fast.

The package can be used just for rapid development (compact syntax) with small datasets. Largest compute time benefits are on 64bit platforms with plentiful RAM, or when smaller datasets are repeatedly queried within a loop, or when other methods use so much working memory that they fail with an out of memory error.

As with \code{[.data.frame}, \emph{compound queries} can be concatenated on one line, e.g. 
\preformatted{
    DT[,sum(v),by=colA][V1<300][tail(order(V1))]
    # sum(v) by colA then return the 6 largest which are under 300
}
The \code{j} expression does not have to return data, e.g.
\preformatted{
    DT[,plot(colB,colC),by=colA]
    # produce a set of plots (likely to pdf) returning no data
}
Multiple \code{data.table}s (e.g. \code{X}, \code{Y} and \code{Z}) can be joined in many ways, e.g. :
\preformatted{
    X[Y][Z]
    X[Z][Y]
    X[Y[Z]]
    X[Z[Y]]
}
A \code{data.table} is a \code{list} of vectors, just like a \code{data.frame}. However :
\enumerate{
\item it never has rownames. Instead it may have one \emph{key} of one or more columns. This key can be used for row indexing instead of rownames.
\item it has enhanced functionality in \code{[.data.table} for fast joins of keyed tables, fast aggregation, and fast last observation carried forward (LOCF).
}

Since a \code{list} \emph{is} a \code{vector}, both \code{data.frame} and \code{data.table} columns may be type \code{list}. Columns of type \code{list} can contain mixed types, and each item in a column of type \code{list} may be different lengths. See examples.

Several \emph{methods} are provided for \code{data.table}, including \code{is.na}, \code{na.omit},
\code{t}, \code{rbind}, \code{cbind}, \code{merge} and others.
}
\references{
\code{data.table} homepage: \url{http://datatable.r-forge.r-project.org/}\cr
User reviews: \url{http://crantastic.org/packages/data-table}\cr
\url{http://en.wikipedia.org/wiki/Binary_search}\cr
\url{http://en.wikipedia.org/wiki/Radix_sort}             
}
\note{ If \code{keep.rownames} or \code{check.names} are suppplied they must be written in full because \R does not allow partial argument names after \code{\dots}. For example \code{data.table(DF,keep=TRUE)} will create a
column called \code{"keep"} containing \code{TRUE} and this is correct behaviour; \code{data.table(DF,keep.rownames=TRUE)} was intended.
}
\seealso{ \code{\link{data.frame}}, \code{\link{[.data.frame}}, \code{\link{as.data.table}}, \code{\link{setkey}}, \code{\link{J}}, \code{\link{SJ}}, \code{\link{CJ}}, \code{\link{merge.data.table}}, \code{\link{tables}}, \code{\link{test.data.table}} }

\examples{
DF = data.frame(a=1:5, b=letters[1:5])
DT = data.table(a=1:5, b=letters[1:5])
identical(as.data.table(DF), DT)
identical(data.table(DF), DT)
identical(dim(DT),dim(DF))
identical(DF$a, DT$a)
DT
tables()
identical(data.table(DT,DT), cbind(DT,DT))
DT2=rbind(DT,DT)
DT3 = data.table(A=DT, B=DT, key="A.b")
tables()

DF = data.frame(a=1:5, b=6:10)
DT = data.table(a=1:5, b=6:10)

DT[2]             # select * from DT where row number = 2
DT[2:3,sum(b)]    # select sum(b) from DT where row number in (2,3)
DT[2:5,plot(a)]   # used for j's side effect only i.e. displaying the plot
DT[c(FALSE,TRUE)] # extract all even numbered rows via standard R recycling

DT[,2,with=FALSE]
colNum = 2
DT[,colNum,with=FALSE]


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

# Examples above all use vector scans (bad)
# Examples below all use binary search (good)

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
cbind(DT[J("A",dts),roll=TRUE],dts)        # joined to date from dts


dts = rev(seq(as.Date("2008-06-30"), by=-1, length=5000))
dts = as.integer(gsub("-","",dts))
ids = paste(rep(LETTERS,each=26),LETTERS,sep="")
DT = data.table(CJ(id=ids, date=dts), v=rnorm(length(ids)*length(dts)))
setkey(DT,id,date)
system.time(tt <<- DT[id=="FD"])  # vector scan.   user 1.16  system 0.11  elapsed 1.27
system.time(ss <<- DT["FD"])      # binary search. user 0.02  system 0.00  elapsed 0.02
identical({setkey(tt,id,date);tt}, ss)
tables()


tt = DT[,mean(v),by="id"][c("FD","FE")]   # select mean(v) from DT group by id having id in ('FD','FE')
ss = DT[c("FD","FE"),mean(v)]             # more efficient way to group for known subgroups
identical(tt, ss)

tt = DT[c("FD","FE")][,mean(v),by="id,month=as.integer(date/100)"]

dt <- data.table(A = rep(1:3, each=4), B = rep(1:4, each=3), C = rep(1:2, 6))
dt[, head(.SD,n=2), by="B"]
dt[, transform(.SD,D=mean(A)), by="B"]
dt[, subset(.SD,A==min(A)), by="B"]

# See over 250 further examples here:
test.data.table()

\dontrun{
vignette("datatable-intro")
vignette("datatable-faq")
vignette("datatable-timings")
}

}
\keyword{ data }

