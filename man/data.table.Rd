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
   \code{data.table} \emph{inherits} from \code{data.frame}. It offers fast subset, fast grouping and fast ordered joins in a short and flexible syntax, for faster development. It was inspired by \code{A[B]} syntax in \R where \code{A} is a matrix and \code{B} is a 2-column matrix. Since a \code{data.table} \emph{is} a \code{data.frame}, it is compatible with \R functions and packages that \emph{only} accept \code{data.frame}.\cr\cr
   The 10 minute quick start guide to \code{data.table} may be a good place to start: type \code{vignette("datatable-intro")}.\cr\cr
   Please check the \href{http://datatable.r-forge.r-project.org/}{homepage} for latest news.
}
\usage{
data.table(..., keep.rownames=FALSE, check.names=TRUE, key=NULL)

\method{[}{data.table}(x, i, j, by=NULL, with=TRUE, nomatch = NA,
  mult = "all", roll = FALSE, rolltolast = FALSE,
  which = FALSE, bysameorder = FALSE, .SDcols,
  verbose=getOption("datatable.verbose",FALSE), drop=NULL)
}
\arguments{
  \item{\dots}{ Just as \code{\dots} in \code{\link{data.frame}}. Usual recycling rules are applied to vectors of different lengths to create a list of equal length vectors.

}
  \item{keep.rownames}{ If \code{\dots} is a \code{matrix} or \code{data.frame}, \code{TRUE} will retain the rownames of that object in a column named \code{rn}.

}
  \item{check.names}{ Just as \code{check.names} in \code{\link{data.frame}}.

}
  \item{key}{ Character vector of one or more column names which is passed to \code{\link{setkey}}. It may be a single comma separated string such as \code{key="x,y,z"}, or a vector of names such as \code{key=c("x","y","z")}. 

}
  \item{x}{ A \code{data.table}.

}
  \item{i}{ Integer, logical or character vector, expression of column names, or \code{data.table}.

  integer and logical vectors work the same way they do in \code{\link{[.data.frame}}. Other than \code{NA}s in logical \code{i} are treated as \code{FALSE}, and a single \code{NA} logical is not recycled to match the number of rows, as it is in \code{[.data.frame}.

  character is matched to the first column of \code{x}'s key.

  expression is evaluated within the frame of the \code{data.table} (i.e. it sees column names as if they are variables) and can evaluate to any of the other types.

  When \code{i} is a \code{data.table}, \code{x} must have a key. \code{i} is \emph{joined} to \code{x} using the key and the rows in \code{x} that match are returned. An equi-join is performed between each column in \code{i} to each column in \code{x}'s key. The match is a binary search in compiled C in O(log n) time. If \code{i} has less columns than \code{x}'s key then many rows of \code{x} may match to each row of \code{i}. If \code{i} has more columns than \code{x}'s key, the columns of \code{i} not involved in the join are included in the result. If \code{i} also has a key, it is \code{i}'s key columns that are used to match to \code{x}'s key columns, and a binary merge of the two tables is carried out.

  Advanced: When \code{i} is an expression of column names that evaluates to \code{data.table}, a join is performed. We call this a \emph{self join}.\cr\cr

  Advanced: When \code{i} is a single variable name, it is not considered an expression of column names, and is instead evaluated in calling scope.
  
  Advanced: When \code{i} is a regular \code{list} (such as \code{.BY}), it is automatically converted to \code{data.table}.

}
  \item{j}{ \code{list()} of expressions of column names, an expression or function call that evaluates to \code{list} (including \code{data.frame} and \code{data.table} which are \code{list}s, too), or (when \code{with=FALSE}) same as \code{j} in \code{[.data.frame}.

  \code{j} is evaluated within the frame of the \code{data.table} (i.e. it sees column names as if they are variables). The result is returned. \code{list()} may be omitted when there is just one expression, for convenience.\cr\cr

}
  \item{by}{ \code{list()} of expressions of column names, or a single character string containing comma separated column names, or a character vector of column names.

  The \code{list()} of expressions is evaluated within the frame of the data.table (i.e. it sees column names as if they are variables). The data.table is then grouped by the \code{by} and \code{j} is evaluated within the frame. The order of the rows within each group is preserved, as is the order of the groups. \code{list()} may be omitted when there is just one expression, typically a single unquoted column name.

  When \code{by} contains the first \code{n} columns of \code{x}'s key, we call this a \emph{keyed by}. In a keyed by the groups appear contiguously in RAM; memory is copied in bulk internally and this is faster. Otherwise, we call it an \emph{ad hoc by}.

  Advanced: Aggregation for a subset of known groups is particularly efficient when passing those groups in \code{i}. When \code{i} is a \code{data.table}, \code{DT[i,j]} evaluates \code{j} for each row of \code{i}. We call this \emph{by without by} or \emph{grouping by i}. Hence the self join \code{DT[data.table(unique(colA)),j]} is identical to \code{DT[,j,by=colA]}.

  Advanced: Objects \code{.SD}, \code{.BY} and \code{.N} may be used in the \code{j} expression.
  \code{.SD} is a \code{data.table} containing the subset of \code{x}'s data for each group, excluding the group column(s). It can be used when grouping by \code{i}, when grouping by \code{by}, keyed by, and ad hoc by.
  \code{.BY} is a \code{list} containing a length 1 vector for each item in \code{by}. This can be useful when \code{by} is not known in advance.
  \code{.N} is an integer, length 1, containing the number of rows in the group. This may be useful when the column names are not known in advance, and for convenience generally.

  Advanced: In the \code{X[Y,j]} form of grouping, the \code{j} expression sees variables in \code{X} first, then \code{Y}. We call this \emph{join inherited scope}. If the variable is not in \code{X} or \code{Y} then the calling frame is searched, its calling frame, and so on in the usual way up to and including the global environment.\cr\cr

}
  \item{with}{ By default \code{with=TRUE} and \code{j} is evaluated within the frame of \code{x}; the column names can be used as variables. When \code{with=FALSE}, \code{j} works as it does in \code{[.data.frame}.

}
  \item{nomatch}{ Same as \code{nomatch} in \code{\link{match}}. When a row in \code{i} has no match to \code{x}'s key, \code{nomatch=NA} means \code{NA} is returned for \code{x}'s non-join columns for that row of \code{i}. \code{0} means no rows will be returned for that row of \code{i}.

}
  \item{mult}{ When \emph{multiple} rows in \code{x} match to the row in \code{i}, \code{mult} controls which are returned: \code{"all"} (default), \code{"first"} or \code{"last"}.

}
  \item{roll}{ Applies to the last join column, generally a date but can be any ordered variable, irregular and including gaps. If \code{roll=TRUE} and \code{i}'s row matches to all but the last \code{x} join column, and its value in the last \code{i} join column falls in a gap (including after the last observation in \code{x} for that group), then the \emph{prevailing} value in \code{x} is \emph{rolled} forward. This operation is particularly fast using a modified binary search. The operation is also known as last observation carried forward (LOCF). Usually, there should be no duplicates in \code{x}'s key, the last key column is a date (or time, or datetime) and all the columns of \code{x}'s key are joined to. A common idiom is to select a contemporaneous regular time series (\code{dts}) across a set of identifiers (\code{ids}):  \code{DT[CJ(ids,dts),roll=TRUE]} where \code{DT} has a 2-column key (id,date) and \code{\link{CJ}} stands for \emph{cross join}.

}
  \item{rolltolast}{ Like \code{roll} but the data is not rolled forward past the \emph{last} observation. The value of \code{i} must fall in a gap in \code{x} but not after the end of the data for that group defined by all but the last join column. \code{roll} and \code{rolltolast} may not both be \code{TRUE}.

}
  \item{which}{ \code{TRUE} returns the integer row numbers of \code{x} that \code{i} matches to.

}
  \item{bysameorder}{ Advanced. \code{TRUE} tells \code{[.data.table} that the expressions in \code{by} are an order preserving map of \code{x}, allowing some efficiency gains. In most situations \code{bysameorder} is set to \code{TRUE} internally and there is no need to use this option. We hope to make this feature fully automatic in a future version and remove this argument.

}
  \item{.SDcols}{ Advanced. Specifies the columns of \code{x} included in \code{.SD}. May be character column names or numeric positions. This is useful for speed when applying a function through a subset of (possible very many) columns; e.g., \code{DT[,lapply(.SD,sum),by="x,y",.SDcols=301:350]}.
 
}
  \item{verbose}{ \code{TRUE} turns on status and information messages to the console. Turn this on by default using \code{options(datatable.verbose=TRUE)}. The quantity and types of verbosity may be expanded in future.

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

The package can be used just for rapid programming (compact syntax). Largest compute time benefits are on 64bit platforms with plentiful RAM, or when smaller datasets are repeatedly queried within a loop, or when other methods use so much working memory that they fail with an out of memory error.

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

Since a \code{list} \emph{is} a \code{vector}, \code{data.table} columns may be type \code{list}. Columns of type \code{list} can contain mixed types. Each item in a column of type \code{list} may be different lengths. This is true of \code{data.frame}, too.

Several \emph{methods} are provided for \code{data.table}, including \code{is.na}, \code{na.omit},
\code{t}, \code{rbind}, \code{cbind}, \code{merge} and others.
}
\references{
\code{data.table} homepage: \url{http://datatable.r-forge.r-project.org/}\cr
User reviews: \url{http://crantastic.org/packages/data-table}\cr
\url{http://en.wikipedia.org/wiki/Binary_search}\cr
\url{http://en.wikipedia.org/wiki/Radix_sort}             
}
\note{ If \code{keep.rownames} or \code{check.names} are supplied they must be written in full because \R does not allow partial argument names after \code{\dots}. For example \code{data.table(DF,keep=TRUE)} will create a
column called \code{"keep"} containing \code{TRUE} and this is correct behaviour; \code{data.table(DF,keep.rownames=TRUE)} was intended.
POSIXlt is not supported. Unexpected errors may occur if you manage to create a column of type POSIXlt. Please see NEWS for 1.6.3, and \code{\link{IDateTime}} instead.
}
\seealso{ \code{\link{data.frame}}, \code{\link{[.data.frame}}, \code{\link{as.data.table}}, \code{\link{setkey}}, \code{\link{J}}, \code{\link{SJ}}, \code{\link{CJ}}, \code{\link{merge.data.table}}, \code{\link{tables}}, \code{\link{test.data.table}}, \code{\link{IDateTime}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{
\dontrun{
example(data.table)  # to run these examples at the prompt}

DF = data.frame(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9)
DT = data.table(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9)
DF
DT
identical(dim(DT),dim(DF)) # TRUE
identical(DF$a, DT$a)      # TRUE
is.list(DF)                # TRUE
is.list(DT)                # TRUE

is.data.frame(DT)          # TRUE

tables()

DT[2]                      # 2nd row
DT[,v]                     # v column (as vector)
DT[,list(v)]               # v column (as data.table)
DT[2:3,sum(v)]             # sum(v) over rows 2 and 3
DT[2:5,cat(v,"\n")]        # just for j's side effect
DT[c(FALSE,TRUE)]          # even rows (usual recycling)

DT[,2,with=FALSE]          # 2nd column
colNum = 2
DT[,colNum,with=FALSE]     # same

setkey(DT,x)               # set a 1-column key
key(DT) = "x"              # same

DT["a"]                    # binary search (fast)
DT[x=="a"]                 # vector scan (slow)

DT[,sum(v),by=x]           # keyed by
DT[,sum(v),by=key(DT)]     # same
DT[,sum(v),by=y]           # ad hoc by

DT["a",sum(v)]             # j for one group
DT[c("a","b"),sum(v)]      # j for two groups

X = data.table(c("b","c"),foo=c(4,2))
X

DT[X]                      # join
DT[X,sum(v)]               # join and eval j for each row in i
DT[X,mult="first"]         # first row of each group
DT[X,mult="last"]          # last row of each group
DT[X,sum(v)*foo]           # join inherited scope

J("a",2)                   # J() is alias for data.table()
data.table("a",2)          # same

setkey(DT,x,y)             # 2-column key
key(DT) = c("x","y")       # same

DT["a"]                    # join to 1st column of key
DT[J("a")]                 # same
DT[J("a",3)]               # join to 2 columns
DT[J("a",3:6)]             # join 4 rows (2 missing)
DT[J("a",3:6),nomatch=0]   # remove missing
DT[J("a",3:6),roll=TRUE]   # rolling join (locf)

DT[,sum(v),by=list(y\%\%2)]  # by expression
DT[,.SD[2],by=x]           # 2nd row of each group
DT[,tail(.SD,2),by=x]      # last 2 rows of each group
DT[,lapply(.SD,sum),by=x]  # applying through columns by group

DT[,list(MySum=sum(v),
         MyMin=min(v),
         MyMax=max(v)),
    by=list(x,y\%\%2)]       # by 2 expressions

DT[,sum(v),x][V1<20]       # compound query
DT[,sum(v),x][order(-V1)]  # ordering results

DT[,transform(.SD,m=mean(v)),by=x] 
DT[,.SD[which.min(v)],by=x]

# Follow posting guide, support is here (not r-help) :
maintainer("data.table")

\dontrun{
vignette("datatable-intro")
vignette("datatable-faq")
vignette("datatable-timings")

test.data.table()          # over 200 low level tests

update.packages()          # keep up to date
}}
\keyword{ data }

