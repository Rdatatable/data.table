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
   \code{data.table} \emph{inherits} from \code{data.frame}. It offers fast subset, fast grouping, fast update, fast ordered joins and list columns in a short and flexible syntax, for faster development. It is inspired by \code{A[B]} syntax in \R where \code{A} is a matrix and \code{B} is a 2-column matrix. Since a \code{data.table} \emph{is} a \code{data.frame}, it is compatible with \R functions and packages that \emph{only} accept \code{data.frame}.
   
   The 10 minute quick start guide to \code{data.table} may be a good place to start: \href{../doc/datatable-intro.pdf}{\code{vignette("datatable-intro")}}. Or, the first section of FAQs is intended to be read from start to finish and is considered core documentation: \href{../doc/datatable-faq.pdf}{\code{vignette("datatable-faq")}}. If you have read and searched these documents and the help page below, please feel free to ask questions on \href{http://r.789695.n4.nabble.com/datatable-help-f2315188.html}{datatable-help} or the Stack Overflow \href{http://stackoverflow.com/questions/tagged/data.table}{data.table tag}. To report a bug please type: \code{bug.report(package="data.table")}.
   
   Please check the \href{http://datatable.r-forge.r-project.org/}{homepage} for up to the minute \href{http://r-forge.r-project.org/scm/viewvc.php/pkg/NEWS?view=markup&root=datatable}{news}.
   
   Tip: one of the quickest ways to learn the features is to type \code{example(data.table)} and study the output at the prompt.
   
   *NEW* : \itemize{
\item help page for \code{\link{:=}}
\item \code{keyby} argument
\item \code{character} and \code{numeric} now allowed as key column types
\item \code{\link{:=}} by group
}
}
\usage{
data.table(..., keep.rownames=FALSE, check.names=FALSE, key=NULL)

\method{[}{data.table}(x, i, j, by, keyby, with=TRUE,
  nomatch = getOption("datatable.nomatch"),   # default: NA_integer_
  mult = "all",
  roll = FALSE, rollends = if (roll=="nearest") c(TRUE,TRUE) else if (roll>=0) c(FALSE,TRUE) else c(TRUE,FALSE),
  which = FALSE,
  .SDcols,
  verbose=getOption("datatable.verbose"),     # default: FALSE
  allow.cartesian=getOption("datatable.allow.cartesian"),   # default: FALSE
  drop=NULL,
  rolltolast = FALSE   # deprecated
  )
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
  \item{i}{ Integer, logical or character vector, expression of column names, \code{list} or \code{data.table}.

  integer and logical vectors work the same way they do in \code{\link{[.data.frame}}. Other than \code{NA}s in logical \code{i} are treated as \code{FALSE} and a single \code{NA} logical is not recycled to match the number of rows, as it is in \code{[.data.frame}.

  character is matched to the first column of \code{x}'s key.

  expression is evaluated within the frame of the \code{data.table} (i.e. it sees column names as if they are variables) and can evaluate to any of the other types.

  When \code{i} is a \code{data.table}, \code{x} must have a key. \code{i} is \emph{joined} to \code{x} using \code{x}'s key and the rows in \code{x} that match are returned. An equi-join is performed between each column in \code{i} to each column in \code{x}'s key; i.e., column 1 of \code{i} is matched to the 1st column of \code{x}'s key, column 2 to the second, etc. The match is a binary search in compiled C in O(log n) time. If \code{i} has \emph{fewer} columns than \code{x}'s key then not all of {x}'s key columns will be joined to (a common use case) and many rows of \code{x} will (ordinarily) match to each row of \code{i}. If \code{i} has \emph{more} columns than \code{x}'s key, the columns of \code{i} not involved in the join are included in the result. If \code{i} also has a key, it is \code{i}'s key columns that are used to match to \code{x}'s key columns (column 1 of \code{i}'s key is joined to column 1 of \code{x}'s key, column 2 of \code{i}'s key to column 2 of \code{x}'s key, and so on for as long as the shorter key) and a binary merge of the two tables is carried out. In all joins the names of the columns are irrelevant; the columns of \code{x}'s key are joined to in order, either from column 1 onwards of \code{i} when \code{i} is unkeyed, or from column 1 onwards of \code{i}'s key. In code, the number of join columns is determined by \code{min(length(key(x)),if (haskey(i)) length(key(i)) else ncol(i))}.
  
  All types of `i` may be prefixed with \code{!}. This signals a \emph{not-join} or \emph{not-select} should be performed. Throughout \code{data.table} documentation, where we refer to the type of `i`, we mean the type of `i` \emph{after} the `!`, if present. See examples.

  Advanced: When \code{i} is an expression of column names that evaluates to \code{data.table} or \code{list}, a join is performed. We call this a \emph{self join}.

  Advanced: When \code{i} is a single variable name, it is not considered an expression of column names and is instead evaluated in calling scope.

}
  \item{j}{ A single column name, single expresson of column names, \code{list()} of expressions of column names, an expression or function call that evaluates to \code{list} (including \code{data.frame} and \code{data.table} which are \code{list}s, too), or (when \code{with=FALSE}) a vector of names or positions to select.

  \code{j} is evaluated within the frame of the \code{data.table}; i.e., it sees column names as if they are variables. Use \code{j=list(...)} to return multiple columns and/or expressions of columns. A single column or single expression returns that type, usually a vector. See the examples.

}
  \item{by}{ A single unquoted column name, a \code{list()} of expressions of column names, a single character string containing comma separated column names, or a character vector of column names.

  The \code{list()} of expressions is evaluated within the frame of the \code{data.table} (i.e. it sees column names as if they are variables). The \code{data.table} is then grouped by the \code{by} and \code{j} is evaluated within each group. The order of the rows within each group is preserved, as is the order of the groups. \code{j=list(...)} may be omitted when there is just one expression, for convenience, typically a single expression such as \code{sum(colB)}; e.g., \code{DT[,sum(colB),by=colA]}.

  When \code{by} contains the first \code{n} columns of \code{x}'s key, we call this a \emph{keyed by}. In a keyed by the groups appear contiguously in RAM and memory is copied in bulk internally, for extra speed. Otherwise, we call it an \emph{ad hoc by}. Ad hoc by is still many times faster than \code{tapply}, for example, but just not as fast as keyed by when datasets are very large, in particular when the size of \emph{each group} is large.

  Advanced: Aggregation for a subset of known groups is particularly efficient when passing those groups in \code{i}. When \code{i} is a \code{data.table}, \code{DT[i,j]} evaluates \code{j} for each row of \code{i}. We call this \emph{by without by} or \emph{grouping by i}. Hence, the self join \code{DT[data.table(unique(colA)),j]} is identical to \code{DT[,j,by=colA]}.

  Advanced: When grouping by \code{by} or by \code{i}, symbols \code{.SD}, \code{.BY}, \code{.N}, \code{.I} and \code{.GRP} may be used in the \code{j} expression, defined as follows.
  
  \code{.SD} is a \code{data.table} containing the \bold{S}ubset of \code{x}'s \bold{D}ata for each group, excluding any columns used in \code{by} (or \code{keyby}).
  
  \code{.BY} is a \code{list} containing a length 1 vector for each item in \code{by}. This can be useful when \code{by} is not known in advance. The \code{by} variables are also available to \code{j} directly by name; useful for example for titles of graphs if \code{j} is a plot command, or to branch with \code{if()} depending on the value of a group variable.
  
  \code{.N} is an integer, length 1, containing the number of rows in the group. This may be useful when the column names are not known in advance and for convenience generally. When grouping by \code{i}, \code{.N} is the number of rows in \code{x} matched to, for each row of \code{i}, regardless of whether \code{nomatch} is \code{NA} or \code{0}. It is renamed to \code{N} (no dot) in the result (otherwise a column called \code{".N"} could conflict with the \code{.N} variable, see FAQ 4.6 for more details and example), unless it is explicity named; e.g., \code{DT[,list(total=.N),by=a]}.
  
  \code{.I} is an integer vector length \code{.N} holding the row locations in \code{x} for this group. This is useful to subset in j; e.g. DT[,.I[which.max(somecol)],by=grp].
  
  \code{.GRP} is an integer, length 1, containing a simple group counter. 1 for the 1st group, 2 for the 2nd, etc.
  
  \code{.SD}, \code{.BY}, \code{.N}, \code{.I} and \code{.GRP} are \emph{read only}. Their bindings are locked and attempting to assign to them will generate an error. If you wish to manipulate \code{.SD} before returning it, take a \code{copy(.SD)} first (see FAQ 4.5). Using \code{:=} in the \code{j} of \code{.SD} is reserved for future use as a (tortuously) flexible way to update \code{DT} by reference by group (even when groups are not contiguous in an ad hoc by).

  Advanced: In the \code{X[Y,j]} form of grouping, the \code{j} expression sees variables in \code{X} first, then \code{Y}. We call this \emph{join inherited scope}. If the variable is not in \code{X} or \code{Y} then the calling frame is searched, its calling frame, and so on in the usual way up to and including the global environment.

}
  \item{keyby}{ An \emph{ad hoc by} just as \code{by} but with an additional \code{setkey()} on the \code{by} columns of the result, for convenience. Not to be confused with a \emph{keyed by} as defined above.
  
}
  \item{with}{ By default \code{with=TRUE} and \code{j} is evaluated within the frame of \code{x}. The column names can be used as variables. When \code{with=FALSE}, \code{j} is a vector of names or positions to select.

}
  \item{nomatch}{ Same as \code{nomatch} in \code{\link{match}}. When a row in \code{i} has no match to \code{x}'s key, \code{nomatch=NA} (default) means \code{NA} is returned for \code{x}'s non-join columns for that row of \code{i}. \code{0} means no rows will be returned for that row of \code{i}. The default value (used when \code{nomatch} is not supplied) can be changed from \code{NA} to \code{0} using \code{options(datatable.nomatch=0)}.

}
  \item{mult}{ When \emph{multiple} rows in \code{x} match to the row in \code{i}, \code{mult} controls which are returned: \code{"all"} (default), \code{"first"} or \code{"last"}.

}
  \item{roll}{ Applies to the last join column, generally a date but can be any ordered variable, irregular and including gaps. If \code{roll=TRUE} and \code{i}'s row matches to all but the last \code{x} join column, and its value in the last \code{i} join column falls in a gap (including after the last observation in \code{x} for that group), then the \emph{prevailing} value in \code{x} is \emph{rolled} forward. This operation is particularly fast using a modified binary search. The operation is also known as last observation carried forward (LOCF). Usually, there should be no duplicates in \code{x}'s key, the last key column is a date (or time, or datetime) and all the columns of \code{x}'s key are joined to. A common idiom is to select a contemporaneous regular time series (\code{dts}) across a set of identifiers (\code{ids}):  \code{DT[CJ(ids,dts),roll=TRUE]} where \code{DT} has a 2-column key (id,date) and \code{\link{CJ}} stands for \emph{cross join}.
  When \code{roll} is a positive number, this limits how far values are carried forward. \code{roll=TRUE} is equivalent to \code{roll=+Inf}.
  When \code{roll} is a negative number, values are rolled backwards; i.e., next observation carried backwards (NOCB). Use \code{-Inf} for unlimited roll back.
  When \code{roll} is \code{"nearest"}, the nearest value is joined to.
  
}
  \item{rollends}{ A logical vector length 2 (a single logical is recycled). When rolling forward (e.g. \code{roll=TRUE}) if a value is past the \emph{last} observation within each group defined by the join columns, \code{rollends[2]=TRUE} will roll the last value forwards. \code{rollends[1]=TRUE} will roll the first value backwards if the value is before it. If \code{rollends=FALSE} the value of \code{i} must fall in a gap in \code{x} but not after the end or before the beginning of the data, for that group defined by all but the last join column. When \code{roll} is a finite number, that limit is also applied when rolling the ends.
  
}
  \item{which}{ \code{TRUE} returns the row numbers of \code{x} that \code{i} matches to. \code{NA} returns the row numbers of \code{i} that have no match in \code{x}. By default \code{FALSE} and the rows in \code{x} that match are returned.

}
  \item{.SDcols}{ Advanced. Specifies the columns of \code{x} included in \code{.SD}. May be character column names or numeric positions. This is useful for speed when applying a function through a subset of (possible very many) columns; e.g., \code{DT[,lapply(.SD,sum),by="x,y",.SDcols=301:350]}.
 
}
  \item{verbose}{ \code{TRUE} turns on status and information messages to the console. Turn this on by default using \code{options(datatable.verbose=TRUE)}. The quantity and types of verbosity may be expanded in future.

}  
  \item{allow.cartesian}{ \code{FALSE} prevents joins that would result in more than \code{max(nrow(x),nrow(i))} rows. This is usually caused by duplicate values in \code{i}'s join columns, each of which join to the same group in `x` over and over again: a \emph{misspecified} join. Usually this was not intended and the join needs to be changed. The word 'cartesian' is used loosely in this context. The traditional cartesian join is (deliberately) difficult to achieve in \code{data.table}: where every row in \code{i} joins to every row in \code{x} (a \code{nrow(x)*nrow(i)} row result). 'cartesian' is just meant in a 'large multiplicative' sense. }
  
  \item{drop}{ Never used by \code{data.table}. Do not use. It needs to be here because \code{data.table} inherits from \code{data.frame}. See \code{vignette("datatable-faq")}.
  
}
  \item{rolltolast}{ Deprecated. Setting \code{rolltolast=TRUE} is converted to \code{roll=TRUE;rollends=FALSE} for backwards compatibility.
  
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

As with \code{[.data.frame}, \emph{compound queries} can be concatenated on one line; e.g., 
\preformatted{
    DT[,sum(v),by=colA][V1<300][tail(order(V1))]
    # sum(v) by colA then return the 6 largest which are under 300
}
The \code{j} expression does not have to return data; e.g.,
\preformatted{
    DT[,plot(colB,colC),by=colA]
    # produce a set of plots (likely to pdf) returning no data
}
Multiple \code{data.table}s (e.g. \code{X}, \code{Y} and \code{Z}) can be joined in many ways; e.g.,
\preformatted{
    X[Y][Z]
    X[Z][Y]
    X[Y[Z]]
    X[Z[Y]]
}
A \code{data.table} is a \code{list} of vectors, just like a \code{data.frame}. However :
\enumerate{
\item it never has rownames. Instead it may have one \emph{key} of one or more columns. This key can be used for row indexing instead of rownames.
\item it has enhanced functionality in \code{[.data.table} for fast joins of keyed tables, fast aggregation, fast last observation carried forward (LOCF) and fast add/modify/delete of columns by reference with no copy at all.
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
\note{ If \code{keep.rownames} or \code{check.names} are supplied they must be written in full because \R does not allow partial argument names after `\code{\dots}`. For example, \code{data.table(DF,keep=TRUE)} will create a
column called \code{"keep"} containing \code{TRUE} and this is correct behaviour; \code{data.table(DF,keep.rownames=TRUE)} was intended.

POSIXlt is not supported as a column type because it uses 40 bytes to store a single datetime. Unexpected errors may occur if you manage to create a column of type POSIXlt. Please see \href{http://r-forge.r-project.org/scm/viewvc.php/pkg/NEWS?view=markup&root=datatable}{NEWS} for 1.6.3, and \code{\link{IDateTime}} instead. IDateTime has methods to convert to and from POSIXlt.
}
\seealso{ \code{\link{data.frame}}, \code{\link{[.data.frame}}, \code{\link{as.data.table}}, \code{\link{setkey}}, \code{\link{J}}, \code{\link{SJ}}, \code{\link{CJ}}, \code{\link{merge.data.table}}, \code{\link{tables}}, \code{\link{test.data.table}}, \code{\link{IDateTime}}, \code{\link{unique.data.table}}, \code{\link{copy}}, \code{\link{:=}}, \code{\link{alloc.col}}, \code{\link{truelength}}, \code{\link{rbindlist}}
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

setkey(DT,x)               # set a 1-column key. No quotes, for convenience.
setkeyv(DT,"x")            # same (v in setkeyv stands for vector)
v="x"
setkeyv(DT,v)              # same
# key(DT)<-"x"             # copies whole table, please use set* functions instead

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

setkey(DT,x,y)             # 2-column key
setkeyv(DT,c("x","y"))     # same

DT["a"]                    # join to 1st column of key
DT[J("a")]                 # same. J() stands for Join, an alias for list()
DT[list("a")]              # same
DT[.("a")]                 # same. In the style of package plyr.
DT[J("a",3)]               # join to 2 columns
DT[.("a",3)]               # same
DT[J("a",3:6)]             # join 4 rows (2 missing)
DT[J("a",3:6),nomatch=0]   # remove missing
DT[J("a",3:6),roll=TRUE]   # rolling join (locf)

DT[,sum(v),by=list(y\%\%2)]  # by expression
DT[,.SD[2],by=x]           # 2nd row of each group
DT[,tail(.SD,2),by=x]      # last 2 rows of each group
DT[,lapply(.SD,sum),by=x]  # apply through columns by group

DT[,list(MySum=sum(v),
         MyMin=min(v),
         MyMax=max(v)),
    by=list(x,y\%\%2)]       # by 2 expressions

DT[,sum(v),x][V1<20]       # compound query
DT[,sum(v),x][order(-V1)]  # ordering results

print(DT[,z:=42L])         # add new column by reference
print(DT[,z:=NULL])        # remove column by reference
print(DT["a",v:=42L])      # subassign to existing v column by reference
print(DT["b",v2:=84L])     # subassign to new column by reference (NA padded)

DT[,m:=mean(v),by=x][]     # add new column by reference by group
                           # NB: postfix [] is shortcut to print()

DT[,.SD[which.min(v)],by=x][]  # nested query by group

DT[!J("a")]                # not join
DT[!"a"]                   # same
DT[!2:4]                   # all rows other than 2:4
DT[x!="b" | y!=3]          # multiple vector scanning approach, slow
DT[!J("b",3)]              # same result but much faster


# Follow r-help posting guide, support is here (*not* r-help) :
# datatable-help@lists.r-forge.r-project.org
# or
# http://stackoverflow.com/questions/tagged/data.table

\dontrun{
vignette("datatable-intro")
vignette("datatable-faq")
vignette("datatable-timings")

test.data.table()          # over 700 low level tests

update.packages()          # keep up to date
}}
\keyword{ data }

