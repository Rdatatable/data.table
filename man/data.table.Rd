\name{data.table-package}
\alias{data.table-package}
\docType{package}
\alias{data.table}
\alias{Ops.data.table}
\alias{is.na.data.table}
\alias{[.data.table}
\title{ Enhanced data.frame }
\description{
   \code{data.table} \emph{inherits} from \code{data.frame}. It offers fast and memory efficient: file reader and writer, aggregations, updates, equi, non-equi, rolling, range and interval joins, in a short and flexible syntax, for faster development.

   It is inspired by \code{A[B]} syntax in \R where \code{A} is a matrix and \code{B} is a 2-column matrix. Since a \code{data.table} \emph{is} a \code{data.frame}, it is compatible with \R functions and packages that accept \emph{only} \code{data.frame}s.

   Type \code{vignette(package="data.table")} to get started. The \href{../doc/datatable-intro.html}{Introduction to data.table} vignette introduces \code{data.table}'s \code{x[i, j, by]} syntax and is a good place to start. If you have read the vignettes and the help page below, please read the \href{https://github.com/Rdatatable/data.table/wiki/Support}{data.table support guide}.

   Please check the \href{https://github.com/Rdatatable/data.table/wiki}{homepage} for up to the minute live NEWS.

   Tip: one of the \emph{quickest} ways to learn the features is to type \code{example(data.table)} and study the output at the prompt.
}
\usage{
data.table(\dots, keep.rownames=FALSE, check.names=FALSE, key=NULL, stringsAsFactors=FALSE)

\method{[}{data.table}(x, i, j, by, keyby, with = TRUE,
  nomatch = getOption("datatable.nomatch", NA),
  mult = "all",
  roll = FALSE,
  rollends = if (roll=="nearest") c(TRUE,TRUE)
             else if (roll>=0) c(FALSE,TRUE)
             else c(TRUE,FALSE),
  which = FALSE,
  .SDcols,
  verbose = getOption("datatable.verbose"),                   # default: FALSE
  allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
  drop = NULL, on = NULL)
}
\arguments{
    \item{\dots}{ Just as \code{\dots} in \code{\link{data.frame}}. Usual recycling rules are applied to vectors of different lengths to create a list of equal length vectors.}

    \item{keep.rownames}{ If \code{\dots} is a \code{matrix} or \code{data.frame}, \code{TRUE} will retain the rownames of that object in a column named \code{rn}.}

    \item{check.names}{ Just as \code{check.names} in \code{\link{data.frame}}.}

    \item{key}{ Character vector of one or more column names which is passed to \code{\link{setkey}}. It may be a single comma separated string such as \code{key="x,y,z"}, or a vector of names such as \code{key=c("x","y","z")}.}

    \item{stringsAsFactors}{Logical (default is \code{FALSE}). Convert all \code{character} columns to \code{factor}s?}

    \item{x}{ A \code{data.table}.}

    \item{i}{ Integer, logical or character vector, single column numeric \code{matrix}, expression of column names, \code{list}, \code{data.frame} or \code{data.table}.

        \code{integer} and \code{logical} vectors work the same way they do in \code{\link{[.data.frame}} except logical \code{NA}s are treated as FALSE.

        \code{expression} is evaluated within the frame of the \code{data.table} (i.e. it sees column names as if they are variables) and can evaluate to any of the other types.

        \code{character}, \code{list} and \code{data.frame} input to \code{i} is converted into a \code{data.table} internally using \code{\link{as.data.table}}.

        If \code{i} is a \code{data.table}, the columns in \code{i} to be matched against \code{x} can be specified using one of these ways:

        \itemize{
            \item{\code{on} argument (see below). It allows for both \code{equi-} and the newly implemented \code{non-equi} joins.}

            \item{If not, \code{x} \emph{must be keyed}. Key can be set using \code{\link{setkey}}. If \code{i} is also keyed, then first \emph{key} column of \code{i} is matched against first \emph{key} column of \code{x}, second against second, etc..

            If \code{i} is not keyed, then first column of \code{i} is matched against first \emph{key} column of \code{x}, second column of \code{i} against second \emph{key} column of \code{x}, etc\ldots

            This is summarised in code as \code{min(length(key(x)), if (haskey(i)) length(key(i)) else ncol(i))}.}
        }
        Using \code{on=} is recommended (even during keyed joins) as it helps understand the code better and also allows for \emph{non-equi} joins.

        When the binary operator \code{==} alone is used, an \emph{equi} join is performed. In SQL terms, \code{x[i]} then performs a \emph{right join} by default. \code{i} prefixed with \code{!} signals a \emph{not-join} or \emph{not-select}.

        Support for \emph{non-equi} join was recently implemented, which allows for other binary operators \code{>=, >, <= and <}.

        See \href{../doc/datatable-keys-fast-subset.html}{\code{vignette("datatable-keys-fast-subset")}} and \href{../doc/datatable-secondary-indices-and-auto-indexing.html}{\code{vignette("datatable-secondary-indices-and-auto-indexing")}}.

        \emph{Advanced:} When \code{i} is a single variable name, it is not considered an expression of column names and is instead evaluated in calling scope.
    }

    \item{j}{When \code{with=TRUE} (default), \code{j} is evaluated within the frame of the data.table; i.e., it sees column names as if they are variables. This allows to not just \emph{select} columns in \code{j}, but also \code{compute} on them e.g., \code{x[, a]} and \code{x[, sum(a)]} returns \code{x$a} and \code{sum(x$a)} as a vector respectively. \code{x[, .(a, b)]} and \code{x[, .(sa=sum(a), sb=sum(b))]} returns a two column data.table each, the first simply \emph{selecting} columns \code{a, b} and the second \emph{computing} their sums.

    As long as \code{j} returns a \code{list}, each element of the list becomes a column in the resulting \code{data.table}. When the output of \code{j} is not a \code{list}, the output is returned as-is (e.g. \code{x[ , a]} returns the column vector \code{a}), unless \code{by} is used, in which case it is implicitly wrapped in \code{list} for convenience (e.g. \code{x[ , sum(a), by=b]} will create a column named \code{V1} with value \code{sum(a)} for each group).

    The expression `.()` is a \emph{shorthand} alias to \code{list()}; they both mean the same. (An exception is made for the use of \code{.()} within a call to \code{\link{bquote}}, where \code{.()} is left unchanged.)

    When \code{j} is a vector of column names or positions to select (as in \code{data.frame}). There is no need to use \code{with=FALSE} anymore. Note that \code{with=FALSE} is still necessary when using a logical vector with length \code{ncol(x)} to include/exclude columns. Note: if a logical vector with length \code{k < ncol(x)} is passed, it will be filled to length \code{ncol(x)} with \code{FALSE}, which is different from \code{data.frame}, where the vector is recycled.

    \emph{Advanced:} \code{j} also allows the use of special \emph{read-only} symbols: \code{\link{.SD}}, \code{\link{.N}}, \code{\link{.I}}, \code{\link{.GRP}}, \code{\link{.BY}}. See \code{\link{special-symbols}} and the Examples below for more.

    \emph{Advanced:} When \code{i} is a \code{data.table}, the columns of \code{i} can be referred to in \code{j} by using the prefix \code{i.}, e.g., \code{X[Y, .(val, i.val)]}. Here \code{val} refers to \code{X}'s column and \code{i.val} \code{Y}'s.

    \emph{Advanced:} Columns of \code{x} can now be referred to using the prefix \code{x.} and is particularly useful during joining to refer to \code{x}'s \emph{join} columns as they are otherwise masked by \code{i}'s. For example, \code{X[Y, .(x.a-i.a, b), on="a"]}.

    See \href{../doc/datatable-intro.html}{\code{vignette("datatable-intro")}} and \code{example(data.table)}.}

    \item{by}{ Column names are seen as if they are variables (as in \code{j} when \code{with=TRUE}). The \code{data.table} is then grouped by the \code{by} and \code{j} is evaluated within each group. The order of the rows within each group is preserved, as is the order of the groups. \code{by} accepts:

    \itemize{
        \item{A single unquoted column name: e.g., \code{DT[, .(sa=sum(a)), by=x]}}

        \item{a \code{list()} of expressions of column names: e.g., \code{DT[, .(sa=sum(a)), by=.(x=x>0, y)]}}

        \item{a single character string containing comma separated column names (where spaces are significant since column names may contain spaces even at the start or end): e.g., \code{DT[, sum(a), by="x,y,z"]}}

        \item{a character vector of column names: e.g., \code{DT[, sum(a), by=c("x", "y")]}}

        \item{or of the form \code{startcol:endcol}: e.g., \code{DT[, sum(a), by=x:z]}}
    }

    \emph{Advanced:} When \code{i} is a \code{list} (or \code{data.frame} or \code{data.table}), \code{DT[i, j, by=.EACHI]} evaluates \code{j} for the groups in `DT` that each row in \code{i} joins to. That is, you can join (in \code{i}) and aggregate (in \code{j}) simultaneously. We call this \emph{grouping by each i}. See \href{http://stackoverflow.com/a/27004566/559784}{this StackOverflow answer} for a more detailed explanation until we \href{https://github.com/Rdatatable/data.table/issues/944}{roll out vignettes}.

    \emph{Advanced:} In the \code{X[Y, j]} form of grouping, the \code{j} expression sees variables in \code{X} first, then \code{Y}. We call this \emph{join inherited scope}. If the variable is not in \code{X} or \code{Y} then the calling frame is searched, its calling frame, and so on in the usual way up to and including the global environment.}

    \item{keyby}{ Same as \code{by}, but with an additional \code{setkey()} run on the \code{by} columns of the result, for convenience. It is common practice to use `keyby=` routinely when you wish the result to be sorted.}

    \item{with}{ By default \code{with=TRUE} and \code{j} is evaluated within the frame of \code{x}; column names can be used as variables. In case of overlapping variables names inside dataset and in parent scope you can use double dot prefix \code{..cols} to explicitly refer to `\code{cols} variable parent scope and not from your dataset.

        When \code{j} is a character vector of column names, a numeric vector of column positions to select or of the form \code{startcol:endcol}, and the value returned is always a \code{data.table}. \code{with=FALSE} is not necessary anymore to select columns dynamically. Note that \code{x[, cols]} is equivalent to \code{x[, ..cols]} and to \code{x[, cols, with=FALSE]} and to \code{x[, .SD, .SDcols=cols]}.}

    \item{nomatch}{ When a row in \code{i} has no match to \code{x}, \code{nomatch=NA} (default) means \code{NA} is returned. \code{NULL} (or \code{0} for backward compatibility) means no rows will be returned for that row of \code{i}. Use \code{options(datatable.nomatch=NULL)} to change the default value (used when \code{nomatch} is not supplied).}

    \item{mult}{ When \code{i} is a \code{list} (or \code{data.frame} or \code{data.table}) and \emph{multiple} rows in \code{x} match to the row in \code{i}, \code{mult} controls which are returned: \code{"all"} (default), \code{"first"} or \code{"last"}.}

    \item{roll}{ When \code{i} is a \code{data.table} and its row matches to all but the last \code{x} join column, and its value in the last \code{i} join column falls in a gap (including after the last observation in \code{x} for that group), then:

        \itemize{
            \item{\code{+Inf} (or \code{TRUE}) rolls the \emph{prevailing} value in \code{x} forward. It is also known as last observation carried forward (LOCF).}
            \item{\code{-Inf} rolls backwards instead; i.e., next observation carried backward (NOCB).}
            \item{finite positive or negative number limits how far values are carried forward or backward.}
            \item{"nearest" rolls the nearest value instead.}
        }
        Rolling joins apply to the last join column, generally a date but can be any variable. It is particularly fast using a modified binary search.

        A common idiom is to select a contemporaneous regular time series (\code{dts}) across a set of identifiers (\code{ids}):  \code{DT[CJ(ids,dts),roll=TRUE]} where \code{DT} has a 2-column key (id,date) and \code{\link{CJ}} stands for \emph{cross join}.}

    \item{rollends}{ A logical vector length 2 (a single logical is recycled) indicating whether values falling before the first value or after the last value for a group should be rolled as well.
        \itemize{
            \item{If \code{rollends[2]=TRUE}, it will roll the last value forward. \code{TRUE} by default for LOCF and \code{FALSE} for NOCB rolls.}
            \item{If \code{rollends[1]=TRUE}, it will roll the first value backward. \code{TRUE} by default for NOCB and \code{FALSE} for LOCF rolls.}
        }
        When \code{roll} is a finite number, that limit is also applied when rolling the ends.}

    \item{which}{\code{TRUE} returns the row numbers of \code{x} that \code{i} matches to. If \code{NA}, returns the row numbers of \code{i} that have no match in \code{x}. By default \code{FALSE} and the rows in \code{x} that match are returned.}

    \item{.SDcols}{ Specifies the columns of \code{x} to be included in the special symbol \code{\link{.SD}} which stands for \code{Subset of data.table}. May be character column names or numeric positions. This is useful for speed when applying a function through a subset of (possible very many) columns; e.g., \code{DT[, lapply(.SD, sum), by="x,y", .SDcols=301:350]}.

    For convenient interactive use, the form \code{startcol:endcol} is also allowed (as in \code{by}), e.g., \code{DT[, lapply(.SD, sum), by=x:y, .SDcols=a:f]}.

    Inversion (column dropping instead of keeping) can be accomplished be prepending the argument with \code{!} or \code{-} (there's no difference between these), e.g. \code{.SDcols = !c('x', 'y')}.

    Finally, you can filter columns to include in \code{.SD} based on their \emph{names} according to regular expressions via \code{.SDcols=patterns(regex1, regex2, ...)}. The included columns will be the \emph{intersection} of the columns identified by each pattern; pattern unions can easily be specified with \code{|} in a regex. You can filter columns on \code{values}  by passing a function, e.g. \code{.SDcols=\link{is.numeric}}. You can also invert a pattern as usual with \code{.SDcols=!patterns(...)} or \code{.SDcols=!is.numeric}.
}
  \item{verbose}{ \code{TRUE} turns on status and information messages to the console. Turn this on by default using \code{options(datatable.verbose=TRUE)}. The quantity and types of verbosity may be expanded in future.

}
  \item{allow.cartesian}{ \code{FALSE} prevents joins that would result in more than \code{nrow(x)+nrow(i)} rows. This is usually caused by duplicate values in \code{i}'s join columns, each of which join to the same group in `x` over and over again: a \emph{misspecified} join. Usually this was not intended and the join needs to be changed. The word 'cartesian' is used loosely in this context. The traditional cartesian join is (deliberately) difficult to achieve in \code{data.table}: where every row in \code{i} joins to every row in \code{x} (a \code{nrow(x)*nrow(i)} row result). 'cartesian' is just meant in a 'large multiplicative' sense, so FALSE does not always prevent a traditional cartesian join. }

  \item{drop}{ Never used by \code{data.table}. Do not use. It needs to be here because \code{data.table} inherits from \code{data.frame}. See \href{../doc/datatable-faq.html}{\code{vignette("datatable-faq")}}.}

  \item{on}{ Indicate which columns in \code{x} should be joined with which columns in \code{i} along with the type of binary operator to join with (see non-equi joins below on this). When specified, this overrides the keys set on \code{x} and \code{i}. When \code{.NATURAL} keyword provided then \emph{natural join} is made (join on common columns). There are multiple ways of specifying the \code{on} argument:
        \itemize{
            \item{As an unnamed character vector, e.g., \code{X[Y, on=c("a", "b")]}, used when columns \code{a} and \code{b} are common to both \code{X} and \code{Y}.}
            \item{\emph{Foreign key joins}: As a \emph{named} character vector when the join columns have different names in \code{X} and \code{Y}.
            For example, \code{X[Y, on=c(x1="y1", x2="y2")]} joins \code{X} and \code{Y} by matching columns \code{x1} and \code{x2} in \code{X} with columns \code{y1} and \code{y2} in \code{Y}, respectively.

            From v1.9.8, you can also express foreign key joins using the binary operator \code{==}, e.g. \code{X[Y, on=c("x1==y1", "x2==y2")]}.

            NB: shorthand like \code{X[Y, on=c("a", V2="b")]} is also possible if, e.g., column \code{"a"} is common between the two tables.}
            \item{For convenience during interactive scenarios, it is also possible to use \code{.()} syntax as \code{X[Y, on=.(a, b)]}.}
            \item{From v1.9.8, (non-equi) joins using binary operators \code{>=, >, <=, <} are also possible, e.g., \code{X[Y, on=c("x>=a", "y<=b")]}, or for interactive use as \code{X[Y, on=.(x>=a, y<=b)]}.}
        }
        See examples as well as \href{../doc/datatable-secondary-indices-and-auto-indexing.html}{\code{vignette("datatable-secondary-indices-and-auto-indexing")}}.
    }
}
\details{
\code{data.table} builds on base \R functionality to reduce 2 types of time:\cr

\enumerate{
    \item{programming time (easier to write, read, debug and maintain), and}
    \item{compute time (fast and memory efficient).}
}

The general form of data.table syntax is:\cr

\preformatted{
    DT[ i,  j,  by ] # + extra arguments
        |   |   |
        |   |    -------> grouped by what?
        |    -------> what to do?
         ---> on which rows?
}

The way to read this out loud is: "Take \code{DT}, subset rows by \code{i}, \emph{then} compute \code{j} grouped by \code{by}. Here are some basic usage examples expanding on this definition. See the vignette (and examples) for working examples.

\preformatted{
    X[, a]                      # return col 'a' from X as vector. If not found, search in parent frame.
    X[, .(a)]                   # same as above, but return as a data.table.
    X[, sum(a)]                 # return sum(a) as a vector (with same scoping rules as above)
    X[, .(sum(a)), by=c]        # get sum(a) grouped by 'c'.
    X[, sum(a), by=c]           # same as above, .() can be omitted in j and by on single expression for convenience
    X[, sum(a), by=c:f]         # get sum(a) grouped by all columns in between 'c' and 'f' (both inclusive)

    X[, sum(a), keyby=b]        # get sum(a) grouped by 'b', and sort that result by the grouping column 'b'
    X[, sum(a), by=b][order(b)] # same order as above, but by chaining compound expressions
    X[c>1, sum(a), by=c]        # get rows where c>1 is TRUE, and on those rows, get sum(a) grouped by 'c'
    X[Y, .(a, b), on="c"]       # get rows where Y$c == X$c, and select columns 'X$a' and 'X$b' for those rows
    X[Y, .(a, i.a), on="c"]     # get rows where Y$c == X$c, and then select 'X$a' and 'Y$a' (=i.a)
    X[Y, sum(a*i.a), on="c" by=.EACHI] # for *each* 'Y$c', get sum(a*i.a) on matching rows in 'X$c'

    X[, plot(a, b), by=c]       # j accepts any expression, generates plot for each group and returns no data
    # see ?assign to add/update/delete columns by reference using the same consistent interface
}

A \code{data.table} is a \code{list} of vectors, just like a \code{data.frame}. However :
\enumerate{
\item it never has or uses rownames. Rownames based indexing can be done by setting a \emph{key} of one or more columns or done \emph{ad-hoc} using the \code{on} argument (now preferred).
\item it has enhanced functionality in \code{[.data.table} for fast joins of keyed tables, fast aggregation, fast last observation carried forward (LOCF) and fast add/modify/delete of columns by reference with no copy at all.
}

See the \code{see also} section for the several other \emph{methods} that are available for operating on data.tables efficiently.

}
\references{
\url{https://github.com/Rdatatable/data.table/wiki} (\code{data.table} homepage)\cr
\url{http://en.wikipedia.org/wiki/Binary_search}
}
\note{ If \code{keep.rownames} or \code{check.names} are supplied they must be written in full because \R does not allow partial argument names after `\code{\dots}`. For example, \code{data.table(DF, keep=TRUE)} will create a
column called \code{"keep"} containing \code{TRUE} and this is correct behaviour; \code{data.table(DF, keep.rownames=TRUE)} was intended.

\code{POSIXlt} is not supported as a column type because it uses 40 bytes to store a single datetime. They are implicitly converted to \code{POSIXct} type with \emph{warning}. You may also be interested in \code{\link{IDateTime}} instead; it has methods to convert to and from \code{POSIXlt}.
}
\seealso{ \code{\link{special-symbols}}, \code{\link{data.frame}}, \code{\link{[.data.frame}}, \code{\link{as.data.table}}, \code{\link{setkey}}, \code{\link{setorder}}, \code{\link{setDT}}, \code{\link{setDF}}, \code{\link{J}}, \code{\link{SJ}}, \code{\link{CJ}}, \code{\link{merge.data.table}}, \code{\link{tables}}, \code{\link{test.data.table}}, \code{\link{IDateTime}}, \code{\link{unique.data.table}}, \code{\link{copy}}, \code{\link{:=}}, \code{\link{setalloccol}}, \code{\link{truelength}}, \code{\link{rbindlist}}, \code{\link{setNumericRounding}}, \code{\link{datatable-optimize}}, \code{\link{fsetdiff}}, \code{\link{funion}}, \code{\link{fintersect}}, \code{\link{fsetequal}}, \code{\link{anyDuplicated}}, \code{\link{uniqueN}}, \code{\link{rowid}}, \code{\link{rleid}}, \code{\link{na.omit}}, \code{\link{frank}} }
\examples{
\dontrun{
example(data.table)  # to run these examples yourself
}
DF = data.frame(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
DT = data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
DF
DT
identical(dim(DT), dim(DF))    # TRUE
identical(DF$a, DT$a)          # TRUE
is.list(DF)                    # TRUE
is.list(DT)                    # TRUE

is.data.frame(DT)              # TRUE

tables()

# basic row subset operations
DT[2]                          # 2nd row
DT[3:2]                        # 3rd and 2nd row
DT[order(x)]                   # no need for order(DT$x)
DT[order(x), ]                 # same as above. The ',' is optional
DT[y>2]                        # all rows where DT$y > 2
DT[y>2 & v>5]                  # compound logical expressions
DT[!2:4]                       # all rows other than 2:4
DT[-(2:4)]                     # same

# select|compute columns data.table way
DT[, v]                        # v column (as vector)
DT[, list(v)]                  # v column (as data.table)
DT[, .(v)]                     # same as above, .() is a shorthand alias to list()
DT[, sum(v)]                   # sum of column v, returned as vector
DT[, .(sum(v))]                # same, but return data.table (column autonamed V1)
DT[, .(sv=sum(v))]             # same, but column named "sv"
DT[, .(v, v*2)]                # return two column data.table, v and v*2

# subset rows and select|compute data.table way
DT[2:3, sum(v)]                # sum(v) over rows 2 and 3, return vector
DT[2:3, .(sum(v))]             # same, but return data.table with column V1
DT[2:3, .(sv=sum(v))]          # same, but return data.table with column sv
DT[2:5, cat(v, "\n")]          # just for j's side effect

# select columns the data.frame way
DT[, 2]                        # 2nd column, returns a data.table always
colNum = 2                     # to refer vars in `j` from the outside of data use `..` prefix
DT[, ..colNum]                 # same, equivalent to DT[, .SD, .SDcols=colNum]
DT[["v"]]                      # same as DT[, v] but much faster

# grouping operations - j and by
DT[, sum(v), by=x]             # ad hoc by, order of groups preserved in result
DT[, sum(v), keyby=x]          # same, but order the result on by cols
DT[, sum(v), by=x][order(x)]   # same but by chaining expressions together

# fast ad hoc row subsets (subsets as joins)
DT["a", on="x"]                # same as x == "a" but uses binary search (fast)
DT["a", on=.(x)]               # same, for convenience, no need to quote every column
DT[.("a"), on="x"]             # same
DT[x=="a"]                     # same, single "==" internally optimised to use binary search (fast)
DT[x!="b" | y!=3]              # not yet optimized, currently vector scan subset
DT[.("b", 3), on=c("x", "y")]  # join on columns x,y of DT; uses binary search (fast)
DT[.("b", 3), on=.(x, y)]      # same, but using on=.()
DT[.("b", 1:2), on=c("x", "y")]             # no match returns NA
DT[.("b", 1:2), on=.(x, y), nomatch=NULL]   # no match row is not returned
DT[.("b", 1:2), on=c("x", "y"), roll=Inf]   # locf, nomatch row gets rolled by previous row
DT[.("b", 1:2), on=.(x, y), roll=-Inf]      # nocb, nomatch row gets rolled by next row
DT["b", sum(v*y), on="x"]                   # on rows where DT$x=="b", calculate sum(v*y)

# all together now
DT[x!="a", sum(v), by=x]                    # get sum(v) by "x" for each i != "a"
DT[!"a", sum(v), by=.EACHI, on="x"]         # same, but using subsets-as-joins
DT[c("b","c"), sum(v), by=.EACHI, on="x"]   # same
DT[c("b","c"), sum(v), by=.EACHI, on=.(x)]  # same, using on=.()

# joins as subsets
X = data.table(x=c("c","b"), v=8:7, foo=c(4,2))
X

DT[X, on="x"]                         # right join
X[DT, on="x"]                         # left join
DT[X, on="x", nomatch=NULL]           # inner join
DT[!X, on="x"]                        # not join
DT[X, on=c(y="v")]                    # join using column "y" of DT with column "v" of X
DT[X, on="y==v"]                      # same as above (v1.9.8+)

DT[X, on=.(y<=foo)]                   # NEW non-equi join (v1.9.8+)
DT[X, on="y<=foo"]                    # same as above
DT[X, on=c("y<=foo")]                 # same as above
DT[X, on=.(y>=foo)]                   # NEW non-equi join (v1.9.8+)
DT[X, on=.(x, y<=foo)]                # NEW non-equi join (v1.9.8+)
DT[X, .(x,y,x.y,v), on=.(x, y>=foo)]  # Select x's join columns as well

DT[X, on="x", mult="first"]           # first row of each group
DT[X, on="x", mult="last"]            # last row of each group
DT[X, sum(v), by=.EACHI, on="x"]      # join and eval j for each row in i
DT[X, sum(v)*foo, by=.EACHI, on="x"]  # join inherited scope
DT[X, sum(v)*i.v, by=.EACHI, on="x"]  # 'i,v' refers to X's v column
DT[X, on=.(x, v>=v), sum(y)*foo, by=.EACHI] # NEW non-equi join with by=.EACHI (v1.9.8+)

# setting keys
kDT = copy(DT)                        # (deep) copy DT to kDT to work with it.
setkey(kDT,x)                         # set a 1-column key. No quotes, for convenience.
setkeyv(kDT,"x")                      # same (v in setkeyv stands for vector)
v="x"
setkeyv(kDT,v)                        # same
# key(kDT)<-"x"                       # copies whole table, please use set* functions instead
haskey(kDT)                           # TRUE
key(kDT)                              # "x"

# fast *keyed* subsets
kDT["a"]                              # subset-as-join on *key* column 'x'
kDT["a", on="x"]                      # same, being explicit using 'on=' (preferred)

# all together
kDT[!"a", sum(v), by=.EACHI]          # get sum(v) for each i != "a"

# multi-column key
setkey(kDT,x,y)                       # 2-column key
setkeyv(kDT,c("x","y"))               # same

# fast *keyed* subsets on multi-column key
kDT["a"]                              # join to 1st column of key
kDT["a", on="x"]                      # on= is optional, but is preferred
kDT[.("a")]                           # same, .() is an alias for list()
kDT[list("a")]                        # same
kDT[.("a", 3)]                        # join to 2 columns
kDT[.("a", 3:6)]                      # join 4 rows (2 missing)
kDT[.("a", 3:6), nomatch=NULL]        # remove missing
kDT[.("a", 3:6), roll=TRUE]           # locf rolling join
kDT[.("a", 3:6), roll=Inf]            # same as above
kDT[.("a", 3:6), roll=-Inf]           # nocb rolling join
kDT[!.("a")]                          # not join
kDT[!"a"]                             # same

# more on special symbols, see also ?"special-symbols"
DT[.N]                                  # last row
DT[, .N]                                # total number of rows in DT
DT[, .N, by=x]                          # number of rows in each group
DT[, .SD, .SDcols=x:y]                  # select columns 'x' through 'y'
DT[ , .SD, .SDcols = !x:y]              # drop columns 'x' through 'y'
DT[ , .SD, .SDcols = patterns('^[xv]')] # select columns matching '^x' or '^v'
DT[, .SD[1]]                            # first row of all columns
DT[, .SD[1], by=x]                      # first row of 'y' and 'v' for each group in 'x'
DT[, c(.N, lapply(.SD, sum)), by=x]     # get rows *and* sum columns 'v' and 'y' by group
DT[, .I[1], by=x]                       # row number in DT corresponding to each group
DT[, grp := .GRP, by=x]                 # add a group counter column
DT[ , dput(.BY), by=.(x,y)]             # .BY is a list of singletons for each group
X[, DT[.BY, y, on="x"], by=x]           # join within each group
DT[, {
  # write each group to a different file
  fwrite(.SD, file.path(tempdir(), paste0('x=', .BY$x, '.csv')))
}, by=x]
dir(tempdir())

# add/update/delete by reference (see ?assign)
print(DT[, z:=42L])                   # add new column by reference
print(DT[, z:=NULL])                  # remove column by reference
print(DT["a", v:=42L, on="x"])        # subassign to existing v column by reference
print(DT["b", v2:=84L, on="x"])       # subassign to new column by reference (NA padded)

DT[, m:=mean(v), by=x][]              # add new column by reference by group
                                      # NB: postfix [] is shortcut to print()
# advanced usage
DT = data.table(x=rep(c("b","a","c"),each=3), v=c(1,1,1,2,2,1,1,2,2), y=c(1,3,6), a=1:9, b=9:1)

DT[, sum(v), by=.(y\%\%2)]              # expressions in by
DT[, sum(v), by=.(bool = y\%\%2)]       # same, using a named list to change by column name
DT[, .SD[2], by=x]                    # get 2nd row of each group
DT[, tail(.SD,2), by=x]               # last 2 rows of each group
DT[, lapply(.SD, sum), by=x]          # sum of all (other) columns for each group
DT[, .SD[which.min(v)], by=x]         # nested query by group

DT[, list(MySum=sum(v),
          MyMin=min(v),
          MyMax=max(v)),
    by=.(x, y\%\%2)]                    # by 2 expressions

DT[, .(a = .(a), b = .(b)), by=x]     # list columns
DT[, .(seq = min(a):max(b)), by=x]    # j is not limited to just aggregations
DT[, sum(v), by=x][V1<20]             # compound query
DT[, sum(v), by=x][order(-V1)]        # ordering results
DT[, c(.N, lapply(.SD,sum)), by=x]    # get number of observations and sum per group
DT[, {tmp <- mean(y);
      .(a = a-tmp, b = b-tmp)
      }, by=x]                        # anonymous lambda in 'j', j accepts any valid
                                      # expression. TO REMEMBER: every element of
                                      # the list becomes a column in result.
pdf("new.pdf")
DT[, plot(a,b), by=x]                 # can also plot in 'j'
dev.off()
\dontshow{file.remove("new.pdf")}

# using rleid, get max(y) and min of all cols in .SDcols for each consecutive run of 'v'
DT[, c(.(y=max(y)), lapply(.SD, min)), by=rleid(v), .SDcols=v:b]

# Support guide and links:
# https://github.com/Rdatatable/data.table/wiki/Support

\dontrun{
if (interactive()) {
  vignette(package="data.table")  # 9 vignettes

  test.data.table()               # 6,000 tests

  # keep up to date with latest stable version on CRAN
  update.packages()

  # get the latest devel version
  update.dev.pkg()
  # read more at:
  # https://github.com/Rdatatable/data.table/wiki/Installation
}
}}
\keyword{ data }
