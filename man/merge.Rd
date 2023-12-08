\name{merge}
\alias{merge}
\alias{merge.data.table}
\title{Merge two data.tables}
\description{
Fast merge of two \code{data.table}s. The \code{data.table} method behaves
similarly to \code{data.frame} except that row order is specified, and by
default the columns to merge on are chosen:

\itemize{
  \item at first based on the shared key columns, and if there are none,
  \item then based on key columns of the first argument \code{x}, and if there
  are none,
  \item then based on the common columns between the two \code{data.table}s.
}

Use the \code{by}, \code{by.x} and \code{by.y} arguments explicitly to override this default.
}

\usage{
\method{merge}{data.table}(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE,
all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE,
allow.cartesian=getOption("datatable.allow.cartesian"),  # default FALSE
incomparables = NULL, \dots)
}

\arguments{
\item{x, y}{\code{data table}s. \code{y} is coerced to a \code{data.table} if
it isn't one already.}
\item{by}{A vector of shared column names in \code{x} and \code{y} to merge on.
This defaults to the shared key columns between the two tables.
If \code{y} has no key columns, this defaults to the key of \code{x}.}
\item{by.x, by.y}{Vectors of column names in \code{x} and \code{y} to merge on.}
\item{all}{logical; \code{all = TRUE} is shorthand to save setting both
\code{all.x = TRUE} and \code{all.y = TRUE}.}
\item{all.x}{logical; if \code{TRUE}, rows from \code{x} which have no matching row
in \code{y} are included. These rows will have 'NA's in the columns that are usually
filled with values from \code{y}. The default is \code{FALSE} so that only rows with
data from both \code{x} and \code{y} are included in the output.}
\item{all.y}{logical; analogous to \code{all.x} above.}
\item{sort}{logical. If \code{TRUE} (default), the rows of the merged
\code{data.table} are sorted by setting the key to the \code{by / by.x} columns. If
\code{FALSE}, unlike base R's \code{merge} for which row order is unspecified, the
row order in \code{x} is retained (including retaining the position of missings when
\code{all.x=TRUE}), followed by \code{y} rows that don't match \code{x} (when \code{all.y=TRUE})
retaining the order those appear in \code{y}.}
\item{suffixes}{A \code{character(2)} specifying the suffixes to be used for
making non-\code{by} column names unique. The suffix behaviour works in a similar
fashion as the \code{\link{merge.data.frame}} method does.}
\item{no.dups}{logical indicating that \code{suffixes} are also appended to
non-\code{by.y} column names in \code{y} when they have the same column name
as any \code{by.x}.}
\item{allow.cartesian}{See \code{allow.cartesian} in \code{\link{[.data.table}}.}
\item{incomparables}{values which cannot be matched and therefore are excluded from by columns.}
\item{\dots}{Not used at this time.}
}

\details{
\code{\link{merge}} is a generic function in base R. It dispatches to either the
\code{merge.data.frame} method or \code{merge.data.table} method depending on
the class of its first argument. Note that, unlike \code{SQL} join, \code{NA} is
matched against \code{NA} (and \code{NaN} against \code{NaN}) while merging.

For a more \code{data.table}-centric way of merging two \code{data.table}s, see
\code{\link{[.data.table}}; e.g., \code{x[y, \dots]}. See FAQ 1.11 for a detailed
comparison of \code{merge} and \code{x[y, \dots]}.
}

\value{
A new \code{data.table} based on the merged \code{data table}s, and sorted by the
columns set (or inferred for) the \code{by} argument if argument \code{sort} is
set to \code{TRUE}.
}

\seealso{
\code{\link{data.table}}, \code{\link{setkey}}, \code{\link{[.data.table}},
\code{\link{merge.data.frame}}
}

\examples{
(dt1 <- data.table(A = letters[1:10], X = 1:10, key = "A"))
(dt2 <- data.table(A = letters[5:14], Y = 1:10, key = "A"))
merge(dt1, dt2)
merge(dt1, dt2, all = TRUE)

(dt1 <- data.table(A = letters[rep(1:3, 2)], X = 1:6, key = "A"))
(dt2 <- data.table(A = letters[rep(2:4, 2)], Y = 6:1, key = "A"))
merge(dt1, dt2, allow.cartesian=TRUE)

(dt1 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(1:3, 2)], X = 1:6, key = "A,B"))
(dt2 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(2:4, 2)], Y = 6:1, key = "A,B"))
merge(dt1, dt2)
merge(dt1, dt2, by="B", allow.cartesian=TRUE)

# test it more:
d1 <- data.table(a=rep(1:2,each=3), b=1:6, key="a,b")
d2 <- data.table(a=0:1, bb=10:11, key="a")
d3 <- data.table(a=0:1, key="a")
d4 <- data.table(a=0:1, b=0:1, key="a,b")

merge(d1, d2)
merge(d2, d1)
merge(d1, d2, all=TRUE)
merge(d2, d1, all=TRUE)

merge(d3, d1)
merge(d1, d3)
merge(d1, d3, all=TRUE)
merge(d3, d1, all=TRUE)

merge(d1, d4)
merge(d1, d4, by="a", suffixes=c(".d1", ".d4"))
merge(d4, d1)
merge(d1, d4, all=TRUE)
merge(d4, d1, all=TRUE)

# setkey is automatic by default
set.seed(1L)
d1 <- data.table(a=sample(rep(1:3,each=2)), z=1:6)
d2 <- data.table(a=2:0, z=10:12)
merge(d1, d2, by="a")
merge(d1, d2, by="a", all=TRUE)

# using by.x and by.y
setnames(d2, "a", "b")
merge(d1, d2, by.x="a", by.y="b")
merge(d1, d2, by.x="a", by.y="b", all=TRUE)
merge(d2, d1, by.x="b", by.y="a")

# using incomparables values
d1 <- data.table(a=c(1,2,NA,NA,3,1), z=1:6)
d2 <- data.table(a=c(1,2,NA), z=10:12)
merge(d1, d2, by="a")
merge(d1, d2, by="a", incomparables=NA)
}

\keyword{ data }

