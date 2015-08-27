\name{merge}
\alias{merge}
\alias{merge.data.table}
\title{ Merge Two Data Tables }
\description{
  Fast merge of two \code{data.table}s.

  This \code{merge} method for \code{data.table} behaves very similarly to that
  of \code{data.frame}s with one major exception: By default, 
  the columns used to merge the \code{data.table}s are the shared key columns
  rather than the shared columns with the same names. Set the \code{by}, or \code{by.x}, 
  \code{by.y} arguments explicitly to override this default.
  
}

\usage{
\method{merge}{data.table}(x, y, by = NULL, by.x = NULL, by.y = NULL,
all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"),
allow.cartesian=getOption("datatable.allow.cartesian"),  # default FALSE
...)
}

\arguments{
  \item{x, y}{
    \code{data table}s. \code{y} is coerced to a \code{data.table} if
    it isn't one already.
  }

  \item{by}{
    A vector of shared column names in \code{x} and \code{y} to merge on.
    This defaults to the shared key columns between the two tables.
    If \code{y} has no key columns, this defaults to the key of \code{x}.
  }
  
  \item{by.x, by.y}{
    Vectors of column names in \code{x} and \code{y} to merge on.
  }

  \item{all}{
    logical; \code{all = TRUE} is shorthand to save setting both \code{all.x = TRUE} and
    \code{all.y = TRUE}.
  }

  \item{all.x}{
    logical; if \code{TRUE}, then extra rows will be added to the
    output, one for each row in \code{x} that has no matching row in
    \code{y}.  These rows will have 'NA's in those columns that are
    usually filled with values from \code{y}.  The default is \code{FALSE},
    so that only rows with data from both \code{x} and \code{y} are
    included in the output.
  }

  \item{all.y}{
    logical; analogous to \code{all.x} above.
  }
  
  \item{sort}{
    logical. If \code{TRUE} (default), the merged \code{data.table} is sorted 
    by setting the key to the \code{by / by.x} columns. If \code{FALSE}, the 
    result is not sorted.
  }
  
  \item{suffixes}{
    A \code{character(2)} specifying the suffixes to be used for making
    non-\code{by} column names unique. The suffix behavior works in a similar 
    fashion as the \code{\link{merge.data.frame}} method does.
  }
  
  \item{allow.cartesian}{
    See \code{allow.cartesian} in \code{\link{[.data.table}}.
  }

  \item{\dots}{
    Not used at this time.
  }
}

\details{
  \code{\link{merge}} is a generic function in base R. It dispatches to either the
  \code{merge.data.frame} method or \code{merge.data.table} method depending on the class of its first argument.
  
  In versions \code{< v1.9.6}, if the specified columns in \code{by} was not the key (or head of the key) of \code{x} or \code{y}, then a \code{\link{copy}} is first rekeyed prior to performing the merge. This was less performant and memory inefficient. 

  In version \code{v1.9.4} secondary keys was implemented. In \code{v1.9.6}, the concept of secondary keys has been 
  extended to \code{merge}. No deep copies are made anymore and therefore very performant and memory efficient. Also there is better control for providing the columns to merge on with the help of newly implemented \code{by.x} and \code{by.y} arguments.

  For a more \code{data.table}-centric way of merging two \code{data.table}s, see \code{\link{[.data.table}}; e.g., \code{x[y, ...]}. See FAQ 1.12 for a detailed comparison of \code{merge} and \code{x[y, ...]}.

  Merges on numeric columns: Columns of numeric types (i.e., double) have their last two bytes rounded off while computing order, by defalult, to avoid any unexpected behaviour due to limitations in representing floating point numbers precisely. For large numbers (integers > 2^31), we recommend using \code{bit64::integer64}. Have a look at \code{\link{setNumericRounding}} to learn more.
  
}

\value{
  A new \code{data.table} based on the merged \code{data table}s, sorted by the
  columns set (or inferred for) the \code{by} argument.
}

\seealso{
  \code{\link{data.table}}, \code{\link{[.data.table}},
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

# new feature, no need to set keys anymore
set.seed(1L)
d1 <- data.table(a=sample(rep(1:3,each=2)), z=1:6)
d2 <- data.table(a=2:0, z=10:12)
merge(d1, d2, by="a")
merge(d1, d2, by="a", all=TRUE)

# new feature, using by.x and by.y arguments
setnames(d2, "a", "b")
merge(d1, d2, by.x="a", by.y="b")
merge(d1, d2, by.x="a", by.y="b", all=TRUE)
merge(d2, d1, by.x="b", by.y="a")
}

\keyword{ data }


