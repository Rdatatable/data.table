\name{merge}
\alias{merge}
\alias{merge.data.table}
\title{ Merge Two Data Tables }
\description{
  Relatively quick merge of two \code{data.table}s based on common key columns (by default).

  This \code{merge} method for \code{data.table} is meant to act very similarly to the
  \code{merge} method for \code{data.frame}, with the major exception being that
  the default columns used to merge two \code{data.table} inputs are the shared key columns
  rather than the shared columns with the same names.
  
  For a more \code{data.table}-centric (and faster) way of merging two \code{data.table}s,
  see \code{\link{[.data.table}}; e.g., \code{x[y, ...]}. In recent
  versions, however, \code{merge()} is much closer to the speed of \code{x[y, ...]}.
  See FAQ 1.12 for a detailed comparison of \code{merge} and \code{x[y, ...]}.
  
  Note that \code{\link{merge}} is a generic function in base R. It dispatches to either the
  \code{merge.data.frame} method or \code{merge.data.table} method depending on the class of its first argument.
  Typing \code{?merge} at the prompt should present a choice of two links:
  the help pages for each of these \code{merge} methods. You don't need to use the full name of the
  method although you may if you wish; i.e., \code{merge(DT1,DT2)} is idiomatic R but you can bypass
  method dispatch by going direct if you wish: \code{merge.data.table(DT1,DT2)}. 
}

\usage{
\method{merge}{data.table}(x, y, by = NULL, all = FALSE, all.x = all, all.y = all, suffixes = c(".x", ".y"), 
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
  Note that if the specified columns in \code{by} is not the key (or
  head of the key) of \code{x} or \code{y}, then a copy is first rekeyed 
  prior to performing the merge. This might make this function perform
  slower than you are expecting. When secondary keys are implemented in
  future we expect performance in this case to improve.
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

}

\keyword{ data }


