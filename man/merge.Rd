\name{merge}
\alias{merge}
\alias{merge.data.table}
\title{ Merge Two Data Tables }
\description{
  Relatively quick merge of data tables based on common keys (by default).

  This function is meant to act very similarly to the
  \code{\link{merge.data.frame}} function, with the major exception being that
  the default columns used to merge two data.tables are the shared key columns,
  and not the shared columns with the same names.
  
  For a more \code{data.table}-centric (and faster) way of merging two data.tables,
  take a look at \code{\link{[.data.table}}; e.g., \code{x[y, ...]}.
}

\usage{
\method{merge}{data.table}(x, y, by = NULL, all = FALSE, all.x = all, all.y = all, suffixes = c(".x", ".y"), ...)
}

\arguments{
  \item{x, y}{
    data tables. \code{y} is coerced to a \code{data.table} if
    it isn't one already
  }

  \item{by}{
    A vector of shared column names in \code{x} and \code{y} to merge on.
    This defaults to the shared key columns between the two tables.
    If \code{y} has no key columns, this defaults to the keys set for \code{x}.
    Note that if the specified values in \code{by} are not the keys (or
    prefixes of keys) for \code{x,y}, then they are first set as the keys
    prior to performing the merge -- this might make this function perform
    slower than you are expecting.
  }

  \item{all}{
    logical; \code{all = L} is shorthand for \code{all.x = L} and
    \code{all.y = L}.
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
    fashion as \code{\link{merge.data.frame}} does.
  }

  \item{\dots}{
    Not used at this time.
  }
}

\details{
  Keys for each data.table are reshuffled to ensure that the columns
  identified in the \code{by} parameter are prefixes of the keys set for
  data.tables \code{x} and \code{y} -- this may cause the function to run
  slower than expected.
}

\value{
  A new \code{data.table} based on the merged data tables, sorted by the
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
    merge(dt1, dt2)

    (dt1 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(1:3, 2)], X = 1:6, key = "A,B"))
    (dt2 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(2:4, 2)], Y = 6:1, key = "A,B"))
    merge(dt1, dt2)
    merge(dt1, dt2, by="B")

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
