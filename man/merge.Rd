\name{merge}
\alias{merge.data.table}
\title{ Merge Two Data Tables }
\description{
     Relatively quick merge of data tables based on common keys.
}
\usage{
\method{merge}{data.table}(x, all = FALSE, all.x = all, all.y = all, ...)

}
\arguments{
\item{x, y}{ data tables. }

\item{all}{ logical; \code{all = L} is shorthand for \code{all.x = L} and \code{all.y =
      L}.}

\item{all.x}{logical; if \code{TRUE}, then extra rows will be added to the
          output, one for each row in \code{x} that has no matching row in
          \code{y}.  These rows will have 'NA's in those columns that are
          usually filled with values from \code{y}.  The default is \code{FALSE},
          so that only rows with data from both \code{x} and \code{y} are
          included in the output.}

\item{all.y}{logical; analogous to \code{all.x} above.}

\item{\dots}{ Not used at this time. }
}
\details{
  Keys must be in the same order.

}
\value{
  A data table based on the merged data tables, sorted by the common keys.
}
% \references{ }
\author{ Tom Short }
% \note{ }
\seealso{ \code{\link{data.table}}, \code{\link{merge}}}
\examples{
    (dt1 <- data.table(A = letters[1:10], X = 1:10, key = "A"))
    (dt2 <- data.table(A = letters[5:14], Y = 1:10, key = "A"))
    merge(dt1, dt2)
    merge(dt1, dt2, all = T)

    (dt1 <- data.table(A = letters[rep(1:3, 2)], X = 1:6, key = "A"))
    (dt2 <- data.table(A = letters[rep(2:4, 2)], Y = 6:1, key = "A"))
    merge(dt1, dt2)

    (dt1 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(1:3, 2)], X = 1:6, key = "A,B"))
    (dt2 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(2:4, 2)], Y = 6:1, key = "A,B"))
    merge(dt1, dt2)
}
\keyword{ data }
