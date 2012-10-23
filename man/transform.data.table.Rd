\name{transform.data.table}
\alias{transform}
\alias{transform.data.table}
\alias{within}
\alias{within.data.table}
\title{ Data table utilities }
\description{
  Utilities for \code{data.table} transformation.
  
  \strong{\code{transform} by group is particularly slow. Please use \code{:=} by group instead.}
  
  \code{within}, \code{transform} and other similar functions in \code{data.table} are not just provided for users who expect them to work, but for non-data.table-aware packages to retain keys, for example. Hopefully the (much) faster and more convenient \code{data.table} syntax will be used in time. See examples.
}
\usage{
\method{transform}{data.table}(`_data`, \ldots)
\method{within}{data.table}(data, expr, \ldots)
}
\arguments{
  \item{data, _data}{ data.table to be transformed.}

  \item{\ldots}{ for \code{transform}, Further arguments of the form
    \code{tag=value}. Ignored for \code{within}.}

  \item{expr}{ expression to be evaluated within the data.table.}
}
\details{
\code{within} is like \code{with}, but modifications (columns changed,
added, or removed) are updated in the returned data.table.

Note that \code{transform} will keep the key of the
\code{data.table} provided the \emph{targets} of the transform (i.e. the
columns that appear in \ldots) are not in the key of the data.table. 
\code{within} also retains the key provided the key columns are not \emph{touched}.
}
\value{
   The modified value of a copy of \code{data}.
}
\seealso{ \code{\link[base]{transform}}, \code{\link[base]{within}} and \code{\link{:=}} }
\examples{
DT <- data.table(a=rep(1:3, each=2), b=1:6)

DT2 <- transform(DT, c = a^2)
DT[, c:=a^2]
identical(DT,DT2)

DT2 <- within(DT, {
  b <- rev(b)
  c <- a*2
  rm(a)
})
DT[,`:=`(b = rev(b),
         c = a*2,
         a = NULL)]
identical(DT,DT2)

DT$d = ave(DT$b, DT$c, FUN=max)               # copies entire DT, even if it is 10GB in RAM
DT = DT[, transform(.SD, d=max(b)), by="c"]   # same, but even worse as .SD is copied for each group
DT[, d:=max(b), by="c"]                       # same result, but much faster, shorter and scales

# Multiple update by group. Convenient, fast, scales and easy to read.
DT[, `:=`(minb = min(b),
          meanb = mean(b),
          bplusd = sum(b+d)),  by=c\%/\%5]
DT

}
\keyword{ data }


