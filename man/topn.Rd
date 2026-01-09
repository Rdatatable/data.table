\name{topn}
\alias{topn}
\alias{which.max}
\alias{which.min}
\title{ Top n values index }
\description{
  \code{topn} returns the indices of the smallest (resp. largest) \code{n} values of x. This is an extension \code{\link{which.min}} (\code{\link{which.max}}) which only return the index of the minimum (resp. maximum).

  Especially, for large vectors this method will be faster and less memory intensive than \code{order} since no full sort is performed.

  \code{bit64::integer64} type is also supported.
}

\usage{
topn(x, n, na.last=TRUE, decreasing=FALSE, sorted=FALSE)
}
\arguments{
  \item{x}{ A numeric, complex, character or logical vector. }
  \item{n}{ A numeric vector length 1. How many indices to select. }
  \item{na.last}{ Control treatment of \code{NA}s. If \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they are put first. }
  \item{decreasing}{ Logical. Default is \code{FALSE}. Indicating whether the order should be increasing or decreasing. }
  \item{sorted}{ Logical. Default is \code{FALSE}. Indicating whether order should be sorted with respect to decreasing. }
}

\value{
  An integer vector giving the indicies of the \code{n} smallest (largest) for \code{decreasing=FALSE (TRUE)} elements of \code{x}.
}

\examples{
x = c(1:4, 0:5, 11)
# indices of smallest 3 values
topn(x, 3)
# indices of largest 3 values
topn(x, 3, decreasing = TRUE)

## NA's can be put to front or back
x = c(NA, 1:4)
topn(x, 5)
topn(x, 5, na.last=FALSE)

}
\seealso{
  \code{\link{data.table}}, \code{\link{order}}, \code{\link{which.max}}, \code{\link{which.min}}
}
\keyword{ data }
