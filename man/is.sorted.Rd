\name{is.sorted}
\alias{is.sorted}
\title{ Checks if input is sorted }
\description{
  Checks if input is sorted.
}
\usage{
  is.sorted(x, by=seq_along(x), retOrd=FALSE)
}
\arguments{
  \item{x}{ data.table type object or atomic vector. }
  \item{by}{ data.table columns used to check if \code{x} is sorted by those columns. }
  \item{retOrd}{ logical, when \code{TRUE} it will set an attribute \code{"order"} on the returned value, providing an order of \code{x}. Works only for data.table type \code{x}, not for atomic vector. }
}
\details{
  Checks if the input is object is sorted. Can check also by a subset of columns provided in \code{by} argument. Can also return an order used in computation when using \code{retOrd} argument.
}
\note{
  Checking sortedness is an expensive computation, and most commonly the intermediate computation, the order, could be re-used.
  For example the following check

\preformatted{
  if (!is.sorted(DT, by="Sepal.Length"))
    DT = DT[order(Sepal.Length)]
}

  could be written as

\preformatted{
  if (!s <- is.sorted(DT, by="Sepal.Length", retOrd=TRUE))
    DT = DT[attr(s, "order")]
}

so the order is computated only once. Of course for performance it is even better to sort in-place using \code{\link{setkey}}.
}
\value{
  Logical scalar, TRUE or FALSE, or if \code{NULL} provided, then logical \code{NA}. When \code{retOrd} set to TRUE, the resulting logical scalar will have an attribute \code{"order"}. The attribute will be integer vector the same length as nrow of \code{x}, or length 0 integer in case if \code{x} was sorted. Any missing values are being ordered to front, unlike \code{\link[base]{order}}. Note that logical scalar having attribute attached will fail test for \code{identical}, although it will work fine with \code{isTRUE} and \code{isFALSE}.
}
\seealso{ \code{\link{data.table}} }
\examples{
x = as.data.table(iris)
is.sorted(x, by="Species")

ans = is.sorted(x, by="Sepal.Length", retOrd=TRUE)
identical(ans, FALSE)
isFALSE(ans)
o = attr(ans, "order")
x[o]
}
\keyword{ data }