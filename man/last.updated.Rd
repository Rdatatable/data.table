\name{.Last.updated}
\alias{.Last.updated}
\alias{Last.updated}
\title{ Number of rows affected by last update }
\description{
    Returns number of rows affected by last \code{:=} or \code{set()}.
}
\usage{
    .Last.updated
}
\value{
    Integer.
}
\details{
  Be aware that in the case of duplicate indices, multiple updates occur (duplicates are overwritten);
  \code{.Last.updated} will include \emph{all} of the
  updates performed, including duplicated ones. See examples.
}
\seealso{
  \code{\link{:=}}
}
\examples{
d = data.table(a=1:4, b=2:5)
d[2:3, z:=5L]
.Last.updated

# updated count takes duplicates into account #2837
DT = data.table(a = 1L)
DT[c(1L, 1L), a := 2:3]
.Last.updated
}
\keyword{ data }
