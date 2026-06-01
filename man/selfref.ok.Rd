\name{.selfref.ok}
\alias{.selfref.ok}
\title{Tests self reference of a data.table}
\description{
  In rare situations, as presented in examples below, a data.table object may lose its internal attribute that holds a self-reference. This function tests just that.

  It is not expected that many end users will have need for this highly technical function about data.table internals.
}
\usage{
  .selfref.ok(x)
}
\arguments{
  \item{x}{ A data.table. }
}
\value{
  \code{TRUE} if self reference attribute is properly set, \code{FALSE} otherwise.
}
\examples{
d1 = structure(list(V1=1L), class=c("data.table","data.frame"))
.selfref.ok(d1)
setDT(d1)
.selfref.ok(d1)

saveRDS(d1, f<-tempfile())
d2 = readRDS(f)
.selfref.ok(d2)
invisible(file.remove(f))
setDT(d2)
.selfref.ok(d2)

d3 = unserialize(serialize(d2, NULL))
.selfref.ok(d3)
setDT(d3)
.selfref.ok(d3)
}
\keyword{ data }
