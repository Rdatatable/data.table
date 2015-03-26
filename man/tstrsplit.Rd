\name{tstrsplit}
\alias{tstrsplit}
\alias{strsplit}
\title{strsplit and transpose the resulting list efficiently}
\description{
  This is equivalent to \code{transpose(strsplit(...))}. This is a convenient wrapper function to split a column using \code{strsplit} and assign the transposed result to individual columns. See examples.

}

\usage{
tstrsplit(x, ..., fill=NA, type.convert=FALSE)
}
\arguments{
	\item{x}{The vector to split (and transpose).}
  \item{...}{ All the arguments to be passed to \code{strsplit}. }
  \item{fill}{ Default is \code{NA}. It is used to fill shorter list elements so as to return each element of the transposed result of equal lengths. }
  \item{type.convert}{\code{TRUE} calls \code{\link{type.convert}} with \code{as.is=TRUE} on the columns.}
}
\details{
  It internally calls \code{strsplit} first, and then \code{\link{transpose}} on the result.
}
\value{
  A transposed list.
}

\examples{
x = c("abcde", "ghij", "klmnopq")
strsplit(x, "", fixed=TRUE)
tstrsplit(x, "", fixed=TRUE)
tstrsplit(x, "", fixed=TRUE, fill="<NA>")

DT = data.table(x=c("A/B", "A", "B"), y=1:3)
DT[, c("c1", "c2") := tstrsplit(x, "/", fixed=TRUE)][]
}
\seealso{
  \code{\link{data.table}}, \code{\link{transpose}}
}
\keyword{ data }

