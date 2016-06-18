\name{tstrsplit}
\alias{tstrsplit}
\alias{strsplit}
\title{strsplit and transpose the resulting list efficiently}
\description{
  This is equivalent to \code{transpose(strsplit(...))}. This is a convenient wrapper function to split a column using \code{strsplit} and assign the transposed result to individual columns. See examples.

}

\usage{
tstrsplit(x, ..., fill=NA, type.convert=FALSE, keep, names=FALSE)
}
\arguments{
	\item{x}{The vector to split (and transpose).}
  \item{...}{ All the arguments to be passed to \code{strsplit}. }
  \item{fill}{ Default is \code{NA}. It is used to fill shorter list elements so as to return each element of the transposed result of equal lengths. }
  \item{type.convert}{\code{TRUE} calls \code{\link{type.convert}} with \code{as.is=TRUE} on the columns.}
  \item{keep}{Specify indices corresponding to just those list elements to retain in the transposed result. Default is to return all.}
  \item{names}{\code{TRUE} auto names the list with \code{V1, V2} etc. Default (\code{FALSE}) is to return an unnamed list.}
}
\details{
  It internally calls \code{strsplit} first, and then \code{\link{transpose}} on the result. 

  \code{names} argument can be used to return an auto named list, although this argument does not have any effect when used with \code{:=}, which requires names to be provided explicitly. It might be useful in other scenarios.
}
\value{
  A transposed list after splitting by the pattern provided.
}

\examples{
x = c("abcde", "ghij", "klmnopq")
strsplit(x, "", fixed=TRUE)
tstrsplit(x, "", fixed=TRUE)
tstrsplit(x, "", fixed=TRUE, fill="<NA>")

# using keep to return just 1,3,5
tstrsplit(x, "", fixed=TRUE, keep=c(1,3,5))

# names argument
tstrsplit(x, "", fixed=TRUE, keep=c(1,3,5), names=LETTERS[1:3])

DT = data.table(x=c("A/B", "A", "B"), y=1:3)
DT[, c("c1") := tstrsplit(x, "/", fixed=TRUE, keep=1L)][]
DT[, c("c1", "c2") := tstrsplit(x, "/", fixed=TRUE)][]
}
\seealso{
  \code{\link{data.table}}, \code{\link{transpose}}
}
\keyword{ data }

