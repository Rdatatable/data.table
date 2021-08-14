\name{tstrsplit}
\alias{tstrsplit}
\alias{strsplit}
\title{strsplit and transpose the resulting list efficiently}
\description{
  This is equivalent to \code{transpose(strsplit(\dots))}. This is a convenient wrapper function to split a column using \code{strsplit} and assign the transposed result to individual columns. See examples.

}

\usage{
tstrsplit(x, \dots, fill=NA, type.convert=FALSE, keep, names=FALSE)
}
\arguments{
	\item{x}{The vector to split (and transpose).}
  \item{\dots}{ All the arguments to be passed to \code{\link[base]{strsplit}}. }
  \item{fill}{ Default is \code{NA}. It is used to fill shorter list elements so as to return each element of the transposed result of equal lengths. }
  \item{type.convert}{\code{TRUE} calls \code{\link{type.convert}} with \code{as.is=TRUE} on the columns. May also be a function, list of functions, or named list of functions to apply to each part; see examples. }
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

# type.convert argument
DT = data.table(
  w = c("Yes/F", "No/M"),
  x = c("Yes 2000-03-01 A/T", "No 2000-04-01 E/R"),
  y = c("1/1/2", "2/5/2.5"),
  z = c("Yes/1/2", "No/5/3.5"),
  v = c("Yes 10 30.5 2000-03-01 A/T", "No 20 10.2 2000-04-01 E/R"))

# convert each element in the transpose list to type factor
DT[, tstrsplit(w, "/", type.convert=as.factor)]

# convert part and leave any others
DT[, tstrsplit(z, "/", type.convert=list(as.numeric=2:3))]

# convert part with one function and any others with another
DT[, tstrsplit(z, "/", type.convert=list(as.factor=1L, as.numeric))]

# convert the remaining using 'type.convert(x, as.is=TRUE)' (i.e. what type.convert=TRUE does)
DT[, tstrsplit(v, " ", type.convert=list(as.IDate=4L, function(x) type.convert(x, as.is=TRUE)))]
}
\seealso{
  \code{\link{data.table}}, \code{\link{transpose}}, \code{\link[utils]{type.convert}}
}
\keyword{ data }

