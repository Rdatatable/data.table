\name{lagby}
\alias{lagby}
\alias{lagbyv}
\title{Carry last observations in a group to the next group}
\description{
	A convenience function to carry last observations from a group to the next group.
}
\usage{
lagby(..., by, fill)
lagbyv(x, by, fill)
}
\arguments{
	\item{...}{A sequence of integer, double, integer64, character or logical vectors, all of the same length.}
	\item{by}{An integer vector that identifies the grouping of variables. It must be the same length as elements in x or ...}
	\item{fill}{A list of values to be used for padding the first group}
	\item{x}{A vector, list, data.frame or data.table}
}
\details{
	Some operations might be slow for large number of small groups.
	It might be faster to perform the operations on the whole value vector regardless of grouping first.
	Then, adjust the values within each group via vectorization as going through the groups to extract the last/first value can be slow.

	\code{lagby} carries the last value of each column of each group to the next group.
	The value is repeated in the whole of the next group.

	Argument \code{by} expects an integer vector to speed up calculations.
	\code{rleid} can be used to create this integer grouping when multiple grouping columns are involved.
}
\value{
	A vector when a vector is passed in or a list of vectors when a list, data.frame or data.table is passed in
}
\examples{
DT <- data.table(g1=c(1,1,2,2), g2=c(0,1,2,2), v=11:14, d=as.double(1001:1004))
DT[, lagby(v, by=g1, fill=1L)]

#with 2 grouping columns
DT[, lagby(v, by=rleid(g1, g2))]

#using order to decide ordering of groups
DT[order( g1,  g2), lagby(v, by=rleid(g1, g2), fill=1L)]
DT[order( g1, -g2), lagby(v, by=rleid(g1, g2), fill=1L)]
DT[order(-g1,  g2), lagby(v, by=rleid(g1, g2), fill=1L)]
DT[order(-g1, -g2), lagby(v, by=rleid(g1, g2), fill=1L)]

#usage of fill
DT[, lagby(v, d, by=g1, fill=.(5, 4))]
lagbyv(list(DT$v, DT$d), by=DT$g1, fill=list(5, 4))

}
\seealso{
	\code{\link{data.table}}
	\code{\link{rleid}}
	\code{\link{rowid}}
}
\keyword{data}