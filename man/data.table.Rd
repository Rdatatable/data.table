\name{data.table}
\alias{data.table}
\alias{DT}
\alias{as.data.table}
\alias{is.data.table}
\title{ Enhanced data.frame }
\description{
   Same internal structure as data.frame (i.e. list of vectors) but enhanced extract methods.
}
\usage{
data.table(..., keep.rownames=FALSE, check.names=TRUE, key=NULL)
DT(...)
}
\arguments{
  \item{\dots}{ Just as \dots in \code{\link{data.frame}}. These arguments are of either the form value
  or tag = value. Column names are created based on the tag (if present) or the deparsed argument itself.
  
  The data.table can be formed from one or more of (mixed type) vector, matrix, data.frame, data.table, etc. Usual
  recycling rules are applied to vectors of different lengths.
  
  \code{\link{make.names}} is used to resolve any column name conflicts in the union of the column names of these objects
  and the tag will be prefixed to the column name.
  }
  \item{keep.rownames}{ If \dots is a matrix or data.frame, TRUE will retain the rownames of that object into a column 'rn'.
  If the matrix or data.frame already has a column called 'rn' it will be renamed 'rn.1' via \code{\link{make.names}}. If more
  than one matrix or data.frame appear in \dots all rownames from all those objects will be kept in multiple rn.<n> columns. }
  \item{check.names}{ Just as check.names in \code{\link{data.frame}}(). For example this replaces spaces in column names with
  period and ensures column names are valid \R object names. }
  
  \item{key}{ Character vector of length 1 containing one or more column names separated by comma which is passed to \code{\link{setkey}} }
}
\details{
data.table creates a data.table from its arguments just as data.frame does. DT() is an alias for data.table() and is
often used instead of as.data.table().

A data.table is a list of vectors, just like a data.frame, however :
\enumerate{
\item it \emph{never} has rownames. Instead it may have an optional \dfn{key} of one or more columns using \code{\link{setkey}}. This key can be used for row indexing instead of rownames.
\item when the data.table has over 20 rows the print method displays column names at the bottom as well as at
the top to save scrolling up at the console.
\item character vectors may be passed in but they are automatically converted to factor. A data.table does not allow character columns for time and space reasons.
\item however \emph{the main difference} is enhanced functionality in \code{\link{[.data.table}} where most documentation for this package lives.
}
}
\value{
   A data.table.
}
% \references{  }
\author{ Matthew Dowle }
\note{ \code{keep.rownames} and \code{check.names}, if suppplied, must be written in full since they appear after
the \dots. \R does not allow partial argument names after \dots. For example \code{data.table(DF,keep=TRUE)} will create a
column called 'keep' containing TRUE and this is correct behaviour. Most likely \code{data.table(DF,keep.rownames=TRUE)} was intended.
}
\seealso{ \code{\link{data.frame}}, \code{\link{tables}}, \code{\link{setkey}}, \code{\link{[.data.table}} }
\examples{
DF = data.frame(a=1:5, b=letters[1:5])
DT = data.table(a=1:5, b=letters[1:5])
identical(as.data.table(DF), DT)
identical(DT(DF), DT)
identical(dim(DT),dim(DF))
identical(DF$a, DT$a)
DT
tables()
identical(DT(DT,DT), cbind(DT,DT))
DT2=rbind(DT,DT)
DT3 = data.table(A=DT, B=DT, key="A.b")
tables()
test.data.table()
\dontrun{
example("[.data.table")
}
}
\keyword{ data }
