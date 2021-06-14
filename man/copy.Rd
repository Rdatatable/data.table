\name{copy}
\alias{copy}
\title{ Copy an entire object }
\description{
  In \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.

  \code{copy()} copies an entire object.
}
\usage{
copy(x)
}
\arguments{
  \item{x}{ A \code{data.table}. }
}
\details{
\code{data.table} provides functions that operate on objects \emph{by reference} and minimise full object copies as much as possible. Still, it might be necessary in some situations to work on an object's copy which can be done using \code{DT.copy <- copy(DT)}. It may also be sometimes useful before \code{:=} (or \code{set}) is used to subassign to a column by reference.

A \code{copy()} may be required when doing \code{dt_names = names(DT)}. Due to R's \emph{copy-on-modify}, \code{dt_names} still points to the same location in memory as \code{names(DT)}. Therefore modifying \code{DT} \emph{by reference} now, say by adding a new column, \code{dt_names} will also get updated. To avoid this, one has to \emph{explicitly} copy: \code{dt_names <- copy(names(DT))}.
}
\note{
  To confirm precisely whether an object is a copy of another, compare their exact memory address with \code{\link{address}}.
}
\value{
  Returns a copy of the object.
}
\seealso{
  \code{\link{data.table}}, \code{\link{address}}, \code{\link{setkey}}, \code{\link{setDT}}, \code{\link{setDF}}, \code{\link{set}} \code{\link{:=}}, \code{\link{setorder}}, \code{\link{setattr}}, \code{\link{setnames}}
}
\examples{
# Type 'example(copy)' to run these at prompt and browse output

DT = data.table(A=5:1,B=letters[5:1])
DT2 = copy(DT)        # explicit copy() needed to copy a data.table
setkey(DT2,B)         # now just changes DT2
identical(DT,DT2)     # FALSE. DT and DT2 are now different tables

DT = data.table(A=5:1, B=letters[5:1])
nm1 = names(DT)
nm2 = copy(names(DT))
DT[, C := 1L]
identical(nm1, names(DT)) # TRUE, nm1 is also changed by reference
identical(nm2, names(DT)) # FALSE, nm2 is a copy, different from names(DT)
}
\keyword{ data }

