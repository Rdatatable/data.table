\name{setDF}
\alias{setDF}
\title{Coerce a data.table to data.frame by reference}
\description{
  In \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column.. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.

  A helper function to convert a \code{data.table} or \code{list} of equal length to \code{data.frame} by reference.
}
\usage{
setDF(x, rownames=NULL)
}
\arguments{
  \item{x}{ A \code{data.table}, \code{data.frame} or \code{list} of equal length. }
  \item{rownames}{ A \code{character} vector to assign as the row names of \code{x}. }
}

\details{
  This feature request came up on the \href{http://r.789695.n4.nabble.com/Is-there-any-overhead-to-converting-back-and-forth-from-a-data-table-to-a-data-frame-td4688332.html}{data.table mailing list}. All \code{data.table} attributes including any keys of the input data.table are stripped off.
  
  When using \code{rownames}, recall that the row names of a \code{data.frame} must be unique. By default, the assigned set of row names is simply the sequence 1, ..., \code{nrow(x)} (or \code{length(x)} for \code{list}s).
}

\value{
    The input \code{data.table} is modified by reference to a \code{data.frame} and returned (invisibly). If you require a copy, take a copy first (using \code{DT2 = copy(DT)}). See \code{?copy}..
}

\seealso{ \code{\link{data.table}}, \code{\link{as.data.table}}, \code{\link{setDT}}, \code{\link{copy}}, \code{\link{setkey}}, \code{\link{setcolorder}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setorder}}
}
\examples{
X = data.table(x=1:5, y=6:10)
## convert 'X' to data.frame, without any copy.
setDF(X)

X = data.table(x=1:5, y=6:10)
## idem, assigning row names
setDF(X, rownames = LETTERS[1:5])

X = list(x=1:5, y=6:10)
# X is converted to a data.frame without any copy.
setDF(X)
}
\keyword{ data }