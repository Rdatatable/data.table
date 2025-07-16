\name{setDT}
\alias{setDT}
\title{Coerce lists and data.frames to data.table by reference}
\description{
  In \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.

  \code{setDT} converts lists (both named and unnamed) and data.frames to data.tables \emph{by reference}. This feature was requested on \href{https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy}{Stackoverflow}.

}
\usage{
setDT(x, keep.rownames=FALSE, key=NULL, check.names=FALSE)
}
\arguments{
  \item{x}{ A named or unnamed \code{list}, \code{data.frame} or \code{data.table}. }
  \item{keep.rownames}{ For \code{data.frame}s, \code{TRUE} retains the \code{data.frame}'s row names under a new column \code{rn}. \code{keep.rownames = "id"} names the column \code{"id"} instead. } 
  \item{key}{ Character vector of one or more column names which is passed to \code{\link{setkeyv}}. }
  \item{check.names}{ Just as \code{check.names} in \code{\link{data.frame}}. }
}

\details{
  When working on large \code{list}s or \code{data.frame}s, it might be both time- and memory-consuming to convert them to a \code{data.table} using \code{as.data.table(.)}, which will make a complete copy of the input object before converting it to a \code{data.table}. \code{setDT} takes care of this issue by converting any \code{list} (named or unnamed, data.frame or not) \emph{by reference} instead. That is, the input object is modified in place with no copy.

  This should come with low overhead, but note that \code{setDT} does check that the input is valid by looking for inconsistent input lengths and inadmissible column types (e.g. matrix).
}

\value{
  The input is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setDT(X)[, sum(B), by=A]}. If you require a copy, take a copy first (using \code{DT2 = copy(DT)}). See \code{?copy}.
}

\seealso{
  \code{\link{data.table}}, \code{\link{as.data.table}}, \code{\link{setDF}}, \code{\link{copy}}, \code{\link{setkey}}, \code{\link{setcolorder}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setorder}},
  See the FAQ vignette: \code{vignette("datatable-faq", package = "data.table")}.
}
\examples{

set.seed(45L)
X = data.frame(
  A=sample(3, 10, TRUE),
  B=sample(letters[1:3], 10, TRUE),
  C=sample(10))

# Convert X to data.table by reference and
# get the frequency of each "A,B" combination
setDT(X)[, .N, by=.(A,B)]

# convert list to data.table
# autofill names
X = list(1:4, letters[1:4])
setDT(X)
# don't provide names
X = list(a=1:4, letters[1:4])
setDT(X, FALSE)

# setkey directly
X = list(a = 4:1, b=runif(4))
setDT(X, key="a")[]

# check.names argument
X = list(a=1:5, a=6:10)
setDT(X, check.names=TRUE)[]

# Example demonstrating setDT after loading from RDS
rds_file = tempfile(fileext = ".rds")
X = data.table(a = 1:5, b = letters[1:5])
saveRDS(X, rds_file)
X_loaded = readRDS(rds_file)
setDT(X_loaded)  # restore internal data.table attributes
print(X_loaded)
unlink(rds_file)
}
\keyword{ data }

